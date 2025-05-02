use std::{fs::File, io::BufReader, path::PathBuf};

use checked_design::CheckedDesign;
use clap::Parser;
use logical_design::LogicalDesign;
use lua::{get_lua, LogicalDesignAPI, PhysicalDesignAPI};
use mapped_design::MappedDesign;
use mlua::AnyUserData;
use phy::PhysicalDesign;
use serializable_design::SerializableDesign;

pub mod checked_design;
mod connected_design;
mod logical_design;
mod mapped_design;
mod phy;
mod serializable_design;
pub mod signal_lookup_table;

pub mod lua;
pub mod sim;

mod ndarr;
mod svg;

mod util;

#[cfg(test)]
mod tests;

#[derive(Debug)]
pub enum Error {
	SerializationError(serde_json::Error),
	LuaError(mlua::Error),
	IOError(std::io::Error),
	ReadlineError(rustyline::error::ReadlineError),
	LuaErrorNoReturnedDesign,
	ResultantFileNameIsBad,
}

impl From<serde_json::Error> for Error {
	fn from(value: serde_json::Error) -> Self {
		Self::SerializationError(value)
	}
}

impl From<mlua::Error> for Error {
	fn from(value: mlua::Error) -> Self {
		Self::LuaError(value)
	}
}

impl From<std::io::Error> for Error {
	fn from(value: std::io::Error) -> Self {
		Self::IOError(value)
	}
}

type Result<T> = std::result::Result<T, Error>;

/// Verilog to Factorio combinator compiler (v2f)
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
	/// Input technology-mapped file, .json, or lua script, .lua.
	#[arg(short, long)]
	input_file: PathBuf,
}

fn main() {
	let args = Args::parse();
	match if args.input_file.ends_with(".lua") {
		lua_flow(args)
	} else {
		mapped_flow(args)
	} {
		Ok(json) => println!("Blueprint:\n{}\n", json),
		Err(e) => println!("Couldn't complete compilation due to error: {:?}", e),
	};
}

fn mapped_flow(args: Args) -> Result<String> {
	let file = File::open(args.input_file)?;
	let reader = BufReader::new(file);
	let mapped_design: MappedDesign = serde_json::from_reader(reader)?;
	let mut checked_design = CheckedDesign::new();
	let mut logical_design = LogicalDesign::new();
	let mut physical_design = PhysicalDesign::new();
	let mut serializable_design = SerializableDesign::new();
	checked_design.build_from(&mapped_design);
	logical_design.build_from(&checked_design, &mapped_design);
	physical_design.build_from(&logical_design);
	serializable_design.build_from(&physical_design, &logical_design);
	let blueprint_json = serde_json::to_string(&serializable_design)?;
	Ok(blueprint_json)
}

fn lua_flow(args: Args) -> Result<String> {
	let lua = get_lua()?;
	let chunk = lua.load(args.input_file.clone());
	let eval = chunk.eval::<AnyUserData>()?;
	if let Ok(logd) = eval.borrow::<LogicalDesignAPI>() {
		let mut serd = SerializableDesign::new();
		let mut phyd = PhysicalDesign::new();
		phyd.build_from(&logd.logd.borrow());
		if logd.make_svg {
			let filename = args.input_file.file_stem().map(|stem| {
				let mut stem = stem.to_owned();
				stem.push(".svg");
				stem
			});
			if filename.is_none() {
				return Err(Error::ResultantFileNameIsBad);
			}
			let folder = args.input_file.parent();
			if folder.is_none() || !folder.unwrap().is_dir() {
				return Err(Error::ResultantFileNameIsBad);
			}
			let mut file = PathBuf::from(folder.unwrap());
			file.push(PathBuf::from(filename.unwrap()));
			phyd.save_svg(&logd.logd.borrow(), file);
		}
		serd.build_from(&phyd, &logd.logd.borrow());
		let blueprint_json = serde_json::to_string(&serd)?;
		return Ok(blueprint_json);
	} else if let Ok(phyd) = eval.borrow::<PhysicalDesignAPI>() {
		let mut serd = SerializableDesign::new();
		serd.build_from(&phyd.phyd.borrow(), &phyd.logd.borrow());
		let blueprint_json = serde_json::to_string(&serd)?;
		return Ok(blueprint_json);
	} else {
		return Err(Error::LuaErrorNoReturnedDesign);
	}
}

#[cfg(test)]
mod test {
	use super::*;
	#[allow(unused)]
	use logical_design::ArithmeticOperator as Aop;
	use logical_design::DeciderOperator as Dop;
	use logical_design::Signal as Sig;

	#[test]
	fn make_single_combinator() {
		let mut p = PhysicalDesign::new();
		let mut s = SerializableDesign::new();
		let mut l = LogicalDesign::new();
		l.add_lamp((Sig::None, Dop::Equal, Sig::Constant(0)));
		p.build_from(&l);
		s.build_from(&p, &l);
		let json = serde_json::to_string(&s).unwrap();
		println!("{}", json.as_str());
	}

	#[test]
	fn make_simple_design() {
		let mut p = PhysicalDesign::new();
		let mut s = SerializableDesign::new();
		let l = tests::logical_design_tests::get_simple_logical_design();
		p.build_from(&l);
		s.build_from(&p, &l);
		let json = serde_json::to_string(&s).unwrap();
		println!("{}", json.as_str());
	}

	#[test]
	fn make_complex_40_design() {
		let mut p = PhysicalDesign::new();
		let mut s = SerializableDesign::new();
		let l = tests::logical_design_tests::get_complex_40_logical_design();
		p.build_from(&l);
		s.build_from(&p, &l);
		let json = serde_json::to_string(&s).unwrap();
		println!("{}", json.as_str());
	}

	#[test]
	fn example_basics() {
		let args = Args {
			input_file: PathBuf::from("examples/lua/basics.lua"),
		};
		match lua_flow(args) {
			Ok(_) => {}
			Err(e) => {
				match e {
					Error::LuaError(error) => println!("{:?}", error),
					_ => println!("{:?}", e),
				};
				assert!(false);
			}
		}
		assert!(PathBuf::from("examples/lua/basics.svg").is_file());
	}
}

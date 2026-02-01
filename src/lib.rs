use std::{
	fs::{self, File},
	io::BufReader,
	ops::Deref,
	path::{Path, PathBuf},
};

use checked_design::CheckedDesign;
use clap::Parser;
use logical_design::LogicalDesign;
use lua::{get_lua, LogicalDesignAPI, PhysicalDesignAPI};
use mapped_design::MappedDesign;
use mlua::Value;
use phy::PhysicalDesign;
use serializable_design::SerializableDesign;

use crate::lua::PhysicalEnsembleAPI;

pub mod checked_design;
mod connected_design;
pub mod logical_design;
mod mapped_design;
mod phy;
mod serializable_design;
pub mod signal_lookup_table;
mod timing_engine;

pub mod lua;
pub mod sim;

mod ndarr;
mod svg;

pub mod util;

#[cfg(test)]
mod tests;

#[derive(Debug)]
pub enum Error {
	SerializationError(serde_json::Error),
	LuaError(mlua::Error),
	IOError(std::io::Error),
	NoSuchFile,
	ReadlineError(rustyline::error::ReadlineError),
	LuaErrorNoReturnedDesign,
	ResultantFileNameIsBad(PathBuf),
	V2FRootNotDefined(String),
	YosysExeNotFound,
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

pub type Result<T> = std::result::Result<T, Error>;

/// Verilog to Factorio combinator compiler (v2f)
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
pub struct Args {
	/// Input file if in normal flow.
	///   .json -> technology-mapped
	///   .lua -> script
	/// Sub-tools use this flag for input files.
	#[arg(short, long, required_unless_present = "dump_phy_cfg")]
	pub input_file: Option<PathBuf>,

	/// Output
	#[arg(short, long)]
	pub output_file: Option<PathBuf>,

	/// Dump a template cfg/phy_template.toml. Rename to cfg/phy.toml to use for real.
	#[arg(short, long)]
	pub dump_phy_cfg: bool,

	/// Sub-tool to convert any file into a 32 bit $readmemh compatible format.
	#[arg(short, long)]
	pub convert_to_memh: bool,
}

pub fn mapped_flow(args: Args) -> Result<String> {
	let input_file = args.input_file.unwrap();
	let file = File::open(input_file)?;
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

pub fn lua_flow(args: Args) -> Result<String> {
	let input_file = args.input_file.unwrap();
	let lua = get_lua()?;
	let chunk = lua.load(input_file.clone());
	let eval = chunk.eval::<Value>();
	let eval = match eval {
		Ok(v) => v,
		Err(err) => {
			println!("{err:#>}");
			return Ok("".to_string());
		},
	};
	let eval = match eval {
		Value::UserData(d) => d,
		Value::Error(e) => return Result::Err(Error::LuaError(*e)),
		_ => {
			return Ok(format!("Script returned: \n{eval:#?}"));
		},
	};
	let blueprint_json = if let Ok(logd) = eval.borrow::<LogicalDesignAPI>() {
		let mut serd = SerializableDesign::new();
		let mut phyd = PhysicalDesign::new();
		phyd.set_group_io(logd.group_io);
		phyd.build_from(&logd.logd.read().unwrap());
		if logd.make_svg {
			let svg_name = get_derivative_file_name(input_file.clone(), ".svg")?;
			phyd.save_svg(&logd.logd.read().unwrap(), svg_name)?;
		}
		serd.build_from(&phyd, &logd.logd.read().unwrap());
		let blueprint_json = serde_json::to_string(&serd)?;
		Ok(blueprint_json)
	} else if let Ok(phyd) = eval.borrow::<PhysicalDesignAPI>() {
		let mut serd = SerializableDesign::new();
		let logd = phyd.logd.read().unwrap();
		let phyd = phyd.phyd.read().unwrap();
		serd.build_from(phyd.deref(), logd.deref());
		let blueprint_json = serde_json::to_string(&serd)?;

		Ok(blueprint_json)
	} else if let Ok(design) = eval.borrow::<PhysicalEnsembleAPI>() {
		let mut serd = SerializableDesign::new();
		let logd = design.root.logd.read().unwrap();
		let phyd = design.root.phyd.read().unwrap();
		serd.build_from(phyd.deref(), logd.deref());
		let blueprint_json = serde_json::to_string(&serd)?;

		Ok(blueprint_json)
	} else {
		Err(Error::LuaErrorNoReturnedDesign)
	}?;
	match args.output_file {
		Some(file) => fs::write(file.clone(), blueprint_json)
			.map(|_| format!("Blueprint written to {file:?}"))
			.map_err(|e| Error::IOError(e)),
		None => Ok(blueprint_json),
	}
}

pub fn get_derivative_file_name<P, S>(filename: P, derivative: S) -> Result<PathBuf>
where
	P: AsRef<Path>,
	S: AsRef<str>,
{
	let filename: &Path = filename.as_ref();
	let mut newfilename = if let Some(s) = filename.file_stem() {
		s.to_owned()
	} else {
		return Err(Error::ResultantFileNameIsBad(filename.into()));
	};
	/*let parent = if let Some(s) = filename.parent() {
		s.to_owned()
	} else {
		return Err(Error::ResultantFileNameIsBad(filename.into()));
	};*/
	newfilename.push(derivative.as_ref());
	Ok(newfilename.into())
}

pub fn get_v2f_root() -> Result<PathBuf> {
	match std::env::var("V2F_ROOT") {
		Ok(v) => {
			let ret = PathBuf::from(v);
			if ret.is_dir() {
				Ok(ret)
			} else {
				Err(Error::V2FRootNotDefined("Not a directory".to_string()))
			}
		},
		Err(_) => Err(Error::V2FRootNotDefined("Env var not defined".to_string())),
	}
}

pub fn get_yosys_exe() -> Result<PathBuf> {
	let v2f_root = get_v2f_root()?;
	if let Ok(v) = std::env::var("YOSYS_EXE") {
		if PathBuf::from(&v).is_file() {
			return Ok(PathBuf::from(v));
		} else {
			return Err(Error::YosysExeNotFound); // They defined an env var, but it was bad
		}
	}
	if v2f_root.join("yosys/yosys").is_file() {
		return Ok(v2f_root.join("yosys/yosys"));
	}
	let res = std::process::Command::new("yosys")
		.arg("--version")
		.output();
	if res.is_ok() {
		return Ok(PathBuf::from("yosys"));
	}
	Err(Error::YosysExeNotFound)
}

pub fn dump_phy_cfg() {
	phy::dump_phy_cfg();
}

#[cfg(test)]
mod main_tests {

	use std::env;

	use super::*;
	use itertools::Itertools;
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
}

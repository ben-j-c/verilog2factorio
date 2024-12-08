use std::{fs::File, io::BufReader, path::PathBuf};

use clap::Parser;
use logical_design::LogicalDesign;
use mapped_design::MappedDesign;
use physical_design::PhysicalDesign;
use serializable_design::SerializableDesign;

mod logical_design;
mod mapped_design;
mod physical_design;
mod serializable_design;
mod signal_lookup_table;

/// Verilog to Factorio combinator compiler (v2f)
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
	/// Input technology-mapped file
	#[arg(short, long)]
	input_file: PathBuf,
}

fn main() {
	let args = Args::parse();
	let file = File::open(args.input_file).unwrap();
	let reader = BufReader::new(file);
	let mapped_design: MappedDesign = serde_json::from_reader(reader).unwrap();
	let mut logical_design = LogicalDesign::new();
	logical_design.build_from(&mapped_design);
	let mut physical_design = PhysicalDesign::new();
	physical_design.build_from(&logical_design);
	let mut serializable_design = SerializableDesign::new();
	serializable_design.build_from(&physical_design, &logical_design);
	let blueprint_json = serde_json::to_string(&serializable_design).unwrap();
	println!("{}", blueprint_json);
}

#[cfg(test)]
mod tests {
	use super::*;
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
		let l = logical_design::get_simple_logical_design();
		p.build_from(&l);
		s.build_from(&p, &l);
		let json = serde_json::to_string(&s).unwrap();
		println!("{}", json.as_str());
	}

	#[test]
	fn make_complex_40_design() {
		let mut p = PhysicalDesign::new();
		let mut s = SerializableDesign::new();
		let l = logical_design::get_complex_40_logical_design();
		p.build_from(&l);
		s.build_from(&p, &l);
		let json = serde_json::to_string(&s).unwrap();
		println!("{}", json.as_str());
	}
}

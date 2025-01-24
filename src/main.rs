use std::{fs::File, io::BufReader, path::PathBuf};

use checked_design::CheckedDesign;
use clap::Parser;
use logical_design::LogicalDesign;
use mapped_design::MappedDesign;
use physical_design::{PhysicalDesign, PlacementStrategy};
use serializable_design::SerializableDesign;

pub mod checked_design;
mod connected_design;
pub mod logical_design;
mod mapped_design;
mod physical_design;
mod serializable_design;
pub mod signal_lookup_table;

/// Verilog to Factorio combinator compiler (v2f)
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
	/// Input technology-mapped file
	#[arg(short, long)]
	input_file: PathBuf,
	/// Physical planning placement strategy
	#[arg(short, long)]
	placement_strategy: PlacementStrategy,
}

fn main() {
	let args = Args::parse();
	let file = File::open(args.input_file).unwrap();
	let reader = BufReader::new(file);
	let mapped_design: MappedDesign = serde_json::from_reader(reader).unwrap();
	let mut checked_design = CheckedDesign::new();
	let mut logical_design = LogicalDesign::new();
	let mut physical_design = PhysicalDesign::new();
	let mut serializable_design = SerializableDesign::new();
	checked_design.build_from(&mapped_design);
	logical_design.build_from(&checked_design, &mapped_design);
	physical_design.build_from(&logical_design, args.placement_strategy);
	serializable_design.build_from(&physical_design, &logical_design);
	let blueprint_json = serde_json::to_string(&serializable_design).unwrap();
	println!("{}", blueprint_json);
}

#[cfg(test)]
mod tests {
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
		p.build_from(&l, PlacementStrategy::default());
		s.build_from(&p, &l);
		let json = serde_json::to_string(&s).unwrap();
		println!("{}", json.as_str());
	}

	#[test]
	fn make_simple_design() {
		let mut p = PhysicalDesign::new();
		let mut s = SerializableDesign::new();
		let l = logical_design::get_simple_logical_design();
		p.build_from(&l, PlacementStrategy::default());
		s.build_from(&p, &l);
		let json = serde_json::to_string(&s).unwrap();
		println!("{}", json.as_str());
	}

	#[test]
	fn make_complex_40_design() {
		let mut p = PhysicalDesign::new();
		let mut s = SerializableDesign::new();
		let l = logical_design::get_complex_40_logical_design();
		p.build_from(&l, PlacementStrategy::default());
		s.build_from(&p, &l);
		let json = serde_json::to_string(&s).unwrap();
		println!("{}", json.as_str());
	}
}

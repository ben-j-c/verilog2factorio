use std::{fs::File, io::BufReader, path::PathBuf};

use base64::prelude::*;
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
	let compressed = deflate::deflate_bytes(blueprint_json.as_bytes());
	let blueprint_string = BASE64_STANDARD.encode(compressed);
	println!("0e{}", blueprint_string);
}

#[cfg(test)]
mod tests {
	#[test]
	fn grid_construction() {}
}

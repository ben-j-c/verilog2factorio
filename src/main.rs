use std::path::PathBuf;

use clap::{Arg, Parser};

mod logical_design;
mod mapped_design;
mod physical_design;
mod serializable_design;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
	/// Input technology-mapped file
	#[arg(short, long)]
	input_file: PathBuf,
}

fn main() {
	let args = Args::parse();
}

#[cfg(test)]
mod tests {
	#[test]
	fn grid_construction() {}
}

use itertools::Itertools;
use std::{env, fs};
use v2f::*;
fn main() {
	// Simulate that its being invoked from the CLI, in reality you can just call
	// `v2f -i tb.lua` and it will work.
	env::set_var("V2F_ROOT", env::current_dir().unwrap());
	let tb_dir = std::fs::read_dir("examples/riscv_v2f_optimized/testbench")
		.expect("Failed to read directory.");
	let tb_dir = tb_dir
		.filter_map(|e| e.ok())
		.filter(|p| p.path().is_dir() && !p.path().is_symlink())
		.map(|p| p.path().canonicalize())
		.into_iter()
		.flatten()
		.collect_vec();
	for path in tb_dir {
		env::set_current_dir(&path).expect(&format!("Failed to descend into {path:?}"));
		for input_file in std::fs::read_dir(".")
			.expect("Failed to read directory.")
			.filter_map(|e| e.ok())
			.filter(|p| p.path().is_file() && p.path().extension() == Some("lua".as_ref()))
			.map(|p| p.path())
		{
			let args = Args {
				input_file: Some(input_file),
				dump_phy_cfg: false,
				output_file: None,
			};
			match lua_flow(args) {
				Ok(json) => {
					fs::write("blueprint.json", json)
						.expect(&format!("Failed to write blueprint.json in {path:#?}"));
				},
				Err(e) => {
					match e {
						Error::LuaError(error) => println!("{}", error),
						_ => println!("{:#?}", e),
					};
					assert!(false);
				},
			}
		}
	}
}

#[cfg(test)]
mod tests {
	#[test]
	fn main() {
		super::main();
	}
}

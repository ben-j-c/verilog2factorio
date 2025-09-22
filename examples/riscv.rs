use std::{env, path::PathBuf};
use v2f::*;
fn main() {
	env::set_var("V2F_ROOT", env::current_dir().unwrap());
	let tb_dir = std::fs::read_dir("examples/riscv_v2f_optimized/testbench")
		.expect("Failed to read directory.");
	for entry in tb_dir {
		let entry = entry.expect("Failed to get directory entry");
		let path = entry.path();
		if !path.is_dir() || path.is_symlink() {
			continue;
		}
		env::set_current_dir(&path).expect(&format!("Failed to descend into {path:?}"));
		let args = Args {
			input_file: Some(PathBuf::from("tb.lua")),
			dump_phy_cfg: false,
		};
		match lua_flow(args) {
			Ok(json) => {
				println!("{json}");
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

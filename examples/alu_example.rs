#[cfg(test)]
mod test {
	use std::path::PathBuf;
	#[test]
	fn alu_example() {
		super::main();
		assert!(PathBuf::from("alu.svg").is_file());
	}
}

use std::{env, path::PathBuf};
use v2f::*;
fn main() {
	env::set_var("V2F_ROOT", env::current_dir().unwrap());
	env::set_current_dir("examples/lua/alu").expect("Failed to chdir");
	let args = Args {
		input_file: Some(PathBuf::from("alu.lua")),
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

#[cfg(test)]
mod test {
	#[test]
	fn alu_example() {
		super::main()
	}
}

use std::{env, path::PathBuf};
use v2f::*;
fn main() {
	env::set_var("V2F_ROOT", env::current_dir().unwrap());
	env::set_current_dir("examples/lua/alu").expect("Failed to chdir");
	let args = Args {
		input_file: PathBuf::from("alu.lua"),
	};
	match lua_flow(args) {
		Ok(_) => {},
		Err(e) => {
			match e {
				Error::LuaError(error) => println!("{:?}", error),
				_ => println!("{:?}", e),
			};
			assert!(false);
		},
	}
	assert!(PathBuf::from("alu.svg").is_file());
}

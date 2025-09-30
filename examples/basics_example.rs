#[cfg(test)]
use std::{env, path::PathBuf};
#[cfg(test)]
use v2f::*;

#[test]
fn basics_example() {
	env::set_var("V2F_ROOT", env::current_dir().unwrap());
	env::set_current_dir("examples/lua/basics").expect("Failed to chdir");
	let args = Args {
		input_file: Some(PathBuf::from("basics.lua")),
		dump_phy_cfg: true,
		output_file: None,
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
	assert!(PathBuf::from("basics.svg").is_file());
}

fn main() {}

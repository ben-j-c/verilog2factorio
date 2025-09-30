#[cfg(test)]
use std::{env, fs::File, io::Write, path::PathBuf};
#[cfg(test)]
use v2f::*;

#[test]
fn yosys_example() {
	env::set_var("V2F_ROOT", env::current_dir().unwrap());
	env::set_current_dir("examples/lua/yosys").expect("Failed to chdir");
	let args = Args {
		input_file: Some(PathBuf::from("yosys.lua")),
		dump_phy_cfg: true,
		output_file: None,
	};
	match lua_flow(args) {
		Ok(bp) => {
			let mut f = File::create("balancer_blueprint.json").unwrap();
			f.write_all(bp.as_bytes()).unwrap();
		},
		Err(e) => {
			match e {
				Error::LuaError(error) => println!("{}", error),
				_ => println!("{:?}", e),
			};
			assert!(false);
		},
	}
	assert!(PathBuf::from("yosys.svg").is_file());
}

fn main() {}

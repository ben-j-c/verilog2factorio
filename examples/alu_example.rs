#[cfg(test)]
mod test {
	use std::{env, io::Write, path::PathBuf};
	use v2f::*;

	#[test]
	fn alu_example() {
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
}

fn main() {}

use std::env;
use v2f::*;
fn main() {
	// Simulate that its being invoked from the CLI, in reality you can just call
	// `v2f -i tb.lua` and it will work.
	env::set_var("V2F_ROOT", env::current_dir().unwrap());
	let tb_dir = std::path::PathBuf::from("examples/riscv_v2f_optimized/top_v2f");
	env::set_current_dir(&tb_dir).expect("Failed to CD into top_v2f");
	for input_file in ["sys_verify.lua"] {
		println!("Executing {}", input_file);
		let args = Args {
			input_file: Some(std::path::PathBuf::from(input_file)),
			dump_phy_cfg: false,
			output_file: None,
			convert_to_memh: false,
		};
		match lua_flow(args) {
			Ok(_json) => {
				println!("Pass");
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

#[cfg(test)]
mod tests {
	#[test]
	fn main() {
		super::main();
	}
}

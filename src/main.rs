use std::path::PathBuf;

use clap::Parser as _;
use v2f::{lua_flow, mapped_flow, Args};

pub fn main() -> Result<(), v2f::Error> {
	let args = Args::parse();
	if args.dump_phy_cfg {
		v2f::dump_phy_cfg();
		return Ok(());
	}
	match std::env::var("V2F_ROOT") {
		Ok(_) => {},
		Err(_) => {
			println!("Assuming V2F_ROOT as current working directory.");
			std::env::set_var("V2F_ROOT", std::env::current_dir().unwrap())
		},
	}
	if args.convert_to_memh {
		let input_file = args.input_file.ok_or(v2f::Error::NoSuchFile)?;
		let output_file = args.output_file.unwrap_or(PathBuf::from("program.mem"));
		let mut program_i32 = v2f::util::load_bin_file(input_file)?;
		program_i32.resize(1 << 16, 0);
		v2f::util::save_memh_file(output_file, program_i32)?;
		return Ok(());
	}
	match if args.input_file.as_ref().unwrap().extension().unwrap() == "lua" {
		lua_flow(args)
	} else {
		mapped_flow(args)
	} {
		Ok(text) => {
			println!("{text}");
			Ok(())
		},
		Err(e) => {
			println!("Couldn't complete compilation due to error.");
			match &e {
				v2f::Error::LuaError(error) => {
					println!("{error:#?}");
				},
				_ => {},
			}
			Err(e)
		},
	}
}

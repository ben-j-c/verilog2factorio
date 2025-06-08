use clap::Parser as _;
use v2f::{lua_flow, mapped_flow, Args};

pub fn main() {
	let args = Args::parse();
	match std::env::var("V2F_ROOT") {
		Ok(_) => {},
		Err(_) => {
			println!("Assuming V2F_ROOT as current working directory.");
			std::env::set_var("V2F_ROOT", std::env::current_dir().unwrap())
		},
	}
	match if args.input_file.ends_with(".lua") {
		lua_flow(args)
	} else {
		mapped_flow(args)
	} {
		Ok(json) => println!("Blueprint:\n{}\n", json),
		Err(e) => println!("Couldn't complete compilation due to error: {:?}", e),
	};
}

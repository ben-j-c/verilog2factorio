#[test]
fn yosys_example() {
	env::set_var("V2F_ROOT", env::current_dir().unwrap());
	env::set_current_dir("examples/lua/yosys").expect("Failed to chdir");
	let args = Args {
		input_file: PathBuf::from("yosys.lua"),
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

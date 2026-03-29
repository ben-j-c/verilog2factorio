use std::path::PathBuf;

use clap::Parser as _;

#[cfg(not(target_arch = "wasm32"))]
use v2f::{lua_flow, mapped_flow, Args};

#[cfg(not(target_arch = "wasm32"))]
pub fn main() -> Result<(), v2f::Error> {
	rayon::ThreadPoolBuilder::new()
		.num_threads(5)
		.build_global()
		.unwrap();
	let args = Args::parse();
	if args.dump_phy_cfg {
		v2f::dump_phy_cfg();
		return Ok(());
	}
	#[cfg(not(target_arch = "wasm32"))]
	match std::env::var("V2F_ROOT") {
		Ok(_) => {},
		Err(_) => {
			let detected_root = std::env::current_exe().ok()
				.and_then(|p| p.parent().map(PathBuf::from))
				.or_else(|| std::env::current_dir().ok())
				.expect("Could not infer V2F_ROOT, please set this environment variable to location of executable.");
			println!("Assuming V2F_ROOT as {:?}", detected_root);
			std::env::set_var("V2F_ROOT", detected_root);
		},
	};
	if args.gui {
		let mut native_options = eframe::NativeOptions::default();
		native_options.viewport = egui::ViewportBuilder::default()
			.with_inner_size([800.0, 600.0])
			.with_min_inner_size([300.0, 220.0]);
		return eframe::run_native(
			"eframe template",
			native_options,
			Box::new(|cc| Ok(Box::new(v2f::gui::V2FApp::new(cc)))),
		)
		.map_err(|e| e.into());
	}
	if args.convert_to_memh {
		let input_file = args.input_file.ok_or(v2f::Error::NoSuchFile)?;
		let output_file = args.output_file.unwrap_or(PathBuf::from("program.mem"));
		let mut program_i32 = v2f::util::load_bin_file(input_file)?;
		program_i32.resize(1 << 16, 0);
		v2f::util::save_memh_file(output_file, program_i32)?;
		return Ok(());
	}
	let input_file = args.input_file.ok_or(v2f::Error::NoSuchFile)?;
	let is_lua = input_file.extension().map_or(false, |ext| ext == "lua");
	match if is_lua {
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

#[cfg(target_arch = "wasm32")]
pub fn main() -> Result<(), v2f::Error> {
	use eframe::wasm_bindgen::JsCast as _;

	// Redirect `log` message to `console.log` and friends:
	eframe::WebLogger::init(log::LevelFilter::Debug).ok();

	let web_options = eframe::WebOptions::default();

	wasm_bindgen_futures::spawn_local(async {
		let document = web_sys::window()
			.expect("No window")
			.document()
			.expect("No document");

		let canvas = document
			.get_element_by_id("the_canvas_id")
			.expect("Failed to find the_canvas_id")
			.dyn_into::<web_sys::HtmlCanvasElement>()
			.expect("the_canvas_id was not a HtmlCanvasElement");

		let start_result = eframe::WebRunner::new()
			.start(
				canvas,
				web_options,
				Box::new(|cc| Ok(Box::new(v2f::gui::V2FApp::new(cc)))),
			)
			.await;

		// Remove the loading text and spinner:
		if let Some(loading_text) = document.get_element_by_id("loading_text") {
			match start_result {
				Ok(_) => {
					loading_text.remove();
				},
				Err(e) => {
					loading_text.set_inner_html(
						"<p> The app has crashed. See the developer console for details. </p>",
					);
					panic!("Failed to start eframe: {e:?}");
				},
			}
		}
	});
	Ok(())
}

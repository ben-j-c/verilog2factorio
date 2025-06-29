use mlua::{
	AnyUserData, Error, FromLua, Lua, MetaMethod, MultiValue, UserData, UserDataFields,
	UserDataMethods, Value,
};
use rustyline::{config::Configurer, DefaultEditor};

use std::{
	cell::RefCell,
	fs::File,
	io::{BufReader, Read, Write},
	panic::{catch_unwind, AssertUnwindSafe},
	path::{Path, PathBuf},
	rc::Rc,
};

use crate::{
	checked_design::CheckedDesign,
	get_derivative_file_name, get_v2f_root, get_yosys_exe,
	logical_design::{
		ArithmeticOperator, DeciderOperator, DeciderRowConjDisj, LogicalDesign, NodeId, Signal,
		WireColour, NET_GREEN, NET_RED, NET_RED_GREEN,
	},
	mapped_design::MappedDesign,
	phy::PhysicalDesign,
	signal_lookup_table,
	sim::SimState,
};

impl From<crate::Error> for mlua::Error {
	fn from(value: crate::Error) -> Self {
		Self::RuntimeError(format!("{:?}", value))
	}
}

pub(crate) struct LogicalDesignAPI {
	pub(crate) logd: Rc<RefCell<LogicalDesign>>,
	pub(crate) make_svg: bool,
}

pub(crate) struct PhysicalDesignAPI {
	pub(crate) logd: Rc<RefCell<LogicalDesign>>,
	pub(crate) phyd: Rc<RefCell<PhysicalDesign>>,
}

pub(crate) struct SimStateAPI {
	pub(crate) logd: Rc<RefCell<LogicalDesign>>,
	pub(crate) sim: Rc<RefCell<SimState>>,
}

#[derive(Debug, Clone)]
pub(crate) struct RTL {
	filename: PathBuf,
	top_mod: String,
}

#[derive(Clone, Debug)]
enum TerminalSide {
	Input(NodeId, Rc<RefCell<LogicalDesign>>),
	Output(NodeId, Rc<RefCell<LogicalDesign>>),
}

impl PartialEq for TerminalSide {
	fn eq(&self, other: &Self) -> bool {
		match self {
			TerminalSide::Input(..) => match other {
				TerminalSide::Input(..) => true,
				TerminalSide::Output(..) => false,
			},
			TerminalSide::Output(..) => match other {
				TerminalSide::Input(..) => false,
				TerminalSide::Output(..) => true,
			},
		}
	}
}

impl UserData for DeciderRowConjDisj {}

struct DeciderExpression(Signal, DeciderOperator, Signal);

impl UserData for DeciderExpression {}

struct ArithmeticExpression(Signal, ArithmeticOperator, Signal);

impl UserData for ArithmeticExpression {}

impl TerminalSide {
	fn get(&self) -> (NodeId, Rc<RefCell<LogicalDesign>>) {
		match self {
			TerminalSide::Input(node_id, ref_cell) => (*node_id, ref_cell.clone()),
			TerminalSide::Output(node_id, ref_cell) => (*node_id, ref_cell.clone()),
		}
	}
}

impl UserData for TerminalSide {
	fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
		fields.add_field_method_get("green", |_, this| {
			Ok(Terminal(this.clone(), WireColour::Green))
		});
		fields.add_field_method_get("red", |_, this| Ok(Terminal(this.clone(), WireColour::Red)));
	}
}

impl UserData for WireColour {}

impl FromLua for WireColour {
	fn from_lua(value: Value, _lua: &Lua) -> mlua::Result<Self> {
		match &value {
			Value::String(s) => match s.to_str()?.as_ref() {
				"red" => Ok(WireColour::Red),
				"green" => Ok(WireColour::Green),
				_ => Err(mlua::Error::FromLuaConversionError {
					from: value.type_name(),
					to: "WireColour".to_owned(),
					message: Some("This can't be converted into a wire colour.".into()),
				}),
			},
			Value::UserData(data) if data.is::<WireColour>() => {
				Ok(data.borrow::<WireColour>().unwrap().clone())
			},
			_ => Err(mlua::Error::FromLuaConversionError {
				from: value.type_name(),
				to: "WireColour".to_owned(),
				message: Some("This can't be converted into a wire colour.".into()),
			}),
		}
	}
}

impl UserData for Signal {
	fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
		methods.add_meta_method(MetaMethod::Add, |_, lhs, rhs: Signal| {
			Ok(ArithmeticExpression(*lhs, ArithmeticOperator::Add, rhs))
		});
		methods.add_meta_method(MetaMethod::Sub, |_, lhs, rhs: Signal| {
			Ok(ArithmeticExpression(*lhs, ArithmeticOperator::Sub, rhs))
		});
		methods.add_meta_method(MetaMethod::Mul, |_, lhs, rhs: Signal| {
			Ok(ArithmeticExpression(*lhs, ArithmeticOperator::Mult, rhs))
		});
		methods.add_meta_method(MetaMethod::Div, |_, lhs, rhs: Signal| {
			Ok(ArithmeticExpression(*lhs, ArithmeticOperator::Div, rhs))
		});
		methods.add_meta_method(MetaMethod::Mod, |_, lhs, rhs: Signal| {
			Ok(ArithmeticExpression(*lhs, ArithmeticOperator::Mod, rhs))
		});
		methods.add_meta_method(MetaMethod::Pow, |_, lhs, rhs: Signal| {
			Ok(ArithmeticExpression(*lhs, ArithmeticOperator::Exp, rhs))
		});
		methods.add_meta_method(MetaMethod::Shr, |_, lhs, rhs: Signal| {
			Ok(ArithmeticExpression(*lhs, ArithmeticOperator::Srl, rhs))
		});
		methods.add_meta_method(MetaMethod::Shl, |_, lhs, rhs: Signal| {
			Ok(ArithmeticExpression(*lhs, ArithmeticOperator::Sll, rhs))
		});
		methods.add_meta_method(MetaMethod::BAnd, |_, lhs, rhs: Signal| {
			Ok(ArithmeticExpression(*lhs, ArithmeticOperator::And, rhs))
		});
		methods.add_meta_method(MetaMethod::BOr, |_, lhs, rhs: Signal| {
			Ok(ArithmeticExpression(*lhs, ArithmeticOperator::Or, rhs))
		});
		methods.add_meta_method(MetaMethod::BXor, |_, lhs, rhs: Signal| {
			Ok(ArithmeticExpression(*lhs, ArithmeticOperator::Xor, rhs))
		});
	}
}

impl FromLua for Signal {
	fn from_lua(value: mlua::Value, _lua: &Lua) -> mlua::Result<Self> {
		match &value {
			mlua::Value::Nil => Ok(Signal::None),
			mlua::Value::Integer(c) => Ok(Signal::Constant(*c as i32)),
			mlua::Value::String(s) => {
				let found_signal = signal_lookup_table::lookup_sig_opt(&s.to_str()?);
				match found_signal {
					Some(sig) => Ok(sig),
					None => Err(mlua::Error::FromLuaConversionError {
						from: value.type_name(),
						to: "Signal".to_owned(),
						message: Some(
							"This string doesn't match what I expect a signal to look like.".into(),
						),
					}),
				}
			},
			mlua::Value::UserData(signal) if signal.is::<Signal>() => Ok(*signal.borrow().unwrap()),
			_ => Err(mlua::Error::FromLuaConversionError {
				from: value.type_name(),
				to: "Signal".to_owned(),
				message: Some("Expected a Signal or numeric literal.".into()),
			}),
		}
	}
}

#[derive(Debug, Clone)]
struct Terminal(TerminalSide, WireColour);

impl Terminal {
	fn get(&self) -> (NodeId, Rc<RefCell<LogicalDesign>>, WireColour) {
		let inner = self.0.get();
		(inner.0, inner.1, self.1)
	}
}

fn method_connect(this: &Terminal, other: AnyUserData) -> Result<(), mlua::Error> {
	let other = other.borrow::<TerminalSide>()?;
	let (this_id, this_logd, this_colour) = this.get();
	let (other_id, other_logd) = other.get();
	if this_logd.as_ptr() != other_logd.as_ptr() {
		return Err(Error::RuntimeError(
			"Can't connect two logical designs together".to_owned(),
		));
	}
	if this_id == other_id && this.0 == *other {
		return Err(Error::RuntimeError(
			"Can't connect a terminal to itself.".to_owned(),
		));
	}
	let res = catch_unwind(AssertUnwindSafe(|| {
		let mut fanin = vec![];
		let mut fanout = vec![];
		match this.0 {
			TerminalSide::Input(..) => fanout.push(this_id),
			TerminalSide::Output(..) => fanin.push(this_id),
		}
		match *other {
			TerminalSide::Input(..) => fanout.push(other_id),
			TerminalSide::Output(..) => fanin.push(other_id),
		}
		match this_colour {
			WireColour::Red => this_logd.borrow_mut().add_wire_red(fanin, fanout),
			WireColour::Green => this_logd.borrow_mut().add_wire_green(fanin, fanout),
		};
	}));
	if let Err(_) = res {
		Err(Error::RuntimeError("Invalid connection.".to_owned()))
	} else {
		Ok(res.unwrap())
	}
}

fn method_check_yosys() -> Result<(), mlua::Error> {
	let yosys_exe = get_yosys_exe()?;
	let output = std::process::Command::new(yosys_exe)
		.arg("--version")
		.output()?;
	if !output.status.success() {
		return Err(Error::runtime(format!(
			"Yosys got error code {}",
			output.status
		)));
	}
	let result = String::from_utf8(output.stdout)
		.or_else(|_| Err(Error::runtime("Can't get the version.")))?;
	if !result.starts_with("Yosys 0.52 (git sha1 fee39a328") {
		Err(Error::runtime(
			"Wrong yosys version, expected \"Yosys 0.52 (git sha1 fee39a328\"",
		))
	} else {
		Ok(())
	}
}

fn method_load_rtl<P>(filename: P, top_mod: String) -> Result<RTL, mlua::Error>
where
	P: AsRef<Path>,
{
	let filename: &Path = filename.as_ref();
	let filename_out = get_derivative_file_name(&filename, "_rtl.json")?;
	method_check_yosys()?;
	let exe_dir = get_v2f_root()?;
	if !exe_dir.is_dir() {
		return Err(Error::RuntimeError(format!("Can't locate V2F_ROOT")));
	}
	if !filename.is_file() {
		return Err(Error::RuntimeError(format!(
			"{:?} is not a file.",
			filename
		)));
	}

	{
		let mut rtl_script = File::open(exe_dir.join("v2flib/rtl.ys"))?;
		let mut buf = Vec::new();
		rtl_script.read_to_end(&mut buf)?;
		let rtl_script_text = String::from_utf8(buf).map_err(|e| e.utf8_error())?;
		let rtl_script_text = rtl_script_text
			.replace("{filename}", filename.to_str().unwrap())
			.replace("{filename_out}", filename_out.as_os_str().to_str().unwrap())
			.replace("{exe_dir}", exe_dir.to_str().unwrap())
			.replace("{top_mod}", &top_mod);
		let mut rtl_script_final = File::create("rtl.ys")?;
		rtl_script_final.write_all(rtl_script_text.as_bytes())?;
		rtl_script_final.flush()?;
	}

	let rtl_output = std::process::Command::new(get_yosys_exe()?)
		.arg("--quiet")
		.arg("--quiet")
		.arg("--scriptfile")
		.arg("rtl.ys")
		.output()?;
	if !rtl_output.status.success() {
		return Err(Error::runtime("Failed to compile to RTL."));
	}

	return Ok(RTL {
		filename: filename_out,
		top_mod,
	});
}

fn method_map_rtl<P>(filename: P, top_mod: &String) -> Result<LogicalDesignAPI, mlua::Error>
where
	P: AsRef<Path>,
{
	let filename_out = get_derivative_file_name(&filename, "_map.json")?;
	method_check_yosys()?;
	let filename: &Path = filename.as_ref();
	if !filename.is_file() {
		return Err(Error::RuntimeError(format!(
			"{:?} is not a file.",
			filename
		)));
	}
	if !filename.to_string_lossy().ends_with("_rtl.json") {
		return Err(Error::RuntimeError(format!(
			"{:?} is not an RTL json.",
			filename
		)));
	}
	let exe_dir = get_v2f_root()?;
	{
		let mut buf = Vec::new();
		let mut mapping_script = File::open(exe_dir.join("v2flib/mapping.ys"))?;
		mapping_script.read_to_end(&mut buf)?;
		let mapping_script_text = String::from_utf8(buf).map_err(|e| e.utf8_error())?;
		let mapping_script_text = mapping_script_text
			.replace("{filename}", filename.to_str().unwrap())
			.replace("{filename_out}", filename_out.to_str().unwrap())
			.replace("{exe_dir}", exe_dir.to_str().unwrap())
			.replace("{top_mod}", top_mod);
		let mut mappingscript_final = File::create("mapping.ys")?;
		mappingscript_final.write_all(mapping_script_text.as_bytes())?;
		mappingscript_final.flush()?;
	}

	let mapping_output = std::process::Command::new(get_yosys_exe()?)
		.arg("--quiet")
		.arg("--quiet")
		.arg("--scriptfile")
		.arg("mapping.ys")
		.output()?;
	if !mapping_output.status.success() {
		match String::from_utf8(mapping_output.stderr) {
			Ok(s) => return Err(Error::runtime(s)),
			Err(_) => return Err(Error::runtime("<Failed to get stdout>")),
		}
	}

	let file = File::open(filename_out)?;
	let reader = BufReader::new(file);
	let mapped_design: MappedDesign =
		serde_json::from_reader(reader).map_err(|_| Error::runtime("failed to map design."))?;
	let mut checked_design = CheckedDesign::new();
	checked_design.build_from(&mapped_design);
	let mut logd = LogicalDesign::new();
	logd.build_from(&checked_design, &mapped_design);
	Ok(LogicalDesignAPI {
		logd: Rc::new(RefCell::new(logd)),
		make_svg: false,
	})
}

impl UserData for Terminal {
	fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
		methods.add_method("connect", |_, this, other: AnyUserData| {
			method_connect(this, other)
		});
	}
}

struct Decider {
	id: NodeId,
	logd: Rc<RefCell<LogicalDesign>>,
}

struct Arithmetic {
	id: NodeId,
	logd: Rc<RefCell<LogicalDesign>>,
}

struct Lamp {
	id: NodeId,
	logd: Rc<RefCell<LogicalDesign>>,
}

struct Constant {
	id: NodeId,
	logd: Rc<RefCell<LogicalDesign>>,
}

impl UserData for Decider {
	fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
		fields.add_field_method_get("input", |_, this| {
			Ok(TerminalSide::Input(this.id, this.logd.clone()))
		});
		fields.add_field_method_get("output", |_, this| {
			Ok(TerminalSide::Output(this.id, this.logd.clone()))
		});
	}

	fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
		methods.add_method(
			"add_condition",
			|_, this, args: (AnyUserData, AnyUserData, i32, i32)| {
				let row_op = args.0.borrow::<DeciderRowConjDisj>()?;
				let expr = args.1.borrow::<DeciderExpression>()?;
				let net_left = (args.2 & 1 > 0, args.2 & 2 > 0);
				let net_right = (args.3 & 1 > 0, args.3 & 2 > 0);
				let res = catch_unwind(AssertUnwindSafe(|| {
					this.logd.borrow_mut().add_decider_input(
						this.id,
						(expr.0, expr.1, expr.2),
						row_op.clone(),
						net_left,
						net_right,
					);
				}));
				if let Err(_) = res {
					Err(Error::RuntimeError("Condition invalid.".to_owned()))
				} else {
					Ok(())
				}
			},
		);
		methods.add_method("add_output", |_, this, args: (Signal, Value, i32)| {
			let sig = args.0;
			let net_out = (args.2 & 1 > 0, args.2 & 2 > 0);
			let constant = match args.1 {
				Value::Nil => None,
				Value::Integer(c) if c <= i32::MAX as i64 && c >= i32::MIN as i64 => Some(c as i32),
				_ => {
					return Err(mlua::Error::FromLuaConversionError {
						from: args.1.type_name(),
						to: "integer|nil".to_owned(),
						message: Some("This value isn't an i32 or nil.".into()),
					})
				},
			};
			let res = catch_unwind(AssertUnwindSafe(|| {
				if let Some(constant) = constant {
					this.logd
						.borrow_mut()
						.add_decider_out_constant(this.id, sig, constant, net_out);
				} else {
					this.logd
						.borrow_mut()
						.add_decider_out_input_count(this.id, sig, net_out);
				}
			}));
			if let Err(_) = res {
				Err(Error::RuntimeError("Output invalid.".to_owned()))
			} else {
				Ok(())
			}
		});
	}
}

impl UserData for Arithmetic {
	fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
		fields.add_field_method_get("input", |_, this| {
			Ok(TerminalSide::Input(this.id, this.logd.clone()))
		});
		fields.add_field_method_get("output", |_, this| {
			Ok(TerminalSide::Output(this.id, this.logd.clone()))
		});
	}
}

impl UserData for Constant {
	fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
		fields.add_field_method_get("output", |_, this| {
			Ok(TerminalSide::Output(this.id, this.logd.clone()))
		});
	}
}

impl UserData for Lamp {
	fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
		fields.add_field_method_get("input", |_, this| {
			Ok(TerminalSide::Input(this.id, this.logd.clone()))
		});
	}
}

impl UserData for LogicalDesignAPI {
	fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
		fields.add_field_method_get("description", |_, this| {
			Ok(this.logd.borrow().description.clone())
		});
		fields.add_field_method_set("description", |_, this, description: String| {
			this.logd.borrow_mut().set_description(description);
			Ok(())
		});
	}

	fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
		methods.add_method("add_decider", |_, this, _: ()| {
			let id = this.logd.borrow_mut().add_decider();
			Ok(Decider {
				id,
				logd: this.logd.clone(),
			})
		});
		methods.add_method(
			"add_arithmetic",
			|_, this, args: (AnyUserData, Signal, i32, i32)| {
				let expr = args.0.borrow::<ArithmeticExpression>()?;
				let net_left = (args.2 & 1 > 0, args.2 & 2 > 0);
				let net_right = (args.3 & 1 > 0, args.3 & 2 > 0);
				let out = args.1;
				let id = this.logd.borrow_mut().add_arithmetic_with_net(
					(expr.0, expr.1, expr.2),
					out,
					net_left,
					net_right,
				);
				Ok(Arithmetic {
					id,
					logd: this.logd.clone(),
				})
			},
		);
		methods.add_method("add_lamp", |_, this, args: (AnyUserData,)| {
			let expr = args.0.borrow::<DeciderExpression>()?;
			let id = this.logd.borrow_mut().add_lamp((expr.0, expr.1, expr.2));
			Ok(Lamp {
				id,
				logd: this.logd.clone(),
			})
		});
		methods.add_method(
			"add_constant",
			|_, this, (sigs, counts): (Vec<Signal>, Vec<i32>)| {
				if sigs.len() != counts.len() {
					return Err(Error::RuntimeError("Mismatched length.".to_owned()));
				}
				let id = this.logd.borrow_mut().add_constant(sigs, counts);
				Ok(Constant {
					id,
					logd: this.logd.clone(),
				})
			},
		);
		methods.add_method("print", |_, this, _: ()| {
			println!("{}", this.logd.borrow());
			Ok(())
		});
		methods.add_method_mut("make_svg", |_, this, _: ()| {
			this.make_svg = true;
			Ok(())
		});
		methods.add_method("new_simulation", |_, this, _: ()| {
			Ok(SimStateAPI {
				logd: this.logd.clone(),
				sim: Rc::new(RefCell::new(SimState::new(this.logd.clone()))),
			})
		});
	}
}

impl UserData for PhysicalDesignAPI {}

fn verify_is_combinator(this: &SimStateAPI, data: &AnyUserData) -> Result<NodeId, mlua::Error> {
	let (nodeid, logd) = if let Ok(x) = data.borrow::<Decider>() {
		(x.id, x.logd.clone())
	} else if let Ok(x) = data.borrow::<Arithmetic>() {
		(x.id, x.logd.clone())
	} else if let Ok(x) = data.borrow::<Lamp>() {
		(x.id, x.logd.clone())
	} else if let Ok(x) = data.borrow::<Constant>() {
		(x.id, x.logd.clone())
	} else {
		return Err(Error::runtime("Got non-combinator as an input."));
	};
	if logd.as_ptr() != this.logd.as_ptr() {
		return Err(Error::runtime(
			"Supplied a combinator that doesn't belong to this design.",
		));
	}
	Ok(nodeid)
}

impl UserData for SimStateAPI {
	fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
		methods.add_method("step", |_, this, steps: usize| {
			this.sim.borrow_mut().step(steps);
			Ok(())
		});
		methods.add_method("probe", |lua, this, data: AnyUserData| {
			let signals: Vec<(i32, i32)> = if let Ok(term) = data.borrow::<Terminal>() {
				let (nodeid, logd, colour) = term.get();
				if logd.as_ptr() != this.logd.as_ptr() {
					return Err(Error::runtime(
						"Supplied a combinator that doesn't belong to this design.",
					));
				}
				let net = match colour {
					WireColour::Red => NET_RED,
					WireColour::Green => NET_GREEN,
				};
				match term.0 {
					TerminalSide::Input(_, _) => this.sim.borrow().probe_input(nodeid, net),
					TerminalSide::Output(_, _) => this.sim.borrow().probe_red_out(nodeid),
				}
			} else if let Ok(term_side) = data.borrow::<TerminalSide>() {
				let (nodeid, logd) = term_side.clone().get();
				if logd.as_ptr() != this.logd.as_ptr() {
					return Err(Error::runtime(
						"Supplied a combinator that doesn't belong to this design.",
					));
				}
				match *term_side {
					TerminalSide::Input(_, _) => {
						this.sim.borrow().probe_input(nodeid, NET_RED_GREEN)
					},
					TerminalSide::Output(_, _) => this.sim.borrow().probe_red_out(nodeid),
				}
			} else {
				return Err(Error::runtime("Passed an invalid type into probe."));
			};
			let ret = lua.create_table()?;
			for s in signals {
				ret.set(Signal::Id(s.0), s.1)?;
			}
			Ok(ret)
		});
		methods.add_method("add_trace", |_, this, data: AnyUserData| {
			let nodeid = verify_is_combinator(this, &data)?;
			this.sim.borrow_mut().add_trace(nodeid);
			Ok(())
		});
		methods.add_method("probe_lamp_state", |_, this, data: AnyUserData| {
			let nodeid = verify_is_combinator(this, &data)?;
			if !data.is::<Lamp>() {
				return Err(Error::runtime("Tried to probe a non-lamp as a lamp."));
			}
			Ok(this.sim.borrow().probe_lamp_state(nodeid))
		});
		methods.add_method("save_svg", |_, this, filename: String| {
			let svg = this.sim.borrow().render_traces();
			svg.save(filename)?;
			Ok(())
		});
		methods.add_method("print", |_, this, _: ()| {
			this.sim.borrow().print();
			Ok(())
		});
	}
}

impl UserData for RTL {
	fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
		methods.add_method("to_design", |_, this: &RTL, _: ()| {
			method_map_rtl(&this.filename, &this.top_mod)
		});
	}
}

impl FromLua for RTL {
	fn from_lua(value: Value, _lua: &Lua) -> mlua::Result<Self> {
		match &value {
			Value::String(filename) => {
				method_load_rtl(filename.to_string_lossy(), "top".to_owned())
			},
			Value::UserData(data) => Ok(data.borrow::<Self>()?.clone()),
			_ => Err(Error::FromLuaConversionError {
				from: value.type_name(),
				to: "RTL".to_owned(),
				message: None,
			}),
		}
	}
}

pub fn get_lua() -> Result<Lua, Error> {
	let lua = Lua::new();
	lua.globals().set(
		"Signal",
		lua.create_function(
			|_, name: String| match signal_lookup_table::lookup_sig_opt(&name) {
				Some(signal) => Ok(signal),
				None => Err(Error::RuntimeError("Not a real signal.".to_owned())),
			},
		)?,
	)?;
	lua.globals().set("Each", Signal::Each)?;
	lua.globals().set("Anything", Signal::Anything)?;
	lua.globals().set("Everything", Signal::Everything)?;
	lua.globals().set(
		"Constant",
		lua.create_function(|_, val: i32| Ok(Signal::Constant(val)))?,
	)?;

	lua.globals().set("NET_NONE", 0)?;
	lua.globals().set("NET_RED", 1)?;
	lua.globals().set("NET_GREEN", 2)?;
	lua.globals().set("NET_REDGREEN", 3)?;

	lua.globals()
		.set("FirstRow", DeciderRowConjDisj::FirstRow)?;
	lua.globals().set("AND", DeciderRowConjDisj::And)?;
	lua.globals().set("OR", DeciderRowConjDisj::Or)?;

	lua.globals().set(
		"get_empty_design",
		lua.create_function(|_, _: ()| {
			Ok(LogicalDesignAPI {
				logd: Rc::new(RefCell::new(LogicalDesign::new())),
				make_svg: false,
			})
		})?,
	)?;

	lua.globals().set(
		"connect",
		lua.create_function(|_, args: (AnyUserData, AnyUserData)| {
			let this = args.0.borrow::<Terminal>()?;
			method_connect(&this, args.1)
		})?,
	)?;

	lua.globals().set(
		"Expr",
		lua.create_function(|_, (lhs, op, rhs): (Signal, String, Signal)| {
			let op = match op.as_str() {
				"==" | "=" => DeciderOperator::Equal,
				">" => DeciderOperator::GreaterThan,
				">=" => DeciderOperator::GreaterThanEqual,
				"<" => DeciderOperator::LessThan,
				"<=" => DeciderOperator::LessThanEqual,
				"!=" => DeciderOperator::NotEqual,
				_ => {
					return Err(mlua::Error::RuntimeError(
						"Not a real decider operator.".into(),
					))
				},
			};
			Ok(DeciderExpression(lhs, op, rhs))
		})?,
	)?;

	lua.globals().set(
		"yosys_check",
		lua.create_function(|_, _: ()| method_check_yosys())?,
	)?;

	lua.globals().set(
		"yosys_load_rtl",
		lua.create_function(|_, (filename, top_mod): (String, String)| {
			method_load_rtl(filename, top_mod)
		})?,
	)?;

	lua.globals().set(
		"yosys_map_rtl",
		lua.create_function(|_, rtl: RTL| method_map_rtl(rtl.filename, &rtl.top_mod))?,
	)?;

	lua.globals().set(
		"enter_repl",
		lua.create_function(|lua, filename: Option<String>| {
			let mut editor = DefaultEditor::new().expect("Failed to create editor");
			editor.set_tab_stop(4);
			let mut line = String::new();
			let mut prompt = "> ";
			loop {
				match editor.readline(prompt) {
					Ok(v) => line.push_str(&v),
					Err(e) => match e {
						rustyline::error::ReadlineError::Io(_) => {
							return Err(Error::runtime("readline: IO error"))
						},
						rustyline::error::ReadlineError::Eof => break,
						rustyline::error::ReadlineError::Interrupted => continue,
						rustyline::error::ReadlineError::Errno(errno) => {
							return Err(Error::RuntimeError(
								"readline: ".to_owned() + &errno.to_string(),
							))
						},
						rustyline::error::ReadlineError::WindowResized => continue,
						_ => continue,
					},
				}

				match lua.load(&line).eval::<MultiValue>() {
					Ok(values) => {
						editor.add_history_entry(&line).unwrap();
						println!(
							"{}",
							values
								.iter()
								.map(|value| format!("{:#?}", value))
								.collect::<Vec<_>>()
								.join("\t")
						);
					},
					Err(Error::SyntaxError {
						incomplete_input: true,
						..
					}) => {
						line.push_str("\n");
						prompt = ">> ";
						continue;
					},
					Err(e) => return Err(e),
				}
				line.clear();
			}
			if let Some(filename) = filename {
				let mut file = File::create(filename)?;
				for line in editor.history() {
					file.write(line.as_bytes())?;
					file.write("\n".as_bytes())?;
				}
				todo!()
			}
			Ok(())
		})?,
	)?;

	Ok(lua)
}

pub mod physical_ensemble;
pub use physical_ensemble::*;

use itertools::Itertools;
use mlua::{
	AnyUserData, Error, FromLua, Lua, MetaMethod, MultiValue, Table, UserData, UserDataFields,
	UserDataMethods, Value,
};
use rustyline::{config::Configurer, DefaultEditor};

use std::{
	fmt::Display,
	fs::File,
	io::{BufReader, Read, Write},
	ops::Deref,
	panic::{catch_unwind, AssertUnwindSafe},
	path::{Path, PathBuf},
	sync::{atomic::AtomicUsize, Arc, RwLock},
};

type LogDRef = Arc<RwLock<LogicalDesign>>;
type PhyDRef = Arc<RwLock<PhysicalDesign>>;
type SimRef = Arc<RwLock<SimState>>;

use crate::{
	checked_design::CheckedDesign,
	get_derivative_file_name, get_v2f_root, get_yosys_exe,
	logical_design::{
		ArithmeticOperator, DeciderOperator, DeciderRowConjDisj, LogicalDesign, NodeId, Signal,
		WireColour, NET_GREEN, NET_RED, NET_RED_GREEN,
	},
	mapped_design::MappedDesign,
	phy::PhysicalDesign,
	serializable_design::SerializableDesign,
	signal_lookup_table,
	sim::{self, vcd::VCD, SimState},
	util::{hash_map, HashM},
};

impl From<crate::Error> for mlua::Error {
	fn from(value: crate::Error) -> Self {
		Self::RuntimeError(format!("{:?}", value))
	}
}

static ID_COUNTER: AtomicUsize = AtomicUsize::new(0);

#[derive(Clone)]
pub(crate) struct LogicalDesignAPI {
	pub(crate) id: usize,
	pub(crate) logd: LogDRef,
	pub(crate) make_svg: bool,
	pub(crate) group_io: bool,
}

#[derive(Clone)]
pub(crate) struct PhysicalDesignAPI {
	pub(crate) log_id: usize,
	pub(crate) logd: LogDRef,
	pub(crate) phyd: PhyDRef,
}

pub(crate) struct SimStateAPI {
	pub(crate) log_id: usize,
	pub(crate) logd: LogDRef,
	pub(crate) sim: SimRef,
}

#[derive(Debug, Clone, Default)]
pub(crate) struct SignalTable {
	pub(crate) signals: HashM<Signal, i32>,
}

fn runtime_err<T, S: Display>(err: S) -> mlua::Result<T> {
	Err(Error::runtime(err))
}

impl PartialEq for SignalTable {
	fn eq(&self, other: &SignalTable) -> bool {
		for (sig, count) in &self.signals {
			match other.signals.get(sig) {
				Some(count2) => {
					if *count != *count2 {
						return false;
					}
				},
				None => {
					if *count != 0 {
						return false;
					}
				},
			}
		}
		for (sig, count) in &other.signals {
			match self.signals.get(sig) {
				Some(count2) => {
					if *count != *count2 {
						return false;
					}
				},
				None => {
					if *count != 0 {
						return false;
					}
				},
			}
		}
		true
	}
}

#[derive(Debug, Clone)]
pub(crate) struct RTL {
	filename: PathBuf,
	top_mod: String,
	promote_all_nets_to_ports: bool,
}

#[derive(Clone, Debug)]
enum TerminalSide {
	Input(usize, NodeId, LogDRef),
	Output(usize, NodeId, LogDRef),
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
	fn get(&self) -> (usize, NodeId, LogDRef) {
		match self {
			TerminalSide::Input(log_id, node_id, ref_cell) => (*log_id, *node_id, ref_cell.clone()),
			TerminalSide::Output(log_id, node_id, ref_cell) => {
				(*log_id, *node_id, ref_cell.clone())
			},
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
				Ok(*data.borrow::<WireColour>().unwrap())
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
			Ok(ArithmeticExpression(*lhs, ArithmeticOperator::Sshr, rhs))
		});
		methods.add_meta_method(MetaMethod::Shl, |_, lhs, rhs: Signal| {
			Ok(ArithmeticExpression(*lhs, ArithmeticOperator::Shl, rhs))
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
		methods.add_meta_method(MetaMethod::Eq, |_, lhs, rhs: Signal| Ok(*lhs == rhs));
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
	fn get(&self) -> (usize, NodeId, LogDRef, WireColour) {
		let inner = self.0.get();
		(inner.0, inner.1, inner.2, self.1)
	}
}

fn method_connect(this: &Terminal, other: AnyUserData) -> Result<(), mlua::Error> {
	let other = other.borrow::<TerminalSide>()?;
	let (log_id, this_id, this_logd, this_colour) = this.get();
	let (log_id_other, other_id, _other_logd) = other.get();
	if log_id != log_id_other {
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
			WireColour::Red => this_logd.write().unwrap().add_wire_red(fanin, fanout),
			WireColour::Green => this_logd.write().unwrap().add_wire_green(fanin, fanout),
		};
	}));
	if let Err(_) = res {
		Err(Error::RuntimeError("Invalid connection.".to_owned()))
	} else {
		res.unwrap();
		Ok(())
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
	let result =
		String::from_utf8(output.stdout).map_err(|_| Error::runtime("Can't get the version."))?;
	if !result.starts_with("Yosys 0.52 (git sha1 fee39a328") {
		Err(Error::runtime(
			"Wrong yosys version, expected \"Yosys 0.52 (git sha1 fee39a328\"",
		))
	} else {
		Ok(())
	}
}

fn method_sim_yosys(filename_in: &str, filename_out: &str) -> Result<(), mlua::Error> {
	method_check_yosys()?;
	let exe_dir = get_v2f_root()?;
	if !exe_dir.is_dir() {
		return Err(Error::RuntimeError("Can't locate V2F_ROOT".to_string()));
	}
	{
		let mut sim_script = File::open(exe_dir.join("v2flib/sim.ys"))?;
		let mut buf = Vec::new();
		sim_script.read_to_end(&mut buf)?;
		let sim_script_text = String::from_utf8(buf).map_err(|e| e.utf8_error())?;
		let sim_script_text = sim_script_text
			.replace("{input_vcd}", filename_in)
			.replace("{output_vcd}", filename_out);
		let mut rtl_script_final = File::create("sim.ys")?;
		rtl_script_final.write_all(sim_script_text.as_bytes())?;
		rtl_script_final.flush()?;
	}

	let rtl_output = std::process::Command::new(get_yosys_exe()?)
		.arg("--quiet")
		.arg("--quiet")
		.arg("--scriptfile")
		.arg("sim.ys")
		.output()?;
	if !rtl_output.status.success() {
		return Err(Error::runtime("Failed to sim RTL."));
	}
	Ok(())
}

fn method_load_rtl<P>(
	filenames: &[P],
	top_mod: String,
	include_dir: Option<String>,
) -> Result<RTL, mlua::Error>
where
	P: AsRef<Path>,
{
	if filenames.is_empty() {
		return Err(Error::runtime("No files provided."));
	}
	let filename_primary: &Path = filenames[0].as_ref();
	let filename_out = get_derivative_file_name(filename_primary, "_rtl.json")?;
	method_check_yosys()?;
	let exe_dir = get_v2f_root()?;
	if !exe_dir.is_dir() {
		return Err(Error::RuntimeError("Can't locate V2F_ROOT".to_string()));
	}
	for file in filenames {
		let file = file.as_ref();
		if !file.is_file() {
			return Err(Error::RuntimeError(format!("{:?} is not a file.", file)));
		}
	}
	let filenames = filenames
		.iter()
		.map(|file| (file.as_ref(), file.as_ref().to_str()));
	let mut source_read = String::new();
	for (path, file) in filenames {
		if file.is_none() {
			return Err(Error::runtime(format!("{path:?} has non-utf8 character.")));
		}
		let file = file.unwrap();
		source_read += &format!("read_verilog {file}\n");
	}

	{
		let mut rtl_script = File::open(exe_dir.join("v2flib/rtl.ys"))?;
		let mut buf = Vec::new();
		rtl_script.read_to_end(&mut buf)?;
		let rtl_script_text = String::from_utf8(buf).map_err(|e| e.utf8_error())?;
		let rtl_script_text = rtl_script_text
			.replace("{source_read}", &source_read)
			.replace("{filename_out}", filename_out.as_os_str().to_str().unwrap())
			.replace("{exe_dir}", exe_dir.to_str().unwrap())
			.replace("{top_mod}", &top_mod);
		let rtl_script_text = if let Some(include_dir) = include_dir {
			rtl_script_text.replace(
				"{include_dir}",
				&format!(" -libdir {} ", include_dir.as_str()),
			)
		} else {
			rtl_script_text.replace("{include_dir}", "")
		};
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

	Ok(RTL {
		filename: filename_out,
		top_mod,
		promote_all_nets_to_ports: false,
	})
}

fn method_map_rtl(rtl: &RTL) -> Result<LogicalDesignAPI, mlua::Error> {
	let filename_out = get_derivative_file_name(&rtl.filename, "_map.json")?;
	method_check_yosys()?;
	let filename: &Path = rtl.filename.as_ref();
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
		let mut mapping_script = File::open(exe_dir.join("v2flib/mapping.ys")).map_err(|e| {
			let msg = format!(
				"{}, {}",
				e.to_string(),
				exe_dir.join("v2flib/mapping.ys").to_string_lossy(),
			);
			mlua::Error::runtime(msg)
		})?;
		mapping_script.read_to_end(&mut buf)?;
		let mapping_script_text = String::from_utf8(buf).map_err(|e| e.utf8_error())?;
		let mapping_script_text = mapping_script_text
			.replace("{filename}", filename.to_str().unwrap())
			.replace("{filename_out}", filename_out.to_str().unwrap())
			.replace("{exe_dir}", exe_dir.to_str().unwrap())
			.replace("{top_mod}", &rtl.top_mod);
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
	checked_design.promote_all_nets_to_ports = rtl.promote_all_nets_to_ports;
	checked_design.build_from(&mapped_design);
	let mut logd = LogicalDesign::new();
	logd.build_from(&checked_design, &mapped_design);
	checked_design.save_dot(&mapped_design);
	Ok(LogicalDesignAPI {
		id: ID_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed),
		logd: Arc::new(RwLock::new(logd)),
		make_svg: false,
		group_io: false,
	})
}

fn method_load_mapped_rtl<P: AsRef<Path>>(
	mapped_json: &P,
) -> Result<LogicalDesignAPI, mlua::Error> {
	let file = File::open(mapped_json)?;
	let reader = BufReader::new(file);
	let mapped_design: MappedDesign =
		serde_json::from_reader(reader).map_err(|_| Error::runtime("failed to map design."))?;
	let mut checked_design = CheckedDesign::new();
	checked_design.build_from(&mapped_design);
	let mut logd = LogicalDesign::new();
	logd.build_from(&checked_design, &mapped_design);
	Ok(LogicalDesignAPI {
		id: ID_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed),
		logd: Arc::new(RwLock::new(logd)),
		make_svg: false,
		group_io: false,
	})
}

impl UserData for Terminal {
	fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
		methods.add_method("connect", |_, this, other: AnyUserData| {
			method_connect(this, other)
		});
	}
}

#[derive(Debug, Clone)]
struct Decider {
	log_id: usize,
	id: NodeId,
	logd: LogDRef,
}

#[derive(Debug, Clone)]
struct Arithmetic {
	log_id: usize,
	id: NodeId,
	logd: LogDRef,
}

#[derive(Debug, Clone)]
struct Lamp {
	log_id: usize,
	id: NodeId,
	logd: LogDRef,
}

#[derive(Debug, Clone)]
struct DisplayPanel {
	log_id: usize,
	id: NodeId,
	logd: LogDRef,
}

#[derive(Debug, Clone)]
struct Constant {
	log_id: usize,
	id: NodeId,
	logd: LogDRef,
}

impl UserData for Decider {
	fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
		fields.add_field_method_get("input", |_, this| {
			Ok(TerminalSide::Input(this.log_id, this.id, this.logd.clone()))
		});
		fields.add_field_method_get("output", |_, this| {
			Ok(TerminalSide::Output(
				this.log_id,
				this.id,
				this.logd.clone(),
			))
		});
		fields.add_field_method_get("signals", |_, this| {
			let binding = this.logd.clone();
			let logd = binding.write().unwrap();
			Ok(logd
				.get_output_signals(this.id)
				.iter()
				.copied()
				.collect_vec())
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
					this.logd.write().unwrap().add_decider_input(
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
						.write()
						.unwrap()
						.add_decider_out_constant(this.id, sig, constant, net_out);
				} else {
					this.logd
						.write()
						.unwrap()
						.add_decider_out_input_count(this.id, sig, net_out);
				}
			}));
			if let Err(_) = res {
				Err(Error::RuntimeError("Output invalid.".to_owned()))
			} else {
				Ok(())
			}
		});
		methods.add_method("set_description", |_, this, name: String| {
			this.logd
				.write()
				.unwrap()
				.set_description_node(this.id, name);
			Ok(())
		});
	}
}

impl UserData for Arithmetic {
	fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
		fields.add_field_method_get("input", |_, this| {
			Ok(TerminalSide::Input(this.log_id, this.id, this.logd.clone()))
		});
		fields.add_field_method_get("output", |_, this| {
			Ok(TerminalSide::Output(
				this.log_id,
				this.id,
				this.logd.clone(),
			))
		});
		fields.add_field_method_get("signals", |_, this| {
			let binding = this.logd.clone();
			let logd = binding.write().unwrap();
			Ok(logd
				.get_output_signals(this.id)
				.iter()
				.copied()
				.collect_vec())
		});
	}

	fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
		methods.add_method("set_description", |_, this, name: String| {
			this.logd
				.write()
				.unwrap()
				.set_description_node(this.id, name);
			Ok(())
		});
	}
}

impl UserData for Constant {
	fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
		fields.add_field_method_get("output", |_, this| {
			Ok(TerminalSide::Output(
				this.log_id,
				this.id,
				this.logd.clone(),
			))
		});
		fields.add_field_method_get("signals", |_, this| {
			let binding = this.logd.clone();
			let logd = binding.read().unwrap();
			if logd.is_port(this.id).is_some() {
				return Ok(logd.get_port_signal(this.id).into_iter().collect_vec());
			}
			Ok(vec![])
		});
	}

	fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
		methods.add_method(
			"set_ith_output_count",
			|_, this, (idx, count): (usize, i32)| {
				let binding = this.logd.clone();
				let mut logd = binding.write().unwrap();
				let node = logd.get_node(this.id);
				if node.output.len() <= idx {
					return Err(Error::runtime("idx too large"));
				}
				logd.set_ith_output_count(this.id, idx, count);
				Ok(())
			},
		);

		methods.add_method(
			"set_outputs",
			|_, this, (sigs, vals): (Vec<Signal>, Vec<i32>)| {
				assert_eq!(sigs.len(), vals.len());
				let binding = this.logd.clone();
				let mut logd = binding.write().unwrap();
				logd.set_constants_output(this.id, sigs, vals);
				Ok(())
			},
		);

		methods.add_method("set_enabled", |_, this, status: bool| {
			let binding = this.logd.clone();
			let mut logd = binding.write().unwrap();
			logd.set_constant_enabled(this.id, status);
			Ok(())
		});

		methods.add_method("set_description", |_, this, name: String| {
			this.logd
				.write()
				.unwrap()
				.set_description_node(this.id, name);
			Ok(())
		});
	}
}

impl UserData for Lamp {
	fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
		fields.add_field_method_get("input", |_, this| {
			Ok(TerminalSide::Input(this.log_id, this.id, this.logd.clone()))
		});
		fields.add_field_method_get("signals", |_, this| {
			let binding = this.logd.clone();
			let logd = binding.read().unwrap();
			if logd.is_port(this.id).is_some() {
				return Ok(logd.get_port_signal(this.id).into_iter().collect_vec());
			}
			Ok(vec![])
		});
	}

	fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {}
}

impl UserData for DisplayPanel {
	fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
		fields.add_field_method_get("input", |_, this| {
			Ok(TerminalSide::Input(this.log_id, this.id, this.logd.clone()))
		});
	}
	fn add_methods<F: UserDataMethods<Self>>(methods: &mut F) {
		methods.add_method(
			"add_entry",
			|_, this, (expr, out, text): (AnyUserData, Signal, Option<String>)| {
				let expr = expr.borrow::<DeciderExpression>()?;
				this.logd
					.write()
					.unwrap()
					.add_display_panel_entry(this.id, expr.0, expr.1, expr.2, out, text);
				Ok(())
			},
		);
	}
}

impl UserData for LogicalDesignAPI {
	fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
		fields.add_field_method_get("description", |_, this| {
			Ok(this.logd.read().unwrap().description.clone())
		});
		fields.add_field_method_set("description", |_, this, description: String| {
			this.logd.write().unwrap().set_description(description);
			Ok(())
		});
	}

	fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
		methods.add_method("add_decider", |_, this, _: ()| {
			let id = this.logd.write().unwrap().add_decider();
			Ok(Decider {
				log_id: this.id,
				id,
				logd: this.logd.clone(),
			})
		});
		methods.add_method("add_display_panel", |_, this, _: ()| {
			let id = this.logd.write().unwrap().add_display_panel();
			Ok(DisplayPanel {
				log_id: this.id,
				id,
				logd: this.logd.clone(),
			})
		});
		methods.add_method(
			"add_arithmetic",
			|_, this, args: (AnyUserData, Signal, Option<i32>, Option<i32>)| {
				let expr = args.0.borrow::<ArithmeticExpression>()?;
				let net_left_v = args.2.unwrap_or(3);
				let net_right_v = args.3.unwrap_or(3);
				let net_left = (net_left_v & 1 > 0, net_left_v & 2 > 0);
				let net_right = (net_right_v & 1 > 0, net_right_v & 2 > 0);
				let out = args.1;
				let id = this.logd.write().unwrap().add_arithmetic_with_net(
					(expr.0, expr.1, expr.2),
					out,
					net_left,
					net_right,
				);
				Ok(Arithmetic {
					log_id: this.id,
					id,
					logd: this.logd.clone(),
				})
			},
		);
		methods.add_method("add_lamp", |_, this, args: (AnyUserData,)| {
			let expr = args.0.borrow::<DeciderExpression>()?;
			let id = this
				.logd
				.write()
				.unwrap()
				.add_lamp((expr.0, expr.1, expr.2));
			Ok(Lamp {
				log_id: this.id,
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
				let id = this.logd.write().unwrap().add_constant(sigs, counts);
				Ok(Constant {
					log_id: this.id,
					id,
					logd: this.logd.clone(),
				})
			},
		);
		methods.add_method("print", |_, this, _: ()| {
			println!("{}", this.logd.read().unwrap());
			Ok(())
		});
		methods.add_method_mut("make_svg", |_, this, _: ()| {
			this.make_svg = true;
			Ok(())
		});
		methods.add_method_mut("group_io", |_, this, _: ()| {
			this.group_io = true;
			Ok(())
		});
		methods.add_method("new_simulation", |_, this, _: ()| {
			Ok(SimStateAPI {
				log_id: this.id,
				logd: this.logd.clone(),
				sim: SimRef::new(RwLock::new(SimState::new(this.logd.clone()))),
			})
		});
		methods.add_method("find_out_port", |_, this, name: String| {
			Ok(this
				.logd
				.read()
				.unwrap()
				.get_out_port_node(name)
				.map(|id| Lamp {
					log_id: this.id,
					id,
					logd: this.logd.clone(),
				}))
		});
		methods.add_method("find_in_port", |_, this, name: String| {
			Ok(this
				.logd
				.read()
				.unwrap()
				.get_in_port_node(name)
				.map(|id| Constant {
					log_id: this.id,
					id,
					logd: this.logd.clone(),
				}))
		});
		methods.add_method("in_ports", |_, this, _: ()| {
			Ok(this
				.logd
				.read()
				.unwrap()
				.get_in_port_nodes()
				.into_iter()
				.map(|(name, id)| {
					(
						name,
						Constant {
							log_id: this.id,
							id,
							logd: this.logd.clone(),
						},
					)
				})
				.collect::<HashM<String, Constant>>())
		});
		methods.add_method("out_ports", |_, this, _: ()| {
			Ok(this
				.logd
				.read()
				.unwrap()
				.get_out_port_nodes()
				.into_iter()
				.map(|(name, id)| {
					(
						name,
						Lamp {
							log_id: this.id,
							id,
							logd: this.logd.clone(),
						},
					)
				})
				.collect::<HashM<String, Lamp>>())
		});
		methods.add_meta_method(MetaMethod::ToString, |_, this, _: ()| {
			Ok(format!("{}", this.logd.read().unwrap()))
		});
		methods.add_method("make_phy", |_, this, input_file: Option<String>| {
			let logd = this;
			let mut phyd = PhysicalDesign::new();
			phyd.set_group_io(logd.group_io);
			phyd.build_from(&logd.logd.read().unwrap());
			if let Some(input_file) = input_file {
				let svg_name = get_derivative_file_name(input_file.clone(), ".svg")
					.map_err(|_| mlua::Error::runtime("failed to make svg name."))?;
				phyd.save_svg(&logd.logd.read().unwrap(), svg_name)
					.map_err(|_| mlua::Error::runtime("failed to make svg."))?;
			}
			let ret = PhysicalDesignAPI {
				log_id: logd.id,
				logd: LogDRef::new(RwLock::new(logd.logd.read().unwrap().clone())),
				phyd: PhyDRef::new(RwLock::new(phyd)),
			};
			Ok(ret)
		});
	}
}

fn verify_is_combinator(_this: &SimStateAPI, data: &Value) -> Result<NodeId, mlua::Error> {
	let nodeid = match data {
		Value::Integer(i) => NodeId(*i as usize),
		Value::UserData(data) => {
			let (nodeid, _logd) = if let Ok(x) = data.borrow::<Decider>() {
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
			nodeid
		},
		_ => {
			return Err(Error::runtime("Got non-combinator as an input."));
		},
	};
	//if logd.as_ptr() != this.logd.as_ptr() {
	//	return Err(Error::runtime(
	//		"Supplied a combinator that doesn't belong to this design.",
	//	));
	//}
	Ok(nodeid)
}

impl UserData for SimStateAPI {
	fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
		methods.add_method("step", |_, this, steps: u32| {
			this.sim.write().unwrap().step(steps);
			Ok(())
		});
		methods.add_method("probe", |_, this, data: AnyUserData| {
			let signals: Vec<(i32, i32)> = if let Ok(term) = data.borrow::<Terminal>() {
				let (log_id, nodeid, _logd, colour) = term.get();
				if this.log_id != log_id {
					return Err(Error::runtime(
						"Supplied a combinator that doesn't belong to this design.",
					));
				}
				let net = match colour {
					WireColour::Red => NET_RED,
					WireColour::Green => NET_GREEN,
				};
				match term.0 {
					TerminalSide::Input(_, _, _) => {
						this.sim.write().unwrap().probe_input(nodeid, net)
					},
					TerminalSide::Output(_, _, _) => {
						this.sim.write().unwrap().probe_red_out(nodeid)
					},
				}
			} else if let Ok(term_side) = data.borrow::<TerminalSide>() {
				let (log_id, nodeid, _logd) = term_side.clone().get();
				if this.log_id != log_id {
					return Err(Error::runtime(
						"Supplied a combinator that doesn't belong to this design.",
					));
				}
				match *term_side {
					TerminalSide::Input(_, _, _) => {
						this.sim.write().unwrap().probe_input(nodeid, NET_RED_GREEN)
					},
					TerminalSide::Output(_, _, _) => {
						this.sim.write().unwrap().probe_red_out(nodeid)
					},
				}
			} else {
				return Err(Error::runtime("Passed an invalid type into probe."));
			};
			let mut ret = SignalTable::default();
			for s in signals {
				ret.signals.insert(Signal::Id(s.0), s.1);
			}
			Ok(ret)
		});
		methods.add_method("add_trace", |_, this, data: Value| {
			let nodeid = verify_is_combinator(this, &data)?;
			this.sim.write().unwrap().add_trace(nodeid);
			Ok(())
		});
		methods.add_method("probe_lamp_state", |_, this, data: AnyUserData| {
			let nodeid = verify_is_combinator(this, &Value::UserData(data.clone()))?;
			if !data.is::<Lamp>() {
				return Err(Error::runtime("Tried to probe a non-lamp as a lamp."));
			}
			Ok(this.sim.read().unwrap().probe_lamp_state(nodeid))
		});
		methods.add_method("save_svg", |_, this, filename: String| {
			let svg = this.sim.read().unwrap().render_traces();
			svg.save(filename)?;
			Ok(())
		});
		methods.add_method("print", |_, this, _: ()| {
			this.sim.read().unwrap().print();
			Ok(())
		});
		methods.add_meta_method(MetaMethod::ToString, |_, this, _: ()| {
			Ok(format!("{}", this.sim.read().unwrap()))
		});
		methods.add_method("inspect", |_, this, _: ()| {
			this.sim.write().unwrap().inspect();
			Ok(())
		});

		methods.add_method_mut(
			"apply_vcd",
			|_,
			 this,
			 (filename, inputs_lua, outputs_lua, propagation_delay, reset): (
				String,
				Table,
				Table,
				u32,
				bool,
			)| {
				let file = File::open(filename)?;
				let reader = BufReader::new(file);
				let vcd = VCD::import(reader);

				let inputs = {
					let mut inputs = hash_map();
					for pair in inputs_lua.pairs::<String, Constant>() {
						let (net, comb) = pair?;
						if !vcd.has_var(&net) {
							return runtime_err(format!("{net} is not a known input variable."));
						}
						//if comb.logd.as_ptr() != this.logd.as_ptr() {
						//	return runtime_err(format!(
						//		"Input combinator for net {net} is not owned by this design."
						//	));
						//}
						inputs.insert(net, comb.id);
					}
					inputs
				};
				let outputs = {
					let mut outputs = hash_map();
					let logd = this.logd.read().unwrap();
					for pair in outputs_lua.pairs::<String, Lamp>() {
						let (net, comb) = pair?;
						if !vcd.has_var(&net) {
							println!("WARN: {net} is not a known output variable in VCD.");
							continue;
						}
						//if comb.logd.as_ptr() != this.logd.as_ptr() {
						//	return runtime_err(format!(
						//		"Output lamp for net {net} is not owned by this design."
						//	));
						//}
						let lamp = logd.get_node(comb.id);
						let signal = match lamp.function {
							crate::logical_design::NodeFunction::Lamp { expression } => {
								expression.0
							},
							_ => return runtime_err("internal error"),
						};
						outputs.insert(net, (signal, comb.id));
					}
					outputs
				};
				Ok(this.sim.write().unwrap().apply_vcd(
					&vcd,
					inputs,
					outputs,
					propagation_delay,
					reset,
				))
			},
		);

		methods.add_method_mut(
			"apply_snapshot",
			|_, this, (filename, reset): (String, bool)| {
				let trace = sim::snapshot::load_trace(filename);
				Ok(this.sim.write().unwrap().apply_trace(&trace, reset))
			},
		);
	}
}

impl FromLua for Constant {
	fn from_lua(value: Value, _lua: &Lua) -> mlua::Result<Self> {
		match value {
			Value::UserData(d) => Ok(d.borrow::<Self>()?.clone()),
			_ => Err(Error::FromLuaConversionError {
				from: value.type_name(),
				to: "Constant".to_owned(),
				message: None,
			}),
		}
	}
}

impl FromLua for Lamp {
	fn from_lua(value: Value, _lua: &Lua) -> mlua::Result<Self> {
		match value {
			Value::UserData(d) => Ok(d.borrow::<Self>()?.clone()),
			_ => Err(Error::FromLuaConversionError {
				from: value.type_name(),
				to: "Lamp".to_owned(),
				message: None,
			}),
		}
	}
}

impl FromLua for Arithmetic {
	fn from_lua(value: Value, _lua: &Lua) -> mlua::Result<Self> {
		match value {
			Value::UserData(d) => Ok(d.borrow::<Self>()?.clone()),
			_ => Err(Error::FromLuaConversionError {
				from: value.type_name(),
				to: "Arithmetic".to_owned(),
				message: None,
			}),
		}
	}
}

impl FromLua for Decider {
	fn from_lua(value: Value, _lua: &Lua) -> mlua::Result<Self> {
		match value {
			Value::UserData(d) => Ok(d.borrow::<Self>()?.clone()),
			_ => Err(Error::FromLuaConversionError {
				from: value.type_name(),
				to: "Decider".to_owned(),
				message: None,
			}),
		}
	}
}

impl UserData for RTL {
	fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
		methods.add_method("to_design", |_, this: &RTL, _: ()| method_map_rtl(this));
		methods.add_method_mut("set_net_promotion", |_, this: &mut RTL, status: bool| {
			this.promote_all_nets_to_ports = status;
			Ok(())
		});
		methods.add_method(
			"yosys_sim",
			|_, _this: &RTL, (fin, fout): (String, String)| method_sim_yosys(&fin, &fout),
		);
	}
}

impl FromLua for RTL {
	fn from_lua(value: Value, _lua: &Lua) -> mlua::Result<Self> {
		match &value {
			Value::Table(t) => {
				let mut filenames = vec![];
				for pair in t.pairs::<u32, String>() {
					let (_idx, file) = pair?;
					filenames.push(file);
				}
				method_load_rtl(&filenames, "top".to_owned(), None)
			},
			Value::String(filename) => {
				method_load_rtl(&[filename.to_string_lossy()], "top".to_owned(), None)
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

impl UserData for SignalTable {
	fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
		methods.add_meta_method(MetaMethod::Eq, |_, this, other: Value| match other {
			Value::UserData(data) => {
				if let Ok(data) = data.borrow::<SignalTable>() {
					Ok(*this == *data)
				} else {
					Ok(false)
				}
			},
			_ => Ok(false),
		});
		methods.add_meta_method(MetaMethod::Index, |_, this, idx: Signal| {
			Ok(this.signals.get(&idx).copied().unwrap_or_default())
		});
		methods.add_meta_method(MetaMethod::ToString, |_, this, _: ()| {
			Ok(format!("{:?}", this))
		});
	}
}

impl FromLua for SignalTable {
	fn from_lua(value: Value, _: &Lua) -> mlua::Result<Self> {
		match value {
			Value::Table(table) => {
				let mut ret = SignalTable::default();
				for pair in table.pairs() {
					let (key, value): (Signal, i32) = pair?;
					ret.signals.insert(key, value);
				}
				Ok(ret)
			},
			Value::UserData(data) => data.borrow::<Self>().map(|v| v.clone()),
			_ => Err(Error::FromLuaConversionError {
				from: value.type_name(),
				to: "SignalTable".to_string(),
				message: None,
			}),
		}
	}
}

impl UserData for VCD {
	fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
		methods.add_meta_method("get_value", |_, this, (var, time): (String, u64)| {
			Ok(sim::convert_to_signal_count(&this.get_value(var, time)))
		});
	}
}

impl UserData for PhysicalDesignAPI {
	fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
		methods.add_method("save_svg", |_, this, filename: String| {
			this.phyd
				.read()
				.unwrap()
				.save_svg(&this.logd.read().unwrap(), filename)?;
			Ok(())
		});
		methods.add_method("save_blueprint", |_, this, filename: String| {
			let mut serd = SerializableDesign::new();
			let logd = this.logd.read().unwrap();
			let phyd = this.phyd.read().unwrap();
			serd.build_from(phyd.deref(), logd.deref());
			let blueprint_json = serde_json::to_string(&serd).unwrap();
			std::fs::write(filename, blueprint_json)?;
			Ok(())
		});
		methods.add_method(
			"save_json",
			|_, this, (phy_filename, logd_filename): (String, String)| {
				//
				let logd = this.logd.read().unwrap();
				let phyd = this.phyd.read().unwrap();
				let logd = serde_json::to_string(logd.deref())
					.map_err(|_| Error::runtime("Can't serialize"))?;
				let phyd = serde_json::to_string(phyd.deref())
					.map_err(|_| Error::runtime("Can't serialize"))?;
				std::fs::write(logd_filename, logd)?;
				std::fs::write(phy_filename, phyd)?;
				Ok(())
			},
		);
	}
}

impl FromLua for PhysicalDesignAPI {
	fn from_lua(value: Value, _: &Lua) -> mlua::Result<Self> {
		match value {
			Value::UserData(data) => Ok(data.borrow::<Self>()?.clone()),
			_ => Err(Error::FromLuaConversionError {
				from: value.type_name(),
				to: "PhysicalDesignAPI".to_owned(),
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
				id: ID_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed),
				logd: LogDRef::new(RwLock::new(LogicalDesign::new())),
				make_svg: false,
				group_io: false,
			})
		})?,
	)?;

	lua.globals().set(
		"load_mapped_rtl",
		lua.create_function(|_, file: String| method_load_mapped_rtl(&file))?,
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
		lua.create_function(
			|_, (files, top_mod, include_dirs): (Value, String, Option<String>)| match files {
				Value::String(filename) => {
					let filename = filename.to_string_lossy();
					if filename.ends_with(".json") {
						Ok(RTL {
							filename: filename.into(),
							top_mod,
							promote_all_nets_to_ports: false,
						})
					} else {
						method_load_rtl(&[filename], top_mod, include_dirs)
					}
				},
				Value::Table(table) => {
					let mut filenames = vec![];
					for pair in table.pairs::<u32, String>() {
						let (_idx, file) = pair?;
						filenames.push(file);
					}
					method_load_rtl(&filenames, top_mod, include_dirs)
				},
				_ => Err(Error::runtime(
					"Must be a single filename or a list of filenames.",
				)),
			},
		)?,
	)?;

	lua.globals().set(
		"yosys_map_rtl",
		lua.create_function(|_, rtl: RTL| method_map_rtl(&rtl))?,
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
						line.push('\n');
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

	lua.globals().set(
		"make_ensemble",
		lua.create_function(|_lua, _: ()| {
			//
			let ret = PhysicalEnsembleAPI {
				root: PhysicalDesignAPI {
					log_id: ID_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed),
					logd: LogDRef::new(RwLock::new(LogicalDesign::new())),
					phyd: PhyDRef::new(RwLock::new(PhysicalDesign::new())),
				},
				offsets_log: hash_map(),
				offsets_phy: hash_map(),
			};
			Ok(ret)
		})?,
	)?;

	lua.globals().set(
		"load_design",
		lua.create_function(|_, (phy_filename, logd_filename): (String, String)| {
			let file = File::open(phy_filename)?;
			let reader = BufReader::new(file);
			let phyd: PhysicalDesign =
				serde_json::from_reader(reader).map_err(|_| Error::runtime("Can't deserialize"))?;
			let file = File::open(logd_filename)?;
			let reader = BufReader::new(file);
			let logd: LogicalDesign =
				serde_json::from_reader(reader).map_err(|_| Error::runtime("Can't deserialize"))?;
			let phyd = PhysicalDesignAPI {
				log_id: ID_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed),
				logd: Arc::new(RwLock::new(logd)),
				phyd: Arc::new(RwLock::new(phyd)),
			};
			let logd = LogicalDesignAPI {
				id: phyd.log_id,
				logd: phyd.logd.clone(),
				make_svg: false,
				group_io: false,
			};
			Ok((phyd, logd))
		})?,
	)?;

	Ok(lua)
}

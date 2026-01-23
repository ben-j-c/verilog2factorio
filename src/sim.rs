use std::{
	fmt::Display,
	ops::{BitAnd, BitOr, BitXor, Index, IndexMut, Shl, Shr},
	sync::{Arc, RwLock},
	usize,
};

use ::vcd::Value;
use itertools::Itertools;
use rayon::{
	iter::{
		IndexedParallelIterator, IntoParallelIterator, IntoParallelRefIterator,
		IntoParallelRefMutIterator, ParallelIterator,
	},
	slice::{ParallelSlice, ParallelSliceMut},
};

mod decider_model;
pub mod snapshot;
pub mod tui;
pub mod vcd;

use crate::{
	logical_design::{
		ArithmeticOperator, LogicalDesign, Node, NodeFunction, NodeId, Signal, WireColour,
		NET_RED_GREEN,
	},
	mapped_design::Direction,
	signal_lookup_table::{self},
	sim::{snapshot::GameTrace, vcd::VCD},
	svg::SVG,
	util::{hash_map, hash_set, HashM},
};

#[derive(Debug, Clone)]
pub(crate) struct WireNetwork {
	pub(crate) fanin: Vec<NodeId>,
	pub(crate) fanout: Vec<NodeId>,
	pub(crate) wires: Vec<NodeId>,
	pub(crate) colour: WireColour,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NetId(pub usize);

impl Default for NetId {
	fn default() -> Self {
		Self(usize::MAX)
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
struct NetMapEntry {
	input_red: Option<NetId>,
	output_red: Option<NetId>,
	input_green: Option<NetId>,
	output_green: Option<NetId>,
}

impl NetMapEntry {
	pub(crate) fn input(&self, colour: WireColour) -> &Option<NetId> {
		match colour {
			WireColour::Red => &self.input_red,
			WireColour::Green => &self.input_green,
		}
	}

	pub(crate) fn input_mut(&mut self, colour: WireColour) -> &mut Option<NetId> {
		match colour {
			WireColour::Red => &mut self.input_red,
			WireColour::Green => &mut self.input_green,
		}
	}

	#[allow(dead_code)]
	pub(crate) fn output(&self, colour: WireColour) -> &Option<NetId> {
		match colour {
			WireColour::Red => &self.output_red,
			WireColour::Green => &self.output_green,
		}
	}

	pub(crate) fn output_mut(&mut self, colour: WireColour) -> &mut Option<NetId> {
		match colour {
			WireColour::Red => &mut self.output_red,
			WireColour::Green => &mut self.output_green,
		}
	}
}

pub struct Trace {
	red: Vec<(usize, Vec<(i32, i32)>)>,
	green: Vec<(usize, Vec<(i32, i32)>)>,
}

pub struct SimState {
	/// Indexed by network id
	network: Vec<WireNetwork>,
	/// Indexed by Logical id
	network_ownership: Vec<NetId>,
	logd: Arc<std::sync::RwLock<LogicalDesign>>,
	step_number: usize,
	/// Indexed by Logical id
	state: Vec<SimStateRow>,
	first_wire_red: Vec<NodeId>,
	first_wire_green: Vec<NodeId>,

	traces: Vec<Trace>,
	trace_set: Vec<NodeId>,

	new_state_red: Vec<OutputState>,
	new_state_green: Vec<OutputState>,
}

#[derive(Debug, Default, Clone)]
pub struct SimStateRow {
	netmap: NetMapEntry,
	red: OutputState,
	green: OutputState,
}

impl SimState {
	pub fn new(logd: Arc<RwLock<LogicalDesign>>) -> Self {
		let mut ret = Self {
			network: vec![],
			network_ownership: vec![],
			logd,
			step_number: 0,
			state: vec![],
			traces: vec![],
			trace_set: vec![],
			new_state_green: vec![],
			new_state_red: vec![],
			first_wire_red: vec![],
			first_wire_green: vec![],
		};
		ret.update_logical_design();
		ret
	}

	pub fn update_logical_design(&mut self) {
		let logd = self.logd.read().unwrap();
		let state_len = self.state.len();
		for _ in logd.nodes.iter().skip(state_len) {
			self.network_ownership.push(NetId::default());
			self.state.push(SimStateRow::default());
			self.traces.push(Trace {
				red: vec![],
				green: vec![],
			});
			self.first_wire_red.push(NodeId(usize::MAX));
			self.first_wire_green.push(NodeId(usize::MAX));
		}
		for (nodeid, node) in logd.nodes.iter().enumerate().skip(state_len) {
			let colour = if let NodeFunction::WireSum(colour) = node.function {
				colour
			} else {
				continue;
			};
			if let Some(_existing_network) = self.state[nodeid].netmap.input(colour) {
				continue;
			}
			let network_wires = logd.get_wire_network(node.id);
			match colour {
				WireColour::Red => {
					self.first_wire_red[nodeid] = network_wires.first().unwrap().clone();
				},
				WireColour::Green => {
					self.first_wire_green[nodeid] = network_wires.first().unwrap().clone();
				},
			}

			assert!(
				!network_wires.is_empty(),
				"A \"wire\" network was found with no wires."
			);
			assert!(
				network_wires.contains(&NodeId(nodeid)),
				"Wire not in it's own network!"
			);
			let network_id = NetId(self.network.len());
			for wire in &network_wires {
				*self.state[wire.0].netmap.input_mut(colour) = Some(network_id);
				*self.state[wire.0].netmap.output_mut(colour) = Some(network_id);
			}
			let mut fanin = hash_set();
			let mut fanout = hash_set();
			for wire in &network_wires {
				let wire_node = &logd.nodes[wire.0];
				for fiid in wire_node.iter_fanin(colour) {
					fanin.insert(*fiid);
					*self.state[fiid.0].netmap.input_mut(colour) = Some(network_id);
				}
				for foid in wire_node.iter_fanout(colour) {
					fanout.insert(*foid);
					*self.state[foid.0].netmap.output_mut(colour) = Some(network_id);
				}
			}
			for wire in &network_wires {
				if self.network_ownership[wire.0] != NetId::default() {
					panic!("Seen the same wire in multiple different networks");
				}
				self.network_ownership[wire.0] = network_id;
			}
			self.network.push(WireNetwork {
				fanin: fanin.into_iter().sorted().collect_vec(),
				fanout: fanout.into_iter().sorted().collect_vec(),
				wires: network_wires,
				colour,
			});
		}
		for (nodeid, _node) in logd.nodes.iter().enumerate().skip(state_len) {
			let output_red_entry = self.state[nodeid].netmap.output_red;
			if output_red_entry.is_some() {
				let first_wire = self.network[output_red_entry.unwrap().0]
					.wires
					.first()
					.unwrap()
					.0;
				self.first_wire_red[nodeid] = NodeId(first_wire);
			}
			let output_green_entry = self.state[nodeid].netmap.output_green;
			if output_green_entry.is_some() {
				let first_wire = self.network[output_green_entry.unwrap().0]
					.wires
					.first()
					.unwrap()
					.0;
				self.first_wire_green[nodeid] = NodeId(first_wire);
			}
		}
	}

	pub fn add_trace(&mut self, node: NodeId) {
		if self.trace_set.contains(&node) {
			return;
		}
		self.trace_set.push(node);
		self.traces[node.0] = Trace {
			red: vec![(self.step_number, self.probe_red_out(node))],
			green: vec![(self.step_number, self.probe_green_out(node))],
		};
	}

	pub fn probe_out_state_red(&self, id: NodeId) -> &OutputState {
		&self.state[id.0].red
	}

	pub fn probe_red_out(&self, id: NodeId) -> Vec<(i32, i32)> {
		let state = &self.state[id.0].red;
		let red = state
			.data
			.iter()
			.map(|(k, v)| (*k, *v))
			.filter(|(_k, v)| *v != 0)
			.sorted_by(|a, b| a.0.cmp(&b.0))
			.collect_vec();
		red
	}

	pub fn probe_green_out(&self, id: NodeId) -> Vec<(i32, i32)> {
		let green = self.state[id.0]
			.green
			.data
			.iter()
			.map(|(k, v)| (*k, *v))
			.filter(|(_k, v)| *v != 0)
			.sorted_by(|a, b| a.0.cmp(&b.0))
			.collect_vec();
		green
	}

	pub fn probe_input(&self, id: NodeId, net: (bool, bool)) -> Vec<(i32, i32)> {
		let netmap = &self.state[id.0].netmap;
		let red = if let Some(input_red) = netmap.output_red {
			let wire_id = self.network[input_red.0].wires.first().unwrap();
			if net.0 {
				self.probe_red_out(*wire_id)
			} else {
				vec![]
			}
		} else {
			vec![]
		};
		let green = if let Some(input_green) = netmap.output_green {
			let wire_id = self.network[input_green.0].wires.first().unwrap();
			if net.1 {
				self.probe_green_out(*wire_id)
			} else {
				vec![]
			}
		} else {
			vec![]
		};
		let mut ret = vec![];
		let mut i = 0;
		let mut j = 0;
		while i < red.len() && j < green.len() {
			let r = red[i];
			let g = green[i];
			if r.0 < g.0 {
				ret.push(r);
				i += 1;
			} else if g.0 < r.0 {
				ret.push(g);
				j += 1;
			} else {
				if r.1 + g.1 != 0 {
					ret.push((r.0, r.1 + g.1));
				}
				i += 1;
				j += 1;
			}
		}
		while i < red.len() {
			ret.push(red[i]);
			i += 1;
		}
		while j < green.len() {
			ret.push(green[j]);
			j += 1;
		}
		ret
	}

	pub fn probe_input_state(&self, id: NodeId) -> (OutputState, OutputState) {
		let netmap = &self.state[id.0].netmap;
		let red = if let Some(input_red) = netmap.output_red {
			let wire_id = self.network[input_red.0].wires.first().unwrap();
			self.state[wire_id.0].red.clone()
		} else {
			OutputState::default()
		};
		let green = if let Some(input_green) = netmap.output_green {
			let wire_id = self.network[input_green.0].wires.first().unwrap();
			self.state[wire_id.0].green.clone()
		} else {
			OutputState::default()
		};
		(red, green)
	}

	pub fn probe_lamp_state(&self, id: NodeId) -> Option<bool> {
		let logd = self.logd.read().unwrap();
		let expr = match &logd.get_node(id).function {
			NodeFunction::Lamp { expression } => expression,
			_ => return None,
		};
		Some(self.evaluate_decider_condition(
			logd.get_node(id),
			expr,
			&NET_RED_GREEN,
			&NET_RED_GREEN,
			None,
			None,
		))
	}

	fn capture_trace(&mut self) {
		for nodeid in &self.trace_set {
			let new_capture_red = self.probe_red_out(*nodeid);
			let new_capture_green = self.probe_green_out(*nodeid);
			self.traces[nodeid.0]
				.red
				.push((self.step_number, new_capture_red));
			self.traces[nodeid.0]
				.green
				.push((self.step_number, new_capture_green));
		}
	}

	pub fn step(&mut self, steps: u32) {
		self.update_logical_design();
		let n_nodes = self.logd.read().unwrap().nodes.len();
		let mut new_state_red = vec![];
		let mut new_state_green = vec![];
		let mut network_states = vec![];
		self.new_state_red.resize(n_nodes, OutputState::default());
		self.new_state_green.resize(n_nodes, OutputState::default());
		std::mem::swap(&mut self.new_state_red, &mut new_state_red);
		std::mem::swap(&mut self.new_state_green, &mut new_state_green);

		for _ in 0..steps {
			self.compute_combs(&mut new_state_red, &mut new_state_green);
			self.compute_nets(
				&mut new_state_red,
				&mut new_state_green,
				&mut network_states,
			);

			for (idx, row) in self.state.iter_mut().enumerate() {
				std::mem::swap(&mut row.red, &mut new_state_red[idx]);
				std::mem::swap(&mut row.green, &mut new_state_green[idx]);
			}
			self.step_number += 1;
			self.capture_trace();
			for row in &mut new_state_red {
				row.data.clear();
			}
			for row in &mut new_state_green {
				row.data.clear();
			}
		}
		std::mem::swap(&mut self.new_state_red, &mut new_state_red);
		std::mem::swap(&mut self.new_state_green, &mut new_state_green);
	}

	fn execute_arith_op(left: i32, op: ArithmeticOperator, right: i32) -> i32 {
		match op {
			ArithmeticOperator::Mult => left.overflowing_mul(right).0,
			ArithmeticOperator::Div => left.checked_div(right).unwrap_or(0),
			ArithmeticOperator::Add => left.overflowing_add(right).0,
			ArithmeticOperator::Sub => left.overflowing_sub(right).0,
			ArithmeticOperator::Mod => left.checked_rem(right).unwrap_or(0),
			ArithmeticOperator::Exp => left.overflowing_pow(right as u32).0,
			ArithmeticOperator::Shl => left.overflowing_shl((right as u32) & 0x1F).0,
			ArithmeticOperator::Sshr => left.overflowing_shr((right as u32) & 0x1F).0,
			ArithmeticOperator::And => left.bitand(right),
			ArithmeticOperator::Or => left.bitor(right),
			ArithmeticOperator::Xor => left.bitxor(right),
		}
	}

	fn get_seen_signal_count(&self, node: NodeId, id: i32, colours: (bool, bool)) -> i32 {
		let mut ret = 0;
		let output_red_entry = self.state[node.0].netmap.output_red;
		if colours.0 && output_red_entry.is_some() {
			let first_wire = self.first_wire_red[node.0].0;
			ret += self.state[first_wire].red[id];
		}
		let output_green_entry = self.state[node.0].netmap.output_green;
		if colours.1 && output_green_entry.is_some() {
			let first_wire = self.first_wire_green[node.0].0;
			ret += self.state[first_wire].green[id];
		}
		ret
	}

	fn get_seen_output_state(&self, node: NodeId, colours: (bool, bool)) -> OutputState {
		let mut ret = OutputState::default();
		let output_red_entry = self.state[node.0].netmap.output_red;
		if colours.0 && output_red_entry.is_some() {
			let first_wire = self.network[output_red_entry.unwrap().0]
				.wires
				.first()
				.unwrap()
				.0;

			ret = self.state[first_wire].red.clone();
		}
		let output_green_entry = self.state[node.0].netmap.output_green;
		if colours.1 && output_green_entry.is_some() {
			let first_wire = self.network[output_green_entry.unwrap().0]
				.wires
				.first()
				.unwrap()
				.0;
			for (id, count) in &self.state[first_wire].green.data {
				ret[*id] += count;
			}
		}
		ret
	}

	fn get_seen_signal_count_any(&self, node: NodeId, colours: (bool, bool)) -> (i32, i32) {
		let mut ret_red = 0;
		let mut id_red = -1;
		let output_red_entry = self.state[node.0].netmap.output_red;
		let output_green_entry = self.state[node.0].netmap.output_green;
		if colours.0 && output_red_entry.is_some() {
			let first_wire = self.network[output_red_entry.unwrap().0]
				.wires
				.first()
				.unwrap()
				.0;
			if !self.state[first_wire].red.data.is_empty() {
				let (sig_id, count) = self.state[first_wire]
					.red
					.data
					.iter()
					.min_by(|a, b| a.0.cmp(&b.0))
					.unwrap();
				ret_red = *count;
				id_red = *sig_id;
			}
		}
		let mut ret_green = 0;
		let mut id_green = -1;
		if colours.1 && output_green_entry.is_some() {
			let first_wire = self.network[output_green_entry.unwrap().0]
				.wires
				.first()
				.unwrap()
				.0;
			if !self.state[first_wire].green.data.is_empty() {
				let (sig_id, count) = self.state[first_wire]
					.green
					.data
					.iter()
					.min_by(|a, b| a.0.cmp(&b.0))
					.unwrap();
				ret_green = *count;
				id_green = *sig_id;
			}
		}
		if id_green == id_red {
			(ret_green + ret_red, id_green)
		} else if id_green != -1 && id_green < id_red {
			(ret_green, id_green)
		} else {
			(ret_red, id_red)
		}
	}

	fn compute_arithmetic_comb(
		&self,
		node: &Node,
		new_state_red: &mut OutputState,
		new_state_green: &mut OutputState,
	) {
		let (op, input_1, input_2, input_left_network, input_right_network) =
			node.function.unwrap_arithmetic();
		assert!(input_1 != Signal::Anything);
		assert!(input_1 != Signal::Everything);
		assert!(input_2 != Signal::Anything);
		assert!(input_2 != Signal::Everything);
		assert!(node.output.len() == 1);
		let output = *node.output.first().unwrap();
		if output == Signal::None {
			return;
		}
		let mut output = if let Signal::Id(output) = output {
			output
		} else if Signal::Each == output {
			0
		} else {
			panic!("Invalid output setting");
		};
		let output_each = *node.output.first().unwrap() == Signal::Each;

		match (input_1 == Signal::Each, input_2 == Signal::Each) {
			(true, true) => {
				let left = self.get_seen_output_state(node.id, input_left_network);
				let right = self.get_seen_output_state(node.id, input_right_network);
				let mut union = hash_set();
				union.extend(left.keys());
				union.extend(right.keys());
				for sig_id in union {
					if output_each {
						output = sig_id;
					}
					let res = Self::execute_arith_op(left[sig_id], op, right[sig_id]);
					if res != 0 {
						new_state_red[output] += res;
						new_state_green[output] += res;
					}
				}
			},
			(true, false) => {
				let left = self.get_seen_output_state(node.id, input_left_network);
				let right = if let Signal::Id(id) = input_2 {
					self.get_seen_signal_count(node.id, id, input_right_network)
				} else if let Signal::Constant(c) = input_2 {
					c
				} else {
					0
				};
				for sig_id in left.keys() {
					if output_each {
						output = sig_id;
					}
					let res = Self::execute_arith_op(left[sig_id], op, right);
					if res != 0 {
						new_state_red[output] += res;
						new_state_green[output] += res;
					}
				}
			},
			(false, true) => {
				let left = if let Signal::Id(id) = input_1 {
					self.get_seen_signal_count(node.id, id, input_left_network)
				} else if let Signal::Constant(c) = input_1 {
					c
				} else {
					0
				};
				let right = self.get_seen_output_state(node.id, input_right_network);
				for sig_id in right.keys() {
					if output_each {
						output = sig_id;
					}
					let res = Self::execute_arith_op(left, op, right[sig_id]);
					if res != 0 {
						new_state_red[output] += res;
						new_state_green[output] += res;
					}
				}
			},
			(false, false) => {
				assert!(!output_each, "Invalid output");
				let left = if let Signal::Id(id) = input_1 {
					self.get_seen_signal_count(node.id, id, input_left_network)
				} else if let Signal::Constant(c) = input_1 {
					c
				} else {
					0
				};
				let right = if let Signal::Id(id) = input_2 {
					self.get_seen_signal_count(node.id, id, input_right_network)
				} else if let Signal::Constant(c) = input_2 {
					c
				} else {
					0
				};
				let res = Self::execute_arith_op(left, op, right);
				if res != 0 {
					new_state_red[output] += res;
					new_state_green[output] += res;
				}
			},
		}
	}

	fn do_compute_step(
		&self,
		node: &Node,
		new_state_red: &mut OutputState,
		new_state_green: &mut OutputState,
	) {
		match &node.function {
			NodeFunction::Arithmetic { .. } => {
				self.compute_arithmetic_comb(node, new_state_red, new_state_green);
			},
			NodeFunction::Decider { .. } => {
				self.compute_decider_comb(node, new_state_red, new_state_green);
			},
			NodeFunction::Constant { enabled, constants } => {
				if !*enabled {
					return;
				}
				for (signal, count) in node.output.iter().zip(constants) {
					if *count == 0 {
						continue;
					}
					if let Signal::Id(sig_id) = *signal {
						new_state_red[sig_id] = *count;
						new_state_green[sig_id] = *count;
					} else {
						assert!(false, "Constant combinator with id {} is trying to drive an invalid signal type", node.id.0);
					}
				}
			},
			NodeFunction::Lamp { .. } => {},
			NodeFunction::WireSum(_wire_colour) => {},
			NodeFunction::DisplayPanel { .. } => {},
		}
	}

	pub fn compute_combs(
		&self,
		new_state_red: &mut Vec<OutputState>,
		new_state_green: &mut Vec<OutputState>,
	) {
		let logd = self.logd.read().unwrap();

		#[cfg(debug_assertions)]
		logd.nodes
			.iter()
			.zip(new_state_red.iter_mut().zip(new_state_green.iter_mut()))
			.for_each(|(node, (new_state_red, new_state_green))| {
				self.do_compute_step(node, new_state_red, new_state_green);
			});

		#[cfg(not(debug_assertions))]
		{
			let chunk_size = 512;
			logd.nodes
				.par_iter()
				.zip(
					new_state_red
						.par_iter_mut()
						.zip(new_state_green.par_iter_mut()),
				)
				.with_min_len(chunk_size)
				.for_each(|(node, (new_state_red, new_state_green))| {
					self.do_compute_step(node, new_state_red, new_state_green);
				});
		}
	}

	#[cfg(true)]
	pub fn compute_nets(
		&self,
		new_state_red: &mut Vec<OutputState>,
		new_state_green: &mut Vec<OutputState>,
		network_states: &mut Vec<OutputState>,
	) {
		network_states.resize(self.network.len(), OutputState::default());
		let chunk_size = 4096;
		network_states
			.par_iter_mut()
			.with_min_len(chunk_size)
			.enumerate()
			.for_each(|(idx, new_state)| {
				new_state.clear();
				let net = &self.network[idx];
				for fiid in &net.fanin {
					let fanin_state = match net.colour {
						WireColour::Red => &new_state_red[fiid.0],
						WireColour::Green => &new_state_green[fiid.0],
					};
					for (sigid, x) in fanin_state.data.iter() {
						new_state[*sigid] += x;
					}
				}
			});

		let chunk_size = 512;
		self.network_ownership
			.par_iter()
			.zip(
				new_state_red
					.par_iter_mut()
					.zip(new_state_green.par_iter_mut()),
			)
			.with_min_len(chunk_size)
			.for_each(|(netid, (new_state_red, new_state_green))| {
				if *netid == NetId::default() {
					return;
				}
				let net = &self.network[netid.0];
				let wire_state = match net.colour {
					WireColour::Red => new_state_red,
					WireColour::Green => new_state_green,
				};
				for (sigid, x) in network_states[netid.0].data.iter() {
					wire_state[*sigid] = *x
				}
			});
	}

	#[cfg(false)]
	pub fn compute_nets(
		&self,
		new_state_red: &mut Vec<OutputState>,
		new_state_green: &mut Vec<OutputState>,
		network_states: &mut Vec<OutputState>,
	) {
		network_states.resize(self.network.len(), OutputState::default());
		let chunk_size = 128;
		network_states
			.chunks_mut(chunk_size)
			.enumerate()
			.for_each(|(idx, new_state)| {
				for i in 0..new_state.len() {
					let new_state = &mut new_state[i];
					new_state.clear();
					let idx = idx * chunk_size + i;
					let net = &self.network[idx];
					for fiid in &net.fanin {
						let fanin_state = match net.colour {
							WireColour::Red => &new_state_red[fiid.0],
							WireColour::Green => &new_state_green[fiid.0],
						};
						for (sigid, x) in fanin_state.data.iter() {
							new_state[*sigid] += x;
						}
					}
				}
			});

		let chunk_size = 16;
		self.network_ownership
			.chunks(chunk_size)
			.zip(
				new_state_red
					.chunks_mut(chunk_size)
					.zip(new_state_green.chunks_mut(chunk_size)),
			)
			.for_each(|(netid, (new_state_red, new_state_green))| {
				for i in 0..netid.len() {
					let netid = netid[i];
					let new_state_red = &mut new_state_red[i];
					let new_state_green = &mut new_state_green[i];
					if netid == NetId::default() {
						continue;
					}
					let net = &self.network[netid.0];
					let wire_state = match net.colour {
						WireColour::Red => new_state_red,
						WireColour::Green => new_state_green,
					};
					for (sigid, x) in network_states[netid.0].data.iter() {
						wire_state[*sigid] = *x
					}
				}
			});
	}

	pub fn print(&self) {
		let logd = self.logd.read().unwrap();
		println!("--------------");
		println!("RED: (signal_id, count)");
		for node_num in 0..self.state.len() {
			if let Some(descr) = &logd.get_node(NodeId(node_num)).description {
				println!("  {descr} ({node_num})");
			} else {
				println!("  Node {}", node_num);
			}

			let mut no_print = true;
			print!("    ");
			for (id, v) in &self.state[node_num].red.data {
				print!("({}, {}) ", id, v);
				no_print = false;
			}
			if no_print {
				print!("-- No signals --");
			}
			println!();
		}
		println!("GREEN: (signal_id, count)");
		for node_num in 0..self.state.len() {
			if let Some(descr) = &logd.get_node(NodeId(node_num)).description {
				println!("  {descr} ({node_num})");
			} else {
				println!("  Node {}", node_num);
			}
			let mut no_print = true;
			print!("    ");
			for (id, v) in &self.state[node_num].green.data {
				print!("({}, {}) ", id, v);
				no_print = false;
			}
			if no_print {
				print!("-- No signals --");
			}
			println!();
		}
		println!("Nes:\n {:?}", self.network);
		println!("--------------");
	}

	pub fn render_traces(&self) -> SVG {
		let mut svg = SVG::new();
		const TICK_SIZE: i32 = 50;
		const TICK_CORNER: i32 = 8;
		const TRACE_HEIGHT: i32 = 25;
		const TRACE_SPACING_INTRA: i32 = 6;
		const TRACE_SPACING_INTER: i32 = 25;

		let mut max_step = 0;
		let mut max_height = 0;
		let mut unique_red = vec![];
		let mut unique_green = vec![];
		for id in &self.trace_set {
			let trace = &self.traces[id.0];
			let mut unique_ids_red = hash_set();
			let mut unique_ids_green = hash_set();
			for (step, values) in &trace.red {
				max_step = max_step.max(*step);
				for (sig_id, _) in values {
					unique_ids_red.insert(*sig_id);
				}
			}
			for (step, values) in &trace.green {
				max_step = max_step.max(*step);
				for (sig_id, _) in values {
					unique_ids_green.insert(*sig_id);
				}
			}
			max_height += (unique_ids_red.len() as i32 + unique_ids_green.len() as i32)
				* (TRACE_HEIGHT + TRACE_SPACING_INTRA)
				+ TRACE_SPACING_INTER;
			unique_red.push(unique_ids_red);
			unique_green.push(unique_ids_green);
		}
		let mut y = 100;
		let x = 400;
		// Background
		svg.add_rect(
			0,
			0,
			max_step as i32 * TICK_SIZE + x,
			max_height + y,
			(255, 255, 255),
			None,
			None,
		);
		for tick_bar in (0..(max_step + 5)).step_by(5) {
			svg.add_rect(
				x + tick_bar as i32 * TICK_SIZE,
				y - TRACE_SPACING_INTRA - TRACE_HEIGHT,
				TICK_SIZE,
				TRACE_HEIGHT,
				(230, 230, 230),
				Some(format!("{tick_bar}")),
				None,
			);
			svg.add_line(
				x + tick_bar as i32 * TICK_SIZE,
				y - TRACE_SPACING_INTRA - TRACE_HEIGHT,
				x + tick_bar as i32 * TICK_SIZE,
				y + max_height,
				None,
				None,
			);
		}
		//Title
		svg.add_rect(
			0,
			0,
			200,
			25,
			(200, 200, 200),
			Some("Simulation".to_owned()),
			None,
		);
		for (idx, nodeid) in self.trace_set.iter().enumerate() {
			let y1 = y;
			let trace = &self.traces[nodeid.0];
			let unique_ids_red = unique_red[idx].iter().cloned().sorted().collect_vec();
			for sig_id in &unique_ids_red {
				let mut map_last = hash_map();
				svg.add_rect(
					x / 2 + 1,
					y + TRACE_SPACING_INTRA / 2,
					x / 2 - 2,
					TRACE_HEIGHT,
					(200, 200, 200),
					Some(format!(
						"{} ({})",
						signal_lookup_table::lookup_str(*sig_id).0,
						sig_id
					)),
					None,
				);
				for (step, values) in &trace.red {
					let mut map = HashM::from_iter(values.iter().cloned());
					let last = map_last.get(sig_id).cloned();
					if !map.contains_key(sig_id) {
						map.insert(*sig_id, 0);
					}
					draw_trace_tick(
						&mut svg,
						last,
						map[sig_id],
						x + *step as i32 * TICK_SIZE,
						y,
						TICK_SIZE,
						TRACE_HEIGHT,
						TICK_CORNER,
						WireColour::Red,
						*step as i32,
					);
					map_last = map;
				}
				y += TRACE_HEIGHT + TRACE_SPACING_INTRA;
			}
			let unique_ids_green = unique_green[idx].iter().cloned().sorted().collect_vec();
			for sig_id in &unique_ids_green {
				let mut map_last = hash_map();
				svg.add_rect(
					x / 2 + 1,
					y + TRACE_SPACING_INTRA / 2,
					x / 2 - 2,
					TRACE_HEIGHT,
					(200, 200, 200),
					Some(format!(
						"{} ({})",
						signal_lookup_table::lookup_str(*sig_id).0,
						sig_id
					)),
					None,
				);
				for (step, values) in &trace.green {
					let mut map = HashM::from_iter(values.iter().cloned());
					let last = map_last.get(sig_id).cloned();
					if !map.contains_key(sig_id) {
						map.insert(*sig_id, 0);
					}
					draw_trace_tick(
						&mut svg,
						last,
						map[sig_id],
						x + *step as i32 * TICK_SIZE,
						y,
						TICK_SIZE,
						TRACE_HEIGHT,
						TICK_CORNER,
						WireColour::Green,
						*step as i32,
					);

					map_last = map;
				}
				y += TRACE_HEIGHT + TRACE_SPACING_INTRA;
			}
			let y2 = y;
			let text = {
				match &self.logd.read().unwrap().get_node(*nodeid).description {
					Some(desc) => desc.clone(),
					None => format!("{:?}", nodeid),
				}
			};
			svg.add_rect(1, y1, x / 2 - 2, y2 - y1, (200, 200, 200), Some(text), None);
			y += TRACE_SPACING_INTER;
		}
		svg
	}

	pub fn print_row(&self, idx: usize) {
		println!("{idx} {}", self.state[idx]);
	}

	pub fn get_attached(&self, idx: usize) -> (Vec<NodeId>, Vec<NodeId>) {
		for net in &self.network {
			if net.wires.contains(&NodeId(idx)) {
				return (net.fanin.clone(), net.fanout.clone());
			}
		}
		(vec![], vec![])
	}

	pub fn apply_vcd(
		&mut self,
		vcd: &VCD,
		inputs: HashM<String, NodeId>,
		outputs: HashM<String, (Signal, NodeId)>,
		propagation_delay: u32,
		reset: bool,
	) -> bool {
		if reset {
			self.reset();
		}
		println!(
			"VCD Playback started. {} steps in file.",
			vcd.last_time() - 1
		);
		println!("NNodes: {}", self.logd.read().unwrap().nodes.len());
		let time_start = std::time::Instant::now();
		let mut last_informational_stamp = std::time::Instant::now();
		for vcd_time in 0..vcd.last_time() {
			let now = std::time::Instant::now();
			if now - last_informational_stamp > std::time::Duration::from_secs(2) {
				let steps_per_second = vcd_time as f64 / (now - time_start).as_secs() as f64;
				let steps_remaining = vcd.last_time() - vcd_time - 1;
				println!(
					"{}/{}. ~{:.2} seconds remaining. {:.2} sim-steps per second.",
					vcd_time,
					vcd.last_time() - 1,
					steps_remaining as f64 / steps_per_second,
					steps_per_second * propagation_delay as f64,
				);
				last_informational_stamp = now;
			}
			let mut inputs_snapshot = vec![];
			{
				let mut logd = self.logd.write().unwrap();
				for (wire_name, id) in &inputs {
					let val = vcd.get_value(wire_name, vcd_time);
					let count: i32 = convert_to_signal_count(&val);
					inputs_snapshot.push((wire_name, count));
					logd.set_ith_output_count(*id, 0, count);
				}
			}
			self.step(propagation_delay);
			#[cfg(false)]
			{
				{
					let logd = self.logd.read().unwrap();
					println!("\nSTART-------------------{vcd_time}-----------------------");
					for x in 0..self.state.len() {
						println!("{}", logd.get_node(NodeId(x)));
						self.print_row(x);
					}
				}
				if vcd_time == 14 {
					for i in 0..propagation_delay {
						self.step(1);
						{
							let logd = self.logd.read().unwrap();
							println!("\nSTEP-------------------{i}-----------------------");
							for x in 0..self.state.len() {
								println!("{}", logd.get_node(NodeId(x)));
								self.print_row(x);
							}
						}
					}
				} else {
					self.step(propagation_delay);
				}
				{
					let logd = self.logd.read().unwrap();
					println!("\nEND--------------------{vcd_time}-----------------------");
					for x in 0..self.state.len() {
						println!("{}", logd.get_node(NodeId(x)));
						self.print_row(x);
					}
				}
			}
			{
				let mut good = true;
				for (wire_name, (signal, id)) in &outputs {
					let val = vcd.get_value(wire_name, vcd_time);
					let expected_count: i32 = convert_to_signal_count(&val);
					let seen_signals = self.probe_input(*id, NET_RED_GREEN);

					let mut found = false;
					for (sig, actual_count) in seen_signals.clone() {
						if sig == signal.id() {
							if Self::compare_vcd_val_to_i32(actual_count, &val) {
								found = true;
								continue;
							}
							found = true;
							if good {
								println!("At time {vcd_time}:");
							}
							//println!("{val:?}");
							println!("Expected 0x{expected_count:X}, got 0x{actual_count:X}.");
							println!("\tFound unexpected value for output '{}'", wire_name);
							println!("\tNodeId: {id}");
							good = false;
						}
					}
					if !found && !Self::compare_vcd_val_to_i32(0, &val) {
						if good {
							println!("At time {vcd_time}:");
						}
						println!("Expected 0x{expected_count:x}, got 0.");
						println!("\tFound unexpected value for output '{}'", wire_name);
						println!("\tNodeId: {id}");
						good = false;
					}
				}
				if !good {
					println!("Inputs:");
					for (name, count) in &inputs_snapshot {
						println!("\t{} = 0x{:X}", name, count);
					}
					return false;
				}
			}
		}
		if last_informational_stamp - time_start >= std::time::Duration::from_secs(5) {
			println!("{}/{}", vcd.last_time(), vcd.last_time());
		}
		println!("VCD Playback finished.");
		true
	}

	pub fn apply_trace(&mut self, trace: &GameTrace, reset: bool) -> bool {
		if reset {
			self.reset();
		}
		let t_end = trace.constants.len();
		println!("Snapshot Playback started. {} steps in file.", t_end);
		let mut err = false;
		for t in 0..t_end {
			{
				let mut logd = self.logd.write().unwrap();
				for (id, count) in trace.constants[t].iter() {
					logd.set_ith_output_count(*id, 0, *count);
				}
			}
			self.step(1);
			{
				let logd = self.logd.read().unwrap();
				for (id, expected_state) in trace.states[t].iter() {
					let actual_state = self.probe_out_state_red(*id);

					if *actual_state != *expected_state {
						let (actual_red_in, actual_green_in) = self.probe_input_state(*id);
						let expected_red_in = trace.net_states[t]
							.get(&(*id, WireColour::Red, Direction::Input))
							.cloned()
							.unwrap_or_default();
						let expected_green_in = trace.net_states[t]
							.get(&(*id, WireColour::Green, Direction::Input))
							.cloned()
							.unwrap_or_default();
						if !err {
							println!("Mismatch at time {t}:");
						}
						print!("{}: ", id);
						if actual_red_in == expected_red_in && actual_green_in == expected_green_in
						{
							println!();
							println!(
								"    expected: {:?}",
								expected_state.data.iter().sorted().collect_vec()
							);
							println!(
								"    actual:   {:?}",
								actual_state.data.iter().sorted().collect_vec()
							);
							println!(
								"    input red: {:?}",
								actual_red_in.data.iter().sorted().collect_vec()
							);
							println!(
								"    input green: {:?}",
								actual_green_in.data.iter().sorted().collect_vec()
							);
							println!("    Node: {}", logd.get_node(*id));
						} else {
							println!("Bad input.");
						}

						err = true;
					}
				}
			}
			if err {
				break;
			}
		}
		if err {
			println!("Snapshot Playback failed.");
		} else {
			println!("Snapshot Playback finished.");
		}
		!err
	}

	pub fn compare_vcd_val_to_i32(actual: i32, expected: &Option<Vec<Value>>) -> bool {
		let mut actual: u32 = actual as u32;
		if expected.is_none() {
			return true;
		}
		let expected = expected.as_ref().unwrap().iter().rev();
		for val in expected {
			let bit = actual & 1;
			actual >>= 1;
			if matches!(val, Value::V0) && bit == 1 || matches!(val, Value::V1) && bit == 0 {
				return false;
			}
		}
		actual == 0
	}

	pub fn reset(&mut self) {
		self.step_number = 0;
		for row in self.state.iter_mut() {
			row.red = OutputState::default();
			row.green = OutputState::default();
		}
		self.traces.clear();
		self.trace_set.clear();
	}

	pub fn inspect(&mut self) {
		println!("Valid commands:");
		println!("\tlogic <id> // basic print logical node");
		println!("\tlogd <id> // detailed print logical node");
		println!("\tsim <id> // print sim row");
		println!("\tstep <n> // step the simulation n times");
		println!("\tregex <id> // print sim row");
		println!("\twire_net <id> // prints nodes attached to network");
		println!("\tdump // dump whole sim state");
		println!("\ttoggle // toggle a constant comb.");
		let mut rl = rustyline::DefaultEditor::new().unwrap();
		for readline in rl.iter("> ") {
			match readline {
				Ok(line) => {
					let line = line.trim().to_owned();
					if line.starts_with("log") {
						let detailed = line.starts_with("logd");
						let logd = self.logd.read().unwrap();
						for v in line.split(" ").skip(1) {
							if let Ok(v) = v.parse::<usize>() {
								if detailed {
									println!("{:?}", logd.get_node(NodeId(v)))
								} else {
									println!("{}", logd.get_node(NodeId(v)))
								}
							}
						}
					} else if line.starts_with("sim") {
						for v in line.split(" ").skip(1) {
							if let Ok(v) = v.parse::<usize>() {
								self.print_row(v);
							}
						}
					} else if line.starts_with("regex") {
						let logd = self.logd.read().unwrap();
						let pattern = &line[6..];
						let pattern = match regex::Regex::new(pattern) {
							Ok(v) => v,
							Err(e) => {
								println!("{e}");
								continue;
							},
						};
						let mut found = false;
						logd.for_all(|_, node| {
							let description = if let Some(descr) = &node.description {
								descr
							} else {
								return;
							};

							if pattern.is_match(description) {
								println!("{}: {}", node.id, description);
								found = true;
							}
						});
						if !found {
							println!("<no matches>");
						}
					} else if line.starts_with("step") {
						let mut found = false;
						for v in line.split(" ").skip(1).take(1) {
							found = true;
							if let Ok(v) = v.parse::<u32>() {
								println!("Doing {v} step(s).");
								self.step(v);
							} else {
								println!("Invalid step count.")
							}
						}
						if found {
							continue;
						}
						println!("Doing 1 step.");
						self.step(1);
					} else if line.starts_with("wire") {
						let logd = self.logd.read().unwrap();
						for v in line.split(" ").skip(1) {
							if let Ok(v) = v.parse::<usize>() {
								let (fanin, fanout) = self.get_attached(v);
								println!("fanin of wire {v}");
								for id in fanin {
									println!("\t{}", logd.get_node(id));
								}
								println!("fanout of wire {v}");
								for id in fanout {
									println!("\t{}", logd.get_node(id));
								}
							}
						}
					} else if line.starts_with("dump") {
						let logd = self.logd.read().unwrap();
						for x in 0..self.state.len() {
							println!("{}", logd.get_node(NodeId(x)));
							self.print_row(x);
						}
					} else if line.starts_with("t") {
						for v in line.split(" ").skip(1) {
							if let Ok(v) = v.parse::<usize>() {
								let mut logd = self.logd.write().unwrap();
								let en = !logd.get_constant_enabled(NodeId(v));
								logd.set_constant_enabled(NodeId(v), en);
								println!("Setting {} to {}", v, en);
							}
						}
					}
				},
				Err(err) => {
					println!("Error: {:?}", err);
					break;
				},
			}
		}
	}
}

fn draw_transition_edge(
	svg: &mut SVG,
	x: i32,
	y: i32,
	height: i32,
	corner_size: i32,
	colour: &str,
	level: i32,
) {
	svg.add_line(
		x - corner_size / 2,
		y + height * level,
		x + corner_size / 2,
		y + height * (1 - level),
		Some(colour.to_owned()),
		None,
	);
}

fn draw_signal_level(
	svg: &mut SVG,
	x: i32,
	y: i32,
	width: i32,
	height: i32,
	corner_size: i32,
	colour: &str,
	level: i32,
) {
	svg.add_line(
		x + corner_size / 2,
		y + height * (1 - level),
		x + width - corner_size / 2,
		y + height * (1 - level),
		Some(colour.to_owned()),
		Some(1),
	);
}

fn draw_no_edge(
	svg: &mut SVG,
	x: i32,
	y: i32,
	height: i32,
	corner_size: i32,
	colour: &str,
	level: i32,
) {
	svg.add_line(
		x - corner_size / 2,
		y + height * (1 - level),
		x + corner_size / 2,
		y + height * (1 - level),
		Some(colour.to_owned()),
		Some(1),
	);
}

fn draw_trace_tick(
	svg: &mut SVG,
	last: Option<i32>,
	now: i32,
	x: i32,
	y: i32,
	width: i32,
	height: i32,
	corner_size: i32,
	w_colour: WireColour,
	step_num: i32,
) {
	let colour = match w_colour {
		WireColour::Red => "red",
		WireColour::Green => "green",
	};
	if let Some(last) = last {
		if last == now {
			// No change, so no edges
			if now == 0 || now == 1 {
				draw_no_edge(svg, x, y, height, corner_size, colour, now);
			} else {
				draw_no_edge(svg, x, y, height, corner_size, colour, 0);
				draw_no_edge(svg, x, y, height, corner_size, colour, 1);
			}
		} else {
			// Change
			svg.add_rect(
				x,
				y,
				width,
				height,
				(230, 230, 230),
				Some(format!("{now}")),
				Some(format!("{step_num}")),
			);
			if now == 0 || now == 1 {
				// Single edge
				draw_transition_edge(svg, x, y, height, corner_size, colour, now);
				if last != 0 && last != 1 {
					draw_no_edge(svg, x, y, height, corner_size, colour, now);
				}
			} else {
				// One or two edges
				draw_transition_edge(svg, x, y, height, corner_size, colour, 1);
				if last == 0 || last == 1 {
					draw_no_edge(svg, x, y, height, corner_size, colour, last);
				} else {
					draw_transition_edge(svg, x, y, height, corner_size, colour, 0);
				}
			}
		}
	}
	if now == 0 || now == 1 {
		draw_signal_level(svg, x, y, width, height, corner_size, colour, now);
	} else {
		draw_signal_level(svg, x, y, width, height, corner_size, colour, 0);
		draw_signal_level(svg, x, y, width, height, corner_size, colour, 1);
	}
}

static ZERO: i32 = 0;

#[derive(Debug, Default, Clone)]
pub struct OutputState {
	data: HashM<i32, i32>,
}

impl PartialEq for OutputState {
	fn eq(&self, other: &Self) -> bool {
		self.is_subset(other) && other.is_subset(self)
	}
}

impl Index<i32> for OutputState {
	type Output = i32;

	fn index(&self, index: i32) -> &Self::Output {
		if !self.data.contains_key(&index) {
			return &ZERO;
		}
		self.data.get(&index).unwrap()
	}
}

impl IndexMut<i32> for OutputState {
	fn index_mut(&mut self, index: i32) -> &mut Self::Output {
		self.data.entry(index).or_default()
	}
}

impl Display for SimState {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		for (id, state) in self.state.iter().enumerate() {
			writeln!(f, "{id} {state}")?;
		}
		Ok(())
	}
}

impl OutputState {
	fn clear(&mut self) {
		self.data.clear();
	}

	fn copy(&mut self, other: &Self) {
		self.clear();
		for x in &other.data {
			self.data.insert(*x.0, *x.1);
		}
	}

	fn keys(&self) -> impl Iterator<Item = i32> + '_ {
		self.data.iter().map(|(id, _)| *id)
	}

	fn is_subset(&self, other: &Self) -> bool {
		for (id, count) in &self.data {
			let other_get = other.data.get(id);
			if *count != 0 {
				if other_get != Some(count) {
					return false;
				}
			} else {
				if other_get != Some(&0) && other_get != None {
					return false;
				}
			}
		}
		true
	}
}

impl Display for SimStateRow {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if !self.red.data.is_empty() {
			let mut data = self
				.red
				.data
				.iter()
				.filter(|(_k, v)| **v != 0)
				.collect_vec();
			data.sort_by(|(id1, _), (id2, _)| id1.cmp(id2));
			write!(
				f,
				"R[{}]",
				data.iter()
					.map(|(id, count)| format!("({}: {})", Signal::Id(**id), count))
					.join(", ")
			)?;
		}
		if !self.green.data.is_empty() {
			let mut data = self
				.green
				.data
				.iter()
				.filter(|(_k, v)| **v != 0)
				.collect_vec();
			data.sort_by(|(id1, _), (id2, _)| id1.cmp(id2));
			write!(
				f,
				"G[{}]",
				data.iter()
					.map(|(id, count)| format!("({}: {})", Signal::Id(**id), count))
					.join(", ")
			)?;
		}
		Ok(())
	}
}

pub fn convert_to_signal_count(var: &Option<Vec<Value>>) -> i32 {
	let v = if let Some(v) = var {
		v
	} else {
		return 0;
	};
	let mut retval = 0;
	assert!(v.len() <= 32);
	for bit in v {
		retval <<= 1;
		if matches!(bit, Value::V1) {
			retval += 1;
		}
	}
	retval
}

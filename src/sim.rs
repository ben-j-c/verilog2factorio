use std::{
	fmt::Display,
	ops::{BitAnd, BitOr, BitXor, Index, IndexMut, Shl, Shr},
	sync::{Arc, RwLock},
	usize,
};

use ::vcd::Value;
use itertools::Itertools;
use rayon::iter::{
	IndexedParallelIterator, IntoParallelIterator, IntoParallelRefIterator,
	IntoParallelRefMutIterator, ParallelIterator,
};

mod decider_model;
pub mod vcd;

use crate::{
	logical_design::{
		ArithmeticOperator, LogicalDesign, Node, NodeFunction, NodeId, Signal, WireColour,
		NET_RED_GREEN,
	},
	signal_lookup_table::{self},
	sim::vcd::VCD,
	svg::SVG,
	util::{hash_map, hash_set, HashM},
};

#[derive(Debug, Clone)]
struct WireNetwork {
	fanin: Vec<NodeId>,
	#[allow(dead_code)]
	fanout: Vec<NodeId>,
	wires: Vec<NodeId>,
	colour: WireColour,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NetId(usize);

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
	fn input(&self, colour: WireColour) -> &Option<NetId> {
		match colour {
			WireColour::Red => &self.input_red,
			WireColour::Green => &self.input_green,
		}
	}

	fn input_mut(&mut self, colour: WireColour) -> &mut Option<NetId> {
		match colour {
			WireColour::Red => &mut self.input_red,
			WireColour::Green => &mut self.input_green,
		}
	}

	#[allow(dead_code)]
	fn output(&self, colour: WireColour) -> &Option<NetId> {
		match colour {
			WireColour::Red => &self.output_red,
			WireColour::Green => &self.output_green,
		}
	}

	fn output_mut(&mut self, colour: WireColour) -> &mut Option<NetId> {
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
	network: Vec<WireNetwork>,
	logd: Arc<std::sync::RwLock<LogicalDesign>>,
	step_number: usize,
	state: Vec<SimStateRow>,

	traces: Vec<Trace>,
	trace_set: Vec<NodeId>,
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
			logd,
			step_number: 0,
			state: vec![],
			traces: vec![],
			trace_set: vec![],
		};
		ret.update_logical_design();
		ret
	}

	pub fn update_logical_design(&mut self) {
		let logd = self.logd.read().unwrap();
		let state_len = self.state.len();
		for _ in logd.nodes.iter().skip(state_len) {
			self.state.push(SimStateRow::default());
			self.traces.push(Trace {
				red: vec![],
				green: vec![],
			});
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
			self.network.push(WireNetwork {
				fanin: fanin.into_iter().sorted().collect_vec(),
				fanout: fanout.into_iter().sorted().collect_vec(),
				wires: network_wires,
				colour,
			});
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
		for _ in 0..steps {
			let mut new_state_red = vec![OutputState::default(); n_nodes];
			let mut new_state_green = vec![OutputState::default(); n_nodes];

			self.compute_combs(&mut new_state_red, &mut new_state_green);
			self.compute_nets(&mut new_state_red, &mut new_state_green);

			for (idx, row) in self.state.iter_mut().enumerate() {
				std::mem::swap(&mut row.red, &mut new_state_red[idx]);
				std::mem::swap(&mut row.green, &mut new_state_green[idx]);
			}
			self.step_number += 1;
			self.capture_trace();
		}
	}

	fn execute_arith_op(left: i32, op: ArithmeticOperator, right: i32) -> i32 {
		match op {
			ArithmeticOperator::Mult => left.wrapping_mul(right),
			ArithmeticOperator::Div => left.checked_div(right).unwrap_or(0),
			ArithmeticOperator::Add => left.wrapping_add(right),
			ArithmeticOperator::Sub => left.wrapping_sub(right),
			ArithmeticOperator::Mod => left.checked_rem(right).unwrap_or(0),
			ArithmeticOperator::Exp => left.checked_pow(right as u32).unwrap_or(0),
			ArithmeticOperator::Shl => left.shl(right as u32),
			ArithmeticOperator::Sshr => left.shr(right as u32),
			ArithmeticOperator::And => left.bitand(right),
			ArithmeticOperator::Or => left.bitor(right),
			ArithmeticOperator::Xor => left.bitxor(right),
		}
	}

	fn get_seen_signal_count(&self, node: NodeId, id: i32, colours: (bool, bool)) -> i32 {
		let mut ret = 0;
		let output_red_entry = self.state[node.0].netmap.output_red;
		if colours.0 && output_red_entry.is_some() {
			let first_wire = self.network[output_red_entry.unwrap().0]
				.wires
				.first()
				.unwrap()
				.0;
			ret += self.state[first_wire].red[id];
		}
		let output_green_entry = self.state[node.0].netmap.output_green;
		if colours.1 && output_green_entry.is_some() {
			let first_wire = self.network[output_green_entry.unwrap().0]
				.wires
				.first()
				.unwrap()
				.0;
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
					.min_by(|a, b| a.0.cmp(b.0))
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
					.min_by(|a, b| a.0.cmp(b.0))
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
				union.extend(left.data.keys().cloned());
				union.extend(right.data.keys().cloned());
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
				} else {
					0
				};
				for sig_id in left.data.keys() {
					if output_each {
						output = *sig_id;
					}
					let res = Self::execute_arith_op(left[*sig_id], op, right);
					if res != 0 {
						new_state_red[output] += res;
						new_state_green[output] += res;
					}
				}
			},
			(false, true) => {
				let left = if let Signal::Id(id) = input_1 {
					self.get_seen_signal_count(node.id, id, input_left_network)
				} else {
					0
				};
				let right = self.get_seen_output_state(node.id, input_right_network);
				for sig_id in right.data.keys() {
					if output_each {
						output = *sig_id;
					}
					let res = Self::execute_arith_op(left, op, right[*sig_id]);
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
		let iter = logd
			.nodes
			.par_iter()
			.zip(
				new_state_red
					.par_iter_mut()
					.zip(new_state_green.par_iter_mut()),
			)
			.chunks(4096)
			.into_par_iter()
			.for_each(|mut c| {
				c.iter_mut()
					.for_each(|(node, (new_state_red, new_state_green))| {
						self.do_compute_step(*node, *new_state_red, *new_state_green);
					});
			});
	}

	pub fn compute_nets(
		&self,
		new_state_red: &mut Vec<OutputState>,
		new_state_green: &mut Vec<OutputState>,
	) {
		for net in &self.network {
			let mut new_state = OutputState::default();
			for fiid in &net.fanin {
				let fanin_state = match net.colour {
					WireColour::Red => &mut new_state_red[fiid.0],
					WireColour::Green => &mut new_state_green[fiid.0],
				};
				for (sigid, x) in fanin_state.data.iter() {
					new_state[*sigid] += x;
				}
			}
			for wireid in &net.wires {
				let wire_state = match net.colour {
					WireColour::Red => &mut new_state_red[wireid.0],
					WireColour::Green => &mut new_state_green[wireid.0],
				};
				for (sigid, x) in new_state.data.iter() {
					wire_state[*sigid] = *x
				}
			}
		}
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
			if now - last_informational_stamp > std::time::Duration::from_secs(5) {
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
			{
				for (wire_name, (signal, id)) in &outputs {
					let val = vcd.get_value(wire_name, vcd_time);
					if let Some(v) = &val {
						if v.iter().any(|b| matches!(b, Value::X | Value::Z)) {
							println!("WARN: {wire_name} has an X or Z. This can match anything. Regs should be initialized to 0.");
							continue;
						}
					}
					let expected_count: i32 = convert_to_signal_count(&val);
					let seen_signals = self.probe_input(*id, NET_RED_GREEN);
					for (sig, actual_count) in seen_signals {
						if sig == signal.id() {
							if actual_count == expected_count {
								continue;
							}
							println!("At time {vcd_time}:");
							println!("Expected {expected_count}, got {actual_count}.");
							println!("Found unexpected value for output '{}'", wire_name);
							println!("NodeId: {id}");
							println!("Inputs:");
							for (name, count) in &inputs_snapshot {
								println!("\t{} = {}", name, count);
							}
							return false;
						}
					}
				}
			}
		}
		if last_informational_stamp - time_start >= std::time::Duration::from_secs(5) {
			println!("{}/{}", vcd.last_time(), vcd.last_time());
		}
		println!("VCD Playback finished.");
		true
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

impl Display for SimStateRow {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if !self.red.data.is_empty() {
			let mut data = self.red.data.iter().collect_vec();
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
			let mut data = self.green.data.iter().collect_vec();
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

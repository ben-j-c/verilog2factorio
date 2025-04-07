use std::{cell::RefCell, fmt::format, os::linux::net, rc::Rc, usize};

use itertools::Itertools;

mod decider_model;

use decider_model::*;

use crate::{
	logical_design::{
		ArithmeticOperator, DeciderOperator, LogicalDesign, Node, NodeFunction, NodeId, Signal,
		WireColour,
	},
	ndarr::Arr2,
	signal_lookup_table::{self, n_ids},
	svg::SVG,
	util::{hash_map, hash_set, HashM, HashS},
};

#[derive(Debug, Clone)]
struct WireNetwork {
	fanin: Vec<NodeId>,
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
	netmap: Vec<NetMapEntry>,
	logd: Rc<RefCell<LogicalDesign>>,
	step_number: usize,
	state_red: Arr2<i32>,
	state_green: Arr2<i32>,

	traces: Vec<Trace>,
	trace_set: Vec<NodeId>,
}

impl SimState {
	pub fn new(logd: Rc<RefCell<LogicalDesign>>) -> Self {
		let mut ret = Self {
			network: vec![],
			netmap: vec![],
			logd,
			step_number: 0,
			state_red: Arr2::new([0, signal_lookup_table::n_ids() as usize]),
			state_green: Arr2::new([0, signal_lookup_table::n_ids() as usize]),
			traces: vec![],
			trace_set: vec![],
		};
		ret.update_logical_design();
		ret
	}

	pub fn update_logical_design(&mut self) {
		let logd = self.logd.borrow();
		let netmap_len = self.netmap.len();
		for _ in logd.nodes.iter().skip(netmap_len) {
			self.state_red.extend_dim0(1);
			self.state_green.extend_dim0(1);
			self.netmap.push(NetMapEntry::default());
			self.traces.push(Trace {
				red: vec![],
				green: vec![],
			});
		}
		for (nodeid, node) in logd.nodes.iter().enumerate().skip(netmap_len) {
			let colour = if let NodeFunction::WireSum(colour) = node.function {
				colour
			} else {
				continue;
			};
			if let Some(_existing_network) = self.netmap[nodeid].input(colour) {
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
				*self.netmap[wire.0].input_mut(colour) = Some(network_id);
				*self.netmap[wire.0].output_mut(colour) = Some(network_id);
			}
			let mut fanin = hash_set();
			let mut fanout = hash_set();
			for wire in &network_wires {
				let wire_node = &logd.nodes[wire.0];
				for fiid in wire_node.iter_fanin(colour) {
					fanin.insert(*fiid);
					*self.netmap[fiid.0].input_mut(colour) = Some(network_id);
				}
				for foid in wire_node.iter_fanout(colour) {
					fanout.insert(*foid);
					*self.netmap[foid.0].output_mut(colour) = Some(network_id);
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
			red: vec![(self.step_number, self.probe_red_output_sparse(node))],
			green: vec![(self.step_number, self.probe_green_output_sparse(node))],
		};
	}

	pub fn probe_red_output(&self, id: NodeId) -> &[i32] {
		&self.state_red[id.0]
	}

	pub fn probe_green_output(&self, id: NodeId) -> &[i32] {
		&self.state_green[id.0]
	}

	pub fn probe_red_output_sparse(&self, id: NodeId) -> Vec<(i32, i32)> {
		let mut ret = vec![];
		for i in 0..self.state_red.dims().1 {
			if self.state_red[id.0][i] != 0 {
				ret.push((i as i32, self.state_red[id.0][i]));
			}
		}
		ret
	}

	pub fn probe_green_output_sparse(&self, id: NodeId) -> Vec<(i32, i32)> {
		let mut ret = vec![];
		for i in 0..self.state_green.dims().1 {
			if self.state_green[id.0][i] != 0 {
				ret.push((i as i32, self.state_green[id.0][i]));
			}
		}
		ret
	}

	pub fn probe_lamp_state(&self, id: NodeId) -> bool {
		todo!()
	}

	fn capture_trace(&mut self) {
		for nodeid in &self.trace_set {
			let new_capture_red = self.probe_red_output_sparse(*nodeid);
			let new_capture_green = self.probe_green_output_sparse(*nodeid);
			self.traces[nodeid.0]
				.red
				.push((self.step_number, new_capture_red));
			self.traces[nodeid.0]
				.green
				.push((self.step_number, new_capture_green));
		}
	}

	pub fn step(&mut self, steps: usize) {
		self.update_logical_design();
		let n_nodes = self.logd.borrow().nodes.len();
		for _ in 0..steps {
			let mut new_state_red = Arr2::new([n_nodes, signal_lookup_table::n_ids() as usize]);
			let mut new_state_green = Arr2::new([n_nodes, signal_lookup_table::n_ids() as usize]);

			self.compute_combs(&mut new_state_red, &mut new_state_green);
			self.compute_nets(&mut new_state_red, &mut new_state_green);

			self.state_red = new_state_red;
			self.state_green = new_state_green;
			self.step_number += 1;
			self.capture_trace();
		}
	}

	fn execute_arith_op(left: i32, op: ArithmeticOperator, right: i32) -> i32 {
		match op {
			ArithmeticOperator::Mult => left * right,
			ArithmeticOperator::Div => left / right,
			ArithmeticOperator::Add => left + right,
			ArithmeticOperator::Sub => left - right,
			ArithmeticOperator::Mod => left % right,
			ArithmeticOperator::Exp => left.checked_pow(right as u32).unwrap_or(0),
			ArithmeticOperator::Sll => left << right,
			ArithmeticOperator::Srl => left >> right,
			ArithmeticOperator::And => left & right,
			ArithmeticOperator::Or => left | right,
			ArithmeticOperator::Xor => left ^ right,
		}
	}

	fn get_seen_signal_count(&self, node: NodeId, id: i32, colours: (bool, bool)) -> i32 {
		let mut ret = 0;
		let output_red_entry = self.netmap[node.0].output_red;
		if colours.0 && output_red_entry.is_some() {
			let first_wire = self.network[output_red_entry.unwrap().0]
				.wires
				.first()
				.unwrap()
				.0;
			ret += self.state_red[first_wire][id as usize];
		}
		let output_green_entry = self.netmap[node.0].output_green;
		if colours.1 && output_green_entry.is_some() {
			let first_wire = self.network[output_green_entry.unwrap().0]
				.wires
				.first()
				.unwrap()
				.0;
			ret += self.state_green[first_wire][id as usize];
		}
		ret
	}

	fn get_seen_signal_count_any(&self, node: NodeId, colours: (bool, bool)) -> (i32, i32) {
		let mut ret_red = 0;
		let mut id_red = -1;
		let output_red_entry = self.netmap[node.0].output_red;
		if colours.0 && output_red_entry.is_some() {
			let first_wire = self.network[output_red_entry.unwrap().0]
				.wires
				.first()
				.unwrap()
				.0;
			for id in 0..n_ids() as usize {
				if self.state_red[first_wire][id] != 0 {
					ret_red = self.state_red[first_wire][id];
					id_red = id as i32;
					break;
				}
			}
		}
		let mut ret_green = 0;
		let mut id_green = -1;
		let output_green_entry = self.netmap[node.0].output_green;
		if colours.1 && output_green_entry.is_some() {
			let first_wire = self.network[output_green_entry.unwrap().0]
				.wires
				.first()
				.unwrap()
				.0;
			for id in 0..n_ids() as usize {
				if self.state_red[first_wire][id] != 0 {
					ret_green = self.state_green[first_wire][id];
					id_green = id as i32;
					break;
				}
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
		new_state_red: &mut Arr2<i32>,
		new_state_green: &mut Arr2<i32>,
	) {
		let n_ids = signal_lookup_table::n_ids();
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

		if *node.output.first().unwrap() == Signal::Each {
			match (input_1 == Signal::Each, input_2 == Signal::Each) {
				(true, true) => {
					for id in 0..n_ids {
						let left = self.get_seen_signal_count(node.id, id, input_left_network);
						let right = self.get_seen_signal_count(node.id, id, input_right_network);
						new_state_red[node.id.0][id as usize] =
							Self::execute_arith_op(left, op, right);
						new_state_green[node.id.0][id as usize] =
							new_state_red[node.id.0][id as usize];
					}
				}
				(true, false) => {
					let right = if let Signal::Id(id) = input_2 {
						self.get_seen_signal_count(node.id, id, input_right_network)
					} else {
						0
					};
					for id in 0..n_ids {
						let left = self.get_seen_signal_count(node.id, id, input_left_network);
						new_state_red[node.id.0][id as usize] =
							Self::execute_arith_op(left, op, right);
						new_state_green[node.id.0][id as usize] =
							new_state_red[node.id.0][id as usize]
					}
				}
				(false, true) => {
					let left = if let Signal::Id(id) = input_1 {
						self.get_seen_signal_count(node.id, id, input_left_network)
					} else {
						0
					};
					for id in 0..n_ids {
						let right = self.get_seen_signal_count(node.id, id, input_right_network);
						new_state_red[node.id.0][id as usize] =
							Self::execute_arith_op(left, op, right);
						new_state_green[node.id.0][id as usize] =
							new_state_red[node.id.0][id as usize]
					}
				}
				(false, false) => panic!("Invalid output setting"),
			}
			return;
		}
		// Else we got a single output and we need to sum
		let output = if let Signal::Id(output) = output {
			output
		} else {
			panic!("Invalid output setting");
		};
		match (input_1 == Signal::Each, input_2 == Signal::Each) {
			(true, true) => {
				for id in 0..n_ids {
					let left = self.get_seen_signal_count(node.id, id, input_left_network);
					let right = self.get_seen_signal_count(node.id, id, input_right_network);
					new_state_red[node.id.0][output as usize] +=
						Self::execute_arith_op(left, op, right);
					new_state_green[node.id.0][output as usize] =
						new_state_red[node.id.0][output as usize]
				}
			}
			(true, false) => {
				let right = if let Signal::Id(id) = input_2 {
					self.get_seen_signal_count(node.id, id, input_right_network)
				} else {
					0
				};
				for id in 0..n_ids {
					let left = self.get_seen_signal_count(node.id, id, input_left_network);
					new_state_red[node.id.0][output as usize] +=
						Self::execute_arith_op(left, op, right);
					new_state_green[node.id.0][output as usize] =
						new_state_red[node.id.0][output as usize]
				}
			}
			(false, true) => {
				let left = if let Signal::Id(id) = input_1 {
					self.get_seen_signal_count(node.id, id, input_left_network)
				} else {
					0
				};
				for id in 0..n_ids {
					let right = self.get_seen_signal_count(node.id, id, input_right_network);
					new_state_red[node.id.0][output as usize] +=
						Self::execute_arith_op(left, op, right);
					new_state_green[node.id.0][output as usize] =
						new_state_red[node.id.0][output as usize];
				}
			}
			(false, false) => {
				let left = if let Signal::Id(id) = input_1 {
					self.get_seen_signal_count(node.id, id, input_left_network)
				} else {
					0
				};
				let right = if let Signal::Id(id) = input_2 {
					self.get_seen_signal_count(node.id, id, input_right_network)
				} else {
					0
				};
				new_state_red[node.id.0][output as usize] = Self::execute_arith_op(left, op, right);
				new_state_green[node.id.0][output as usize] =
					new_state_red[node.id.0][output as usize];
			}
		}
	}

	pub fn compute_combs(&self, new_state_red: &mut Arr2<i32>, new_state_green: &mut Arr2<i32>) {
		let n_ids = signal_lookup_table::n_ids();
		let logd = self.logd.borrow();
		for node in &logd.nodes {
			match &node.function {
				NodeFunction::Arithmetic { .. } => {
					self.compute_arithmetic_comb(node, new_state_red, new_state_green);
				}
				NodeFunction::Decider { .. } => {
					self.compute_decider_comb(node, new_state_red, new_state_green);
				}
				NodeFunction::Constant { enabled, constants } => {
					if !*enabled {
						continue;
					}
					for (signal, count) in node.output.iter().zip(constants) {
						if let Signal::Id(sig_id) = *signal {
							new_state_red[node.id.0][sig_id as usize] = *count;
							new_state_green[node.id.0][sig_id as usize] = *count;
						} else {
							assert!(false, "Constant combinator with id {} is trying to drive an invalid signal type", node.id.0);
						}
					}
				}
				NodeFunction::Lamp { .. } => continue,
				NodeFunction::WireSum(_wire_colour) => continue,
			}
		}
	}

	pub fn compute_nets(&self, new_state_red: &mut Arr2<i32>, new_state_green: &mut Arr2<i32>) {
		let n_ids = signal_lookup_table::n_ids() as usize;
		for net in &self.network {
			let mut new_state = vec![0; n_ids];
			for fiid in &net.fanin {
				let fanin_state = match net.colour {
					WireColour::Red => &mut new_state_red[fiid.0],
					WireColour::Green => &mut new_state_green[fiid.0],
				};
				for (sigid, x) in fanin_state.iter().enumerate() {
					new_state[sigid] += x;
				}
			}
			for wireid in &net.wires {
				let wire_state = match net.colour {
					WireColour::Red => &mut new_state_red[wireid.0],
					WireColour::Green => &mut new_state_green[wireid.0],
				};
				for (sigid, x) in new_state.iter().enumerate() {
					wire_state[sigid] = *x
				}
			}
		}
	}

	fn print(&self) {
		println!("--------------");
		println!("RED: (signal_id, count)");
		for node_num in 0..self.state_red.dims().0 {
			println!("Node {}", node_num);
			let mut no_print = true;
			for id in 0..self.state_red.dims().1 {
				if self.state_red[node_num][id] > 0 {
					print!("({}, {}) ", id, self.state_red[node_num][id]);
					no_print = false;
				}
			}
			if no_print {
				print!("-- No signals --");
			}
			println!("");
		}
		println!("GREEN: (signal_id, count)");
		for node_num in 0..self.state_green.dims().0 {
			println!("Node {}", node_num);
			let mut no_print = true;
			for id in 0..self.state_red.dims().1 {
				if self.state_green[node_num][id] > 0 {
					print!("({}, {}) ", id, self.state_green[node_num][id]);
					no_print = false;
				}
			}
			if no_print {
				print!("-- No signals --");
			}
			println!("");
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
			svg.add_rect(
				1,
				y1,
				x / 2 - 2,
				y2 - y1,
				(200, 200, 200),
				Some(format!("{:?}", nodeid)),
				None,
			);
			y += TRACE_SPACING_INTER;
		}
		svg
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

#[cfg(test)]
mod test {
	use std::{cell::RefCell, rc::Rc};

	use super::SimState;
	use crate::logical_design::{
		DeciderOperator, DeciderRowConjDisj, LogicalDesign, Signal, NET_RED_GREEN,
	};

	#[test]
	fn new() {
		let logd = Rc::new(RefCell::new(LogicalDesign::new()));
		let nop = logd.borrow_mut().add_nop_simple();
		let mut sim = SimState::new(logd.clone());
		sim.probe_green_output(nop);
		logd.borrow_mut().nodes[nop.0].description = Some("The description".to_owned());
		sim.step(10);
		logd.borrow_mut().nodes[nop.0].description = Some("The description2".to_owned());
		sim.step(10);
	}

	#[test]
	fn constant_through_nop() {
		let logd = Rc::new(RefCell::new(LogicalDesign::new()));
		let (c1, nop, wire) = {
			let mut logd = logd.borrow_mut();
			let c1 = logd.add_constant_comb(vec![Signal::Id(10)], vec![1234]);
			let nop = logd.add_nop_simple();
			let wire = logd.add_wire_red_simple(c1, nop);
			println!("{:?}", logd);
			(c1, nop, wire)
		};
		let mut sim = SimState::new(logd.clone());
		sim.print();
		for x in sim.probe_red_output(c1) {
			assert_eq!(*x, 0);
		}
		for x in sim.probe_red_output(nop) {
			assert_eq!(*x, 0);
		}
		for x in sim.probe_red_output(wire) {
			assert_eq!(*x, 0);
		}

		sim.step(1);
		sim.print();
		for (id, x) in sim.probe_red_output(c1).iter().enumerate() {
			if id == 10 {
				assert_eq!(*x, 1234);
			} else {
				assert_eq!(*x, 0);
			}
		}
		for x in sim.probe_red_output(nop) {
			assert_eq!(*x, 0);
		}
		for (id, x) in sim.probe_red_output(wire).iter().enumerate() {
			if id == 10 {
				assert_eq!(*x, 1234);
			} else {
				assert_eq!(*x, 0);
			}
		}

		sim.step(1);
		sim.print();
		for (id, x) in sim.probe_red_output(c1).iter().enumerate() {
			if id == 10 {
				assert_eq!(*x, 1234);
			} else {
				assert_eq!(*x, 0);
			}
		}
		for (id, x) in sim.probe_red_output(nop).iter().enumerate() {
			if id == 10 {
				assert_eq!(*x, 1234);
			} else {
				assert_eq!(*x, 0);
			}
		}
		for (id, x) in sim.probe_red_output(wire).iter().enumerate() {
			if id == 10 {
				assert_eq!(*x, 1234);
			} else {
				assert_eq!(*x, 0);
			}
		}
	}

	#[test]
	fn decider_simple() {
		let logd = Rc::new(RefCell::new(LogicalDesign::new()));
		let (c1, d1, wire) = {
			let mut logd = logd.borrow_mut();
			let c1 = logd.add_constant_comb(vec![Signal::Id(10)], vec![1234]);
			let d1 = logd.add_decider_comb();
			let wire = logd.add_wire_red_simple(c1, d1);
			logd.add_decider_comb_input(
				d1,
				(
					Signal::Id(10),
					DeciderOperator::GreaterThan,
					Signal::Constant(1233),
				),
				DeciderRowConjDisj::FirstRow,
				NET_RED_GREEN,
				NET_RED_GREEN,
			);
			logd.add_decider_comb_output(d1, Signal::Id(20), false, NET_RED_GREEN);
			println!("{}", logd);
			(c1, d1, wire)
		};
		let mut sim = SimState::new(logd.clone());
		sim.step(1);
		assert_eq!(sim.probe_red_output(wire)[10], 1234);
		assert_eq!(sim.probe_red_output(c1)[10], 1234);
		sim.step(1);
		sim.print();
		assert_eq!(sim.probe_red_output(wire)[10], 1234);
		assert_eq!(sim.probe_red_output(d1)[10], 0);
		assert_eq!(sim.probe_red_output(d1)[20], 1);
	}

	#[test]
	fn decider_simple2() {
		let logd = Rc::new(RefCell::new(LogicalDesign::new()));
		let (c1, d1, wire) = {
			let mut logd = logd.borrow_mut();
			let c1 = logd.add_constant_comb(vec![Signal::Id(10)], vec![1234]);
			let d1 = logd.add_decider_comb();
			let wire = logd.add_wire_red_simple(c1, d1);
			logd.add_decider_comb_input(
				d1,
				(
					Signal::Id(10),
					DeciderOperator::GreaterThan,
					Signal::Constant(1233),
				),
				DeciderRowConjDisj::FirstRow,
				NET_RED_GREEN,
				NET_RED_GREEN,
			);
			logd.add_decider_comb_output(d1, Signal::Id(20), false, NET_RED_GREEN);
			logd.add_decider_comb_output(d1, Signal::Id(20), false, NET_RED_GREEN);
			println!("{}", logd);
			(c1, d1, wire)
		};
		let mut sim = SimState::new(logd.clone());
		sim.step(1);
		assert_eq!(sim.probe_red_output(wire)[10], 1234);
		assert_eq!(sim.probe_red_output(c1)[10], 1234);
		sim.step(1);
		sim.print();
		assert_eq!(sim.probe_red_output(wire)[10], 1234);
		assert_eq!(sim.probe_red_output(d1)[10], 0);
		assert_eq!(sim.probe_red_output(d1)[20], 2);
	}

	#[test]
	fn decider_simple3() {
		let logd = Rc::new(RefCell::new(LogicalDesign::new()));
		let (c1, d1, wire) = {
			let mut logd = logd.borrow_mut();
			let c1 = logd.add_constant_comb(vec![Signal::Id(10)], vec![1234]);
			let d1 = logd.add_decider_comb();
			let wire = logd.add_wire_red_simple(c1, d1);
			logd.add_decider_comb_input(
				d1,
				(
					Signal::Id(10),
					DeciderOperator::GreaterThan,
					Signal::Constant(1233),
				),
				DeciderRowConjDisj::FirstRow,
				NET_RED_GREEN,
				NET_RED_GREEN,
			);
			logd.add_decider_comb_input(
				d1,
				(
					Signal::Id(10),
					DeciderOperator::LessThan,
					Signal::Constant(1235),
				),
				DeciderRowConjDisj::And,
				NET_RED_GREEN,
				NET_RED_GREEN,
			);
			logd.add_decider_comb_output(d1, Signal::Id(20), false, NET_RED_GREEN);
			logd.add_decider_comb_output(d1, Signal::Id(20), false, NET_RED_GREEN);
			println!("{}", logd);
			(c1, d1, wire)
		};
		let mut sim = SimState::new(logd.clone());
		sim.step(1);
		assert_eq!(sim.probe_red_output(wire)[10], 1234);
		assert_eq!(sim.probe_red_output(c1)[10], 1234);
		sim.step(1);
		sim.print();
		assert_eq!(sim.probe_red_output(wire)[10], 1234);
		assert_eq!(sim.probe_red_output(d1)[10], 0);
		assert_eq!(sim.probe_red_output(d1)[20], 2);
	}

	#[test]
	fn decider_simple4() {
		let logd = Rc::new(RefCell::new(LogicalDesign::new()));
		let (c1, d1, d2, wire, wire2) = {
			let mut logd = logd.borrow_mut();
			let c1 = logd.add_constant_comb(vec![Signal::Id(10)], vec![1234]);
			let d1 = logd.add_decider_comb();
			let d2 = logd.add_decider_comb();
			let wire = logd.add_wire_red(vec![c1], vec![d1, d2]);
			let wire2 = logd.add_wire_red(vec![d1, d2], vec![]);
			logd.add_decider_comb_input(
				d1,
				(
					Signal::Id(10),
					DeciderOperator::GreaterThan,
					Signal::Constant(1233),
				),
				DeciderRowConjDisj::FirstRow,
				NET_RED_GREEN,
				NET_RED_GREEN,
			);
			logd.add_decider_comb_input(
				d2,
				(
					Signal::Id(10),
					DeciderOperator::LessThan,
					Signal::Constant(1235),
				),
				DeciderRowConjDisj::And,
				NET_RED_GREEN,
				NET_RED_GREEN,
			);
			logd.add_decider_comb_output(d1, Signal::Id(20), false, NET_RED_GREEN);
			logd.add_decider_comb_output(d2, Signal::Id(20), false, NET_RED_GREEN);
			println!("{}", logd);
			(c1, d1, d2, wire, wire2)
		};
		let mut sim = SimState::new(logd.clone());
		sim.step(1);
		assert_eq!(sim.probe_red_output(wire)[10], 1234);
		assert_eq!(sim.probe_red_output(c1)[10], 1234);
		sim.step(1);
		sim.print();
		assert_eq!(sim.probe_red_output(wire)[10], 1234);
		assert_eq!(sim.probe_red_output(wire2)[10], 0);
		assert_eq!(sim.probe_red_output(wire2)[20], 2);
	}
}

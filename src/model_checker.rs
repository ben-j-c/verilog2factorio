use std::{
	cell::{Ref, RefCell},
	default,
	os::linux::net,
	path::Display,
	process::Output,
	rc::Rc,
	usize,
};

use itertools::{izip, Itertools};
use metis::option::Opt;

use crate::{
	logical_design::{
		ArithmeticOperator, DeciderOperator, LogicalDesign, Node, NodeFunction, NodeId, Signal,
		WireColour,
	},
	ndarr::Arr2,
	signal_lookup_table,
	util::{hash_map, hash_set, HashM},
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

struct SimState {
	network: Vec<WireNetwork>,
	netmap: Vec<NetMapEntry>,
	logd: Rc<RefCell<LogicalDesign>>,
	step_number: usize,
	state_red: Arr2<i32>,
	state_green: Arr2<i32>,
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
		};
		ret.update_logical_design();
		ret
	}

	pub fn update_logical_design(&mut self) {
		let logd = self.logd.borrow();
		for (nodeid, node) in logd.nodes.iter().enumerate().skip(self.netmap.len()) {
			self.state_red.extend_dim0(1);
			self.state_green.extend_dim0(1);
			self.netmap.push(NetMapEntry::default());
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

	pub fn probe_red_output(&self, id: NodeId) -> &[i32] {
		&self.state_red[id.0]
	}

	pub fn probe_green_output(&self, id: NodeId) -> &[i32] {
		&self.state_green[id.0]
	}

	pub fn probe_lamp_state(&self, id: NodeId) -> bool {
		todo!()
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

	fn execute_decider_op(left: i32, op: DeciderOperator, right: i32) -> bool {
		match op {
			DeciderOperator::LessThan => left < right,
			DeciderOperator::GreaterThan => left > right,
			DeciderOperator::Equal => left == right,
			DeciderOperator::NotEqual => left != right,
			DeciderOperator::GreaterThanEqual => left >= right,
			DeciderOperator::LessThanEqual => left <= right,
		}
	}

	fn evaluate_decider_condition(
		&self,
		node: &Node,
		expr: &(Signal, DeciderOperator, Signal),
		left_network: &(bool, bool),
		right_network: &(bool, bool),
	) -> bool {
		if expr.0 == Signal::Anything {
			todo!()
		} else if expr.0 == Signal::Everything {
			todo!()
		} else if expr.0 == Signal::Each {
			todo!()
		}
		// Must have valid single numerical values.
		let left = if let Signal::Id(id) = expr.0 {
			self.get_seen_signal_count(node.id, id, *left_network)
		} else {
			0
		};
		let right = if let Signal::Id(id) = expr.2 {
			self.get_seen_signal_count(node.id, id, *right_network)
		} else {
			0
		};
		Self::execute_decider_op(left, expr.1, right)
	}

	fn compute_decider_comb(
		&self,
		node: &Node,
		new_state_red: &mut Arr2<i32>,
		new_state_green: &mut Arr2<i32>,
	) {
		let (
			expressions,
			expression_conj_disj,
			input_left_networks,
			input_right_networks,
			output_network,
			use_input_count,
			constants,
		) = node.function.unwrap_decider();
		let n_expr = expressions.len();
		let outp_state = &mut new_state_red[node.id.0];
		let mut sat_or = false;
		let mut and_sat = true;
		for idx in 0..n_expr {
			let sat = self.evaluate_decider_condition(
				node,
				&expressions[idx],
				&input_left_networks[idx],
				&input_right_networks[idx],
			);
		}
		sat_or |= and_sat;
		for out in &node.output {}
		for (id, x) in outp_state.iter().enumerate() {
			new_state_green[node.id.0][id] = *x;
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
		println!("RED:\n{:?}", self.state_red);
		println!("GREEN:\n{:?}", self.state_green);
		println!("Nes:\n{:?}", self.network);
		println!("--------------");
	}
}

#[cfg(test)]
mod test {
	use std::{cell::RefCell, rc::Rc};

	use super::SimState;
	use crate::logical_design::{LogicalDesign, Signal};

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
}

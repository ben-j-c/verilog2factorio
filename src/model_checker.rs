use std::{
	cell::{Ref, RefCell},
	rc::Rc,
};

use itertools::Itertools;
use metis::option::Opt;

use crate::{
	logical_design::{LogicalDesign, NodeFunction, NodeId, Signal, WireColour},
	ndarr::Arr2,
	signal_lookup_table,
	util::{hash_map, hash_set, HashM},
};

struct WireNetwork {
	fanin: Vec<NodeId>,
	fanout: Vec<NodeId>,
	wires: Vec<NodeId>,
	colour: WireColour,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NetId(usize);

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

	pub fn compute_combs(&self, new_state_red: &mut Arr2<i32>, new_state_green: &mut Arr2<i32>) {
		let logd = self.logd.borrow();
		for node in &logd.nodes {
			match &node.function {
				NodeFunction::Arithmetic {
					op,
					input_1,
					input_2,
					input_left_network,
					input_right_network,
				} => {
					todo!()
				}
				NodeFunction::Decider {
					expressions,
					expression_conj_disj,
					input_left_networks,
					input_right_networks,
					output_network,
					use_input_count,
				} => {
					todo!()
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
		let logd = self.logd.borrow();
		for net in &self.network {}
	}
}

#[cfg(test)]
mod test {
	use std::{cell::RefCell, rc::Rc};

	use super::SimState;
	use crate::logical_design::LogicalDesign;

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
}

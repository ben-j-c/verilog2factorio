use std::{
	cell::RefCell,
	collections::{BTreeSet, HashSet, LinkedList},
	vec,
};

const AND: &str = "$and";
const OR: &str = "$or";
const XOR: &str = "$xor";
const SHL: &str = "$shl";
const SHR: &str = "$shr";
const MUL: &str = "$mul";
const DIV: &str = "$div";
const MOD: &str = "$mod";
const POW: &str = "$pow";
const ADD: &str = "$add";
const SUB: &str = "$sub";

macro_rules! IMPLEMENTABLE_OPS {
	() => {
		AND | OR | XOR | SHL | SHR | MUL | DIV | MOD | POW | ADD | SUB
	};
}

type NodeId = usize;

use crate::{
	logical_design::{self, ArithmeticOperator, LogicalDesign},
	mapped_design::Direction,
	signal_lookup_table,
};
use crate::{
	mapped_design::{Bit, MappedDesign},
	signal_lookup_table::lookup_id,
};

pub struct CheckedDesign {
	nodes: Vec<Node>,
	signals: Vec<Option<i32>>,
	#[allow(dead_code)]
	cache: RefCell<CheckedDesignCache>,
}

struct CheckedDesignCache {}

#[derive(Debug, Clone, PartialEq, Eq)]
enum NodeType {
	CellInput { port: String },
	CellOutput { port: String },
	PortInput,
	PortOutput,
	PortBody,
	CellBody,
	Nop,
}

#[derive(Debug, Clone)]
struct Node {
	id: NodeId,
	node_type: NodeType,
	bits: Vec<Bit>,
	mapped_id: String,
	fanin: Vec<NodeId>,
	fanout: Vec<NodeId>,
}

fn get_arithmetic_op(op: &str) -> ArithmeticOperator {
	match op {
		AND => ArithmeticOperator::And,
		OR => ArithmeticOperator::Or,
		XOR => ArithmeticOperator::Xor,
		SHL => ArithmeticOperator::Sll,
		SHR => ArithmeticOperator::Srl,
		MUL => ArithmeticOperator::Mult,
		DIV => ArithmeticOperator::Div,
		MOD => ArithmeticOperator::Mod,
		POW => ArithmeticOperator::Exp,
		ADD => ArithmeticOperator::Add,
		SUB => ArithmeticOperator::Sub,
		_ => panic!("Unsupported arithmetic op"),
	}
}

impl CheckedDesign {
	pub fn new() -> Self {
		Self {
			nodes: vec![],
			signals: vec![],
			cache: RefCell::new(CheckedDesignCache {}),
		}
	}

	fn new_node(&mut self, mapped_id: &str, bits: Vec<Bit>, node_type: NodeType) -> NodeId {
		self.nodes.push(Node {
			id: self.nodes.len(),
			node_type,
			bits,
			mapped_id: mapped_id.to_owned(),
			fanin: vec![],
			fanout: vec![],
		});
		self.nodes.len() - 1
	}

	fn connect(&mut self, send: NodeId, recv: NodeId) {
		if !self.nodes[send].fanout.contains(&recv) {
			self.nodes[send].fanout.push(recv);
			self.nodes[recv].fanin.push(send);
		}
	}

	fn disconnect(&mut self, send: NodeId, recv: NodeId) {
		if self.nodes[send].fanout.contains(&recv) {
			let idx1 = self.nodes[send]
				.fanout
				.iter()
				.enumerate()
				.find(|(_idx, val)| **val == recv)
				.unwrap()
				.0;
			let idx2 = self.nodes[recv]
				.fanin
				.iter()
				.enumerate()
				.find(|(_idx, val)| **val == send)
				.unwrap()
				.0;
			self.nodes[send].fanout.remove(idx1);
			self.nodes[recv].fanin.remove(idx2);
		}
	}

	fn initialize_body_nodes(&mut self, mapped_design: &MappedDesign) {
		mapped_design.for_all_top_level_io(|_, name, port| {
			if let Some(_) = lookup_id(name) {
				self.new_node(name, port.bits.clone(), NodeType::PortBody);
			} else {
				panic!(
					"{:?} on top level design doesn't match a named game signal",
					port
				)
			}
		});
		mapped_design.for_all_cells(|_, name, _cell| {
			self.new_node(name, vec![], NodeType::CellBody);
		});
	}

	fn get_max_bit(&mut self, mapped_design: &MappedDesign) -> u64 {
		let mut max_bit: u64 = 0;
		for node in &mut self.nodes {
			if node.node_type != NodeType::CellBody {
				continue;
			}
			let cell = mapped_design.get_cell(&node.mapped_id);
			match cell.cell_type.as_str() {
				IMPLEMENTABLE_OPS!() => {}
				_ => panic!("{:?} can't be implemented", cell),
			};
			cell.connections["A"]
				.iter()
				.chain(cell.connections["B"].iter())
				.chain(cell.connections["Y"].iter())
				.for_each(|b| {
					if let Bit::Id(bitid) = b {
						max_bit = max_bit.max(bitid.0);
					}
				})
		}
		max_bit
	}

	fn update_bits(&mut self, mapped_design: &MappedDesign) -> Vec<Vec<NodeId>> {
		let max_bit = self.get_max_bit(mapped_design);
		let mut bit_map: Vec<Vec<NodeId>> = vec![vec![]; max_bit as usize + 1];
		for node in &mut self.nodes {
			if node.node_type != NodeType::CellBody {
				continue;
			}
			let cell = mapped_design.get_cell(&node.mapped_id);
			cell.connections["A"]
				.iter()
				.chain(cell.connections["B"].iter())
				.chain(cell.connections["Y"].iter())
				.for_each(|b| {
					if let Bit::Id(bitid) = b {
						bit_map[bitid.0 as usize].push(node.id);
					}
				})
		}
		for node in &mut self.nodes {
			if node.node_type != NodeType::PortBody {
				continue;
			}
			let port = mapped_design.get_port(&node.mapped_id);
			port.bits.iter().for_each(|b| {
				if let Bit::Id(bitid) = b {
					bit_map[bitid.0 as usize].push(node.id);
				}
			})
		}
		bit_map
	}

	fn fill_out_body_nodes(
		&mut self,
		bit_map: &Vec<Vec<NodeId>>,
		attached_map: &mut Vec<Vec<Vec<NodeId>>>,
		mapped_design: &MappedDesign,
	) {
		for node in &self.nodes {
			match &node.node_type {
				NodeType::Nop => panic!("Didn't expect to have these yet."),
				NodeType::CellInput { .. } => panic!("Didn't expect to have these yet."),
				NodeType::CellOutput { .. } => panic!("Didn't expect to have these yet."),
				NodeType::PortInput => panic!("Didn't expect to have these yet."),
				NodeType::PortOutput => panic!("Didn't expect to have these yet."),
				NodeType::PortBody => {
					let port = mapped_design.get_port(&node.mapped_id);
					for bit in &port.bits {
						if let Bit::Id(bitid) = bit {
							for attached in &bit_map[bitid.0 as usize] {
								if *attached != node.id
									&& !attached_map[node.id][0].contains(attached)
									&& (port.direction == Direction::Output
										|| port.direction == Direction::Inout)
								{
									attached_map[node.id][0].push(*attached);
								}
								if *attached != node.id
									&& !attached_map[node.id][2].contains(attached)
									&& (port.direction == Direction::Input
										|| port.direction == Direction::Inout)
								{
									attached_map[node.id][2].push(*attached);
								}
							}
						}
					}
				}
				NodeType::CellBody => {
					let cell = mapped_design.get_cell(&node.mapped_id);
					for bit in &cell.connections["A"] {
						if let Bit::Id(bitid) = bit {
							for attached in &bit_map[bitid.0 as usize] {
								if *attached != node.id
									&& !attached_map[node.id][0].contains(attached)
								{
									attached_map[node.id][0].push(*attached);
								}
							}
						}
					}
					for bit in &cell.connections["B"] {
						if let Bit::Id(bitid) = bit {
							for attached in &bit_map[bitid.0 as usize] {
								if *attached != node.id
									&& !attached_map[node.id][1].contains(attached)
								{
									attached_map[node.id][1].push(*attached);
								}
							}
						}
					}
					for bit in &cell.connections["Y"] {
						if let Bit::Id(bitid) = bit {
							for attached in &bit_map[bitid.0 as usize] {
								if *attached != node.id
									&& !attached_map[node.id][2].contains(attached)
								{
									attached_map[node.id][2].push(*attached);
								}
							}
						}
					}
				}
			}
		}
	}

	fn elaborate_attached_map_post_fill_out(
		&mut self,
		attached_map: &mut Vec<Vec<Vec<NodeId>>>,
		mapped_design: &MappedDesign,
	) {
		let mut nodeid: usize = 0;
		while nodeid < self.nodes.len() {
			let node = &self.nodes[nodeid];
			match &node.node_type {
				NodeType::CellInput { .. } => {
					assert!(attached_map[nodeid][2].len() == 0);
					assert!(
						(attached_map[nodeid][0].len() == 1 && attached_map[nodeid][1].len() == 0)
							|| (attached_map[nodeid][1].len() == 1
								&& attached_map[nodeid][0].len() == 0)
					);
					let to_connect_body_id = *attached_map[nodeid][0]
						.get(0)
						.or(attached_map[nodeid][1].get(0))
						.unwrap();
					let to_connect_id = self.nodes[to_connect_body_id].fanout[0];
					self.connect(to_connect_id, nodeid);
				}
				NodeType::CellOutput { .. } => {
					assert!(attached_map[nodeid][0].len() == 0);
					assert!(attached_map[nodeid][1].len() == 0);
					assert!(attached_map[nodeid][2].len() > 0);
				}
				NodeType::PortInput => {
					assert!(attached_map[nodeid][0].len() == 1);
					assert!(attached_map[nodeid][1].len() == 0);
					assert!(attached_map[nodeid][2].len() == 0);
					let to_connect_body_id = attached_map[nodeid]
						.get(0)
						.or(attached_map[nodeid].get(1))
						.unwrap()[0];
					let to_connect_id = self.nodes[to_connect_body_id].fanout[0];
					self.connect(to_connect_id, nodeid);
				}
				NodeType::PortOutput => {
					assert!(attached_map[nodeid][0].len() == 0);
					assert!(attached_map[nodeid][1].len() == 0);
					assert!(attached_map[nodeid][2].len() > 0);
				}
				NodeType::PortBody => {
					let port = mapped_design.get_port(&node.mapped_id);
					let mapped_id = node.mapped_id.clone();
					if port.direction == Direction::Input || port.direction == Direction::Inout {
						let id = self.new_node(&mapped_id, port.bits.clone(), NodeType::PortOutput);
						self.connect(nodeid, id);
						attached_map.push(attached_map[nodeid].clone());
					}
					if port.direction == Direction::Output || port.direction == Direction::Inout {
						let id = self.new_node(&mapped_id, port.bits.clone(), NodeType::PortInput);
						self.connect(id, nodeid);
						attached_map.push(attached_map[nodeid].clone());
					}
				}
				NodeType::CellBody => {
					let cell = mapped_design.get_cell(&node.mapped_id);
					let mapped_id = node.mapped_id.clone();
					let bits_a = cell.connections["A"].clone();
					let bits_b = cell.connections["B"].clone();
					let bits_y = cell.connections["Y"].clone();
					assert_eq!(attached_map[nodeid][0].len(), 1);
					assert_eq!(attached_map[nodeid][1].len(), 1);
					let a = self.new_node(
						&mapped_id,
						bits_a,
						NodeType::CellInput {
							port: "A".to_owned(),
						},
					);
					let b: usize = self.new_node(
						&mapped_id,
						bits_b,
						NodeType::CellInput {
							port: "B".to_owned(),
						},
					);
					let y = self.new_node(
						&mapped_id,
						bits_y,
						NodeType::CellOutput {
							port: "Y".to_owned(),
						},
					);
					self.connect(a, nodeid);
					self.connect(b, nodeid);
					self.connect(nodeid, y);
					attached_map.push(vec![attached_map[nodeid][0].clone(), vec![], vec![]]);
					attached_map.push(vec![vec![], attached_map[nodeid][1].clone(), vec![]]);
					attached_map.push(vec![vec![], vec![], attached_map[nodeid][2].clone()]);
				}
				NodeType::Nop => {}
			}
			nodeid += 1;
		}
	}

	pub fn insert_nop_to_sanitize_ports(&mut self) {
		let topo_order = self.get_topo_order();
		// Insert Nop to partition signal networks
		let signal_choices = self.get_signal_choices();
		for nodeid in &topo_order {
			let node = &self.nodes[*nodeid];
			match &node.node_type {
				NodeType::PortInput => {
					if signal_choices[node.id].len() < 2 {
						continue;
					}
					let bits = node.bits.clone();
					for fiid in node.fanin.clone() {
						self.disconnect(fiid, *nodeid);
						let nop = self.new_node("$nop", bits.clone(), NodeType::Nop);
						self.connect(fiid, nop);
						self.connect(nop, *nodeid);
					}
				}
				_ => {}
			}
		}
		let topo_order = self.get_topo_order();
		// Recalculate and insert Nop to partition signal networks
		let signal_choices = self.get_signal_choices();
		for nodeid in topo_order.iter().rev() {
			let node = &self.nodes[*nodeid];
			match &node.node_type {
				NodeType::CellBody { .. } => {
					for cell_input in node.fanin.clone() {
						let mut signals = signal_choices[cell_input].clone();
						signals.sort();
						let mut found = false;
						for i in 0..signals.len() - 1 {
							if signals[i] == signals[i + 1] {
								found = true;
								break;
							}
						}
						if !found {
							continue;
						}
						let node = &self.nodes[cell_input];
						let bits = node.bits.clone();
						let fiid = node.fanin[0];
						self.disconnect(fiid, cell_input);
						let nop = self.new_node("$nop", bits.clone(), NodeType::Nop);
						self.connect(fiid, nop);
						self.connect(nop, cell_input);
					}
				}
				_ => {}
			}
		}
	}

	fn calculate_and_validate_signal_choices(
		&mut self,
		attached_map: &mut Vec<Vec<Vec<NodeId>>>,
	) -> Vec<Option<i32>> {
		let mut signal_choices = self.get_signal_choices();
		for x in 0..self.nodes.len() {
			match &self.nodes[x].node_type {
				NodeType::CellInput { port } => {
					if signal_choices[x].len() < 2 {
						continue;
					}
					match port.as_str() {
						"A" => {
							assert!(attached_map[x][0].len() == 1);
							signal_choices[x] = signal_choices[attached_map[x][0][0]].clone();
						}
						"B" => {
							assert!(attached_map[x][1].len() == 1);
							signal_choices[x] = signal_choices[attached_map[x][1][0]].clone();
						}
						_ => assert!(false),
					}
				}
				NodeType::CellOutput { port } => {
					if signal_choices[x].len() < 2 {
						continue;
					}
					match port.as_str() {
						"Y" => {
							assert!(attached_map[x][0].len() == 1);
							signal_choices[x] = signal_choices[attached_map[x][2][0]].clone();
						}
						_ => assert!(false),
					}
				}
				NodeType::PortInput => assert!(signal_choices[x].len() < 2),
				NodeType::PortOutput => {}
				NodeType::PortBody => {}
				NodeType::CellBody => {}
				NodeType::Nop => {}
			}
		}
		signal_choices
			.into_iter()
			.map(|x| x.get(0).map(|x| *x))
			.collect()
	}

	fn elaborate_signal_choices(&mut self, signal_choices: &mut Vec<Option<i32>>) {
		// Check that we now only have 0 or 1 option for a signal
		let topo_order = self.get_topo_order();
		topo_order
			.iter()
			.map(|x| {
				(
					*x,
					self.nodes[*x].node_type.clone(),
					signal_choices[*x],
					self.nodes[*x].fanin.clone(),
					self.nodes[*x].fanout.clone(),
				)
			})
			.for_each(|tpl| println!("{:?}", tpl));
		// Final signal resolve
		for nodeid in topo_order {
			if signal_choices[nodeid].is_some() {
				continue;
			}
			let node = &self.nodes[nodeid];
			match node.node_type {
				NodeType::CellInput { .. } | NodeType::CellOutput { .. } => {
					let local_io = self.get_local_cell_io_network(nodeid);
					let set_io = local_io
						.iter()
						.fold(HashSet::<i32>::new(), |mut set, ioid| {
							if let Some(sig) = signal_choices[*ioid] {
								set.insert(sig);
							};
							set
						});
					let mut sig = 0;
					for id in local_io.iter() {
						while set_io.contains(&sig) {
							sig += 1;
						}
						self.set_signal(signal_choices, *id, sig);
						sig += 1;
					}
				}
				NodeType::PortInput => { /* Already picked */ }
				NodeType::PortOutput => { /* Already picked */ }
				NodeType::PortBody => { /* N/A */ }
				NodeType::CellBody => { /* N/A */ }
				NodeType::Nop => { /* Let others pick */ }
			}
		}
	}

	fn signals_correctness_check(&mut self, signal_choices: &Vec<Option<i32>>) {
		// Final correctness check
		let topo_order = self.get_topo_order();
		for nodeid in topo_order {
			let node = &self.nodes[nodeid];
			println!("{:?}", nodeid);
			match node.node_type {
				NodeType::CellInput { .. } | NodeType::PortInput => {
					assert!(signal_choices[node.id].is_some());
					for fiid in &node.fanin {
						if self.nodes[*fiid].node_type != NodeType::Nop {
							assert!(signal_choices[node.id] == signal_choices[*fiid]);
						}
					}
				}
				NodeType::CellOutput { .. } | NodeType::PortOutput => {
					assert!(signal_choices[node.id].is_some());
					for foid in &node.fanout {
						if self.nodes[*foid].node_type != NodeType::Nop {
							assert_eq!(signal_choices[node.id], signal_choices[*foid]);
						}
					}
				}
				NodeType::PortBody => {
					assert!(signal_choices[node.id].is_some());
					for foid in &node.fanout {
						assert_eq!(signal_choices[node.id], signal_choices[*foid]);
					}
					for fiid in &node.fanin {
						assert_eq!(signal_choices[node.id], signal_choices[*fiid]);
					}
				}
				NodeType::CellBody => assert!(signal_choices[node.id].is_none()),
				NodeType::Nop => assert!(signal_choices[node.id].is_none()),
			}
		}
	}

	pub fn build_from(&mut self, mapped_design: &MappedDesign) {
		self.initialize_body_nodes(mapped_design);
		let bit_map = self.update_bits(mapped_design);
		let mut attached_map: Vec<Vec<Vec<NodeId>>> = vec![vec![vec![]; 3]; self.nodes.len()];
		self.fill_out_body_nodes(&bit_map, &mut attached_map, mapped_design);
		self.elaborate_attached_map_post_fill_out(&mut attached_map, mapped_design);
		self.insert_nop_to_sanitize_ports();
		let mut signal_choices = self.calculate_and_validate_signal_choices(&mut attached_map);
		self.elaborate_signal_choices(&mut signal_choices);
		self.signals_correctness_check(&signal_choices);
		self.signals = signal_choices;
	}

	fn set_signal(&self, signals: &mut Vec<Option<i32>>, nodeid: NodeId, mut signal: i32) {
		let node = &self.nodes[nodeid];
		match &node.node_type {
			NodeType::CellInput { .. } | NodeType::PortInput => {
				for fiid in &node.fanin {
					if self.nodes[*fiid].node_type == NodeType::PortOutput
						&& signals[*fiid].is_some()
					{
						signal = signals[*fiid].unwrap();
						break;
					}
				}
				if let Some(s) = signals[nodeid] {
					signal = s;
				}
				signals[nodeid] = Some(signal);
				for fiid in &node.fanin {
					if self.nodes[*fiid].node_type == NodeType::PortOutput {
						signals[*fiid] = Some(signal);
					}
				}
			}
			NodeType::CellOutput { .. } | NodeType::PortOutput => {
				for fiid in &node.fanin {
					if self.nodes[*fiid].node_type == NodeType::PortInput
						&& signals[*fiid].is_some()
					{
						signal = signals[*fiid].unwrap();
						break;
					}
				}
				if let Some(s) = signals[nodeid] {
					signal = s;
				}
				signals[nodeid] = Some(signal);
				for foid in &node.fanout {
					if self.nodes[*foid].node_type == NodeType::PortInput {
						signals[*foid] = Some(signal);
					}
				}
			}
			NodeType::PortBody => {}
			NodeType::CellBody => {}
			NodeType::Nop => {}
		}
	}

	fn get_other_input_nodes(&self, nodeid: NodeId) -> Vec<NodeId> {
		match self.nodes[nodeid].node_type {
			NodeType::CellInput { .. } => assert!(true),
			_ => assert!(false),
		};
		self.nodes[self.nodes[nodeid].fanout[0]]
			.fanin
			.iter()
			.filter(|id| **id != nodeid)
			.map(|id| *id)
			.collect()
	}

	fn get_local_cell_io_network(&self, nodeid: NodeId) -> Vec<NodeId> {
		let mut retval = vec![];
		let mut queue = BTreeSet::new();
		let mut seen = HashSet::new();
		queue.insert(nodeid);
		while !queue.is_empty() {
			let curid = queue.pop_first().unwrap();
			if !seen.insert(curid) {
				continue;
			}
			match &self.nodes[curid].node_type {
				NodeType::PortInput => {
					retval.push(curid);
				}
				NodeType::CellInput { .. } => {
					retval.push(curid);
					queue.extend(self.get_other_input_nodes(curid));
				}
				NodeType::CellOutput { .. } | NodeType::PortOutput => {
					retval.push(curid);
					for foid in &self.nodes[curid].fanout {
						queue.insert(*foid);
					}
				}
				NodeType::PortBody | NodeType::CellBody => {
					panic!("Implementer is a fucking moron.")
				}
				NodeType::Nop => {
					// Do nothing
				}
			}
		}
		retval
	}

	fn get_signal_choices(&self) -> Vec<Vec<i32>> {
		let topo_order = self.get_topo_order();
		let mut signal_choices = vec![vec![]; topo_order.len()];
		for node in &self.nodes {
			if node.node_type == NodeType::PortBody {
				let choice = signal_lookup_table::lookup_id(&node.mapped_id).unwrap();
				signal_choices[node.id] = vec![choice];
				if let Some(fiid) = node.fanin.get(0) {
					signal_choices[*fiid] = vec![choice];
				}
				if let Some(foid) = node.fanout.get(0) {
					signal_choices[*foid] = vec![choice];
				}
			}
		}
		for nodeid in &topo_order {
			let node = &self.nodes[*nodeid];
			match &node.node_type {
				NodeType::CellInput { .. } => {
					for fiid in &node.fanin {
						if self.nodes[*fiid].node_type == NodeType::PortOutput {
							let xx = signal_choices[*fiid].clone();
							signal_choices[*nodeid].extend(xx);
						}
					}
				}
				NodeType::CellOutput { .. } | NodeType::PortOutput => {
					for foid in &node.fanout {
						if self.nodes[*foid].node_type == NodeType::PortInput {
							let xx = signal_choices[*foid].clone();
							signal_choices[*nodeid].extend(xx)
						}
					}
				}
				_ => {}
			};
		}
		signal_choices
	}

	fn get_topo_order(&self) -> Vec<NodeId> {
		let mut topo_seen = HashSet::new();
		let mut topological_order = vec![];
		let mut root_nodes = vec![];
		for node in &self.nodes {
			if node.fanin.is_empty() {
				root_nodes.push(node.id);
			}
		}
		let mut queue = LinkedList::new();
		for id in &root_nodes {
			queue.push_back(*id);
		}
		while !queue.is_empty() {
			let id = queue.pop_front().unwrap();
			topo_seen.insert(id);
			topological_order.push(id);
			for foid in &self.nodes[id].fanout {
				if self.nodes[*foid]
					.fanin
					.iter()
					.all(|fiid| topo_seen.contains(fiid))
				{
					queue.push_back(*foid);
				}
			}
		}
		return topological_order;
	}

	#[allow(dead_code)]
	fn get_depth(&self) -> Vec<i32> {
		let topo = self.get_topo_order();
		let mut depth = vec![0; self.nodes.len()];
		for nodeid in &topo {
			depth[*nodeid] = self.nodes[*nodeid]
				.fanin
				.iter()
				.map(|fiid| depth[*fiid])
				.max()
				.unwrap_or(-1)
				+ 1
		}
		depth
	}

	pub fn apply_onto(&self, logical_design: &mut LogicalDesign, mapped_design: &MappedDesign) {
		type LID = logical_design::NodeId;
		use logical_design::Signal;
		let topo_order = self.get_topo_order();
		let mut logic_map: Vec<Option<LID>> = vec![None; topo_order.len()];
		for nodeid_ref in topo_order.iter() {
			let nodeid = *nodeid_ref;
			let node = &self.nodes[nodeid];
			println!("Node: {:?}", node.node_type);
			match &node.node_type {
				NodeType::CellOutput { .. } | NodeType::PortOutput => {
					logic_map[nodeid] = logic_map[node.fanin[0]];
				}
				NodeType::PortInput | NodeType::CellInput { .. } => {
					logic_map[nodeid] = Some(logical_design.add_wire_floating());
				}
				NodeType::PortBody => {
					if node.fanout.len() > 0 {
						logic_map[nodeid] = Some(logical_design.add_constant_comb(
							vec![Signal::Id(self.signals[nodeid].unwrap())],
							vec![1],
						));
					} else if node.fanin.len() > 0 {
						logic_map[nodeid] = Some(logical_design.add_lamp((
							Signal::Id(self.signals[nodeid].unwrap()),
							logical_design::DeciderOperator::NotEqual,
							Signal::Constant(0),
						)));
					}
				}
				NodeType::CellBody => {
					let sig_left = self.signals[node.fanin[0]].unwrap();
					let sig_right = self.signals[node.fanin[1]].unwrap();
					let sig_out = self.signals[node.fanout[0]].unwrap();
					let op = get_arithmetic_op(&mapped_design.get_cell(&node.mapped_id).cell_type);
					logic_map[nodeid] = Some(logical_design.add_arithmetic_comb(
						(Signal::Id(sig_left), op, Signal::Id(sig_right)),
						Signal::Id(sig_out),
					))
				}
				NodeType::Nop => {
					let sig_in = self.signals[node.fanin[0]].unwrap();
					let sig_out = self.signals[node.fanout[0]].unwrap();
					logic_map[nodeid] = Some(logical_design.add_arithmetic_comb(
						(
							Signal::Id(sig_in),
							logical_design::ArithmeticOperator::Add,
							Signal::Constant(0),
						),
						Signal::Id(sig_out),
					))
				}
			}
		}
		for nodeid_ref in topo_order.iter() {
			let nodeid = *nodeid_ref;
			let node = &self.nodes[nodeid];
			if node.node_type == NodeType::CellBody || node.node_type == NodeType::PortBody {
				continue;
			}
			for foid in node.fanout.iter() {
				logical_design.connect(logic_map[nodeid].unwrap(), logic_map[*foid].unwrap());
			}
		}
	}
}

#[cfg(test)]
mod test {
	use std::{fs::File, io::BufReader};

	use crate::{physical_design::PhysicalDesign, serializable_design::SerializableDesign};

	use super::*;

	#[test]
	fn design_test1() {
		let file = File::open("./test_designs/output/test1.json").unwrap();
		let reader = BufReader::new(file);
		let mapped_design: MappedDesign = serde_json::from_reader(reader).unwrap();
		let mut checked_design = CheckedDesign::new();
		let mut logical_design = LogicalDesign::new();
		let mut physical_design = PhysicalDesign::new();
		let mut serializable_design = SerializableDesign::new();
		checked_design.build_from(&mapped_design);
		logical_design.build_from(&checked_design, &mapped_design);
		println!("{:?}", logical_design);
		physical_design.build_from(&logical_design);
		serializable_design.build_from(&physical_design, &logical_design);
		let blueprint_json = serde_json::to_string(&serializable_design).unwrap();
		println!("{}", blueprint_json);
	}
}

use core::slice;
use std::{
	cell::{self, RefCell},
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
	mapped_design::{BitSliceOps, Direction},
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
	CellBody { cell_type: CellType },
}

impl NodeType {
	fn is_cell_body(&self) -> bool {
		match self {
			NodeType::CellBody { .. } => true,
			_ => false,
		}
	}

	fn is_port_body(&self) -> bool {
		match self {
			NodeType::PortBody => true,
			_ => false,
		}
	}

	fn is_output(&self) -> bool {
		match self {
			NodeType::CellOutput { .. } => true,
			NodeType::PortOutput => true,
			_ => false,
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum CellType {
	ABY,
	Constant { value: i32 },
	Nop,
}

#[derive(Debug, Clone)]
struct Node {
	id: NodeId,
	node_type: NodeType,
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

	fn node_type(&self, nodeid: NodeId) -> &NodeType {
		&self.nodes[nodeid].node_type
	}

	fn is_driver(&self, nodeid: NodeId, to_check: NodeId) -> bool {
		match self.node_type(to_check) {
			NodeType::CellOutput { .. } | NodeType::PortOutput => {
				self.nodes[nodeid].fanin.contains(&to_check)
			}
			_ => false,
		}
	}

	fn new_node(&mut self, mapped_id: &str, node_type: NodeType) -> NodeId {
		self.nodes.push(Node {
			id: self.nodes.len(),
			node_type,
			mapped_id: mapped_id.to_owned(),
			fanin: vec![],
			fanout: vec![],
		});
		self.nodes.len() - 1
	}

	fn connect(&mut self, send: NodeId, recv: NodeId) {
		if !self.nodes[send].fanout.contains(&recv) {
			//println!("Connect: {:?} -> {:?}", send, recv);
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
				self.new_node(name, NodeType::PortBody);
			} else {
				panic!(
					"{:?} on top level design doesn't match a named game signal",
					port
				)
			}
		});
		mapped_design.for_all_cells(|_, name, _cell| {
			self.new_node(
				name,
				NodeType::CellBody {
					cell_type: CellType::ABY,
				},
			);
		});
	}

	fn get_max_bit(&mut self, mapped_design: &MappedDesign) -> u64 {
		let mut max_bit: u64 = 0;
		for node in &mut self.nodes {
			let cell_type = if let NodeType::CellBody { cell_type } = &node.node_type {
				cell_type
			} else {
				continue;
			};
			match cell_type {
				CellType::ABY => {
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
				CellType::Constant { .. } => {}
				CellType::Nop => {
					let cell = mapped_design.get_cell(&node.mapped_id);
					match cell.cell_type.as_str() {
						IMPLEMENTABLE_OPS!() => {}
						_ => panic!("{:?} can't be implemented", cell),
					};
					cell.connections["A"]
						.iter()
						.chain(cell.connections["Y"].iter())
						.for_each(|b| {
							if let Bit::Id(bitid) = b {
								max_bit = max_bit.max(bitid.0);
							}
						})
				}
			}
		}
		max_bit
	}

	fn get_base_attach_map(&mut self, mapped_design: &MappedDesign) -> Vec<Vec<Vec<NodeId>>> {
		let max_bit = self.get_max_bit(mapped_design);
		let mut attach_map: Vec<Vec<Vec<NodeId>>> = vec![vec![vec![]; 3]; self.nodes.len()];
		let mut bit_map: Vec<Vec<Vec<NodeId>>> = vec![vec![vec![]; 3]; max_bit as usize + 1];
		for node in &mut self.nodes {
			let cell_type = if let NodeType::CellBody { cell_type } = &node.node_type {
				cell_type
			} else {
				continue;
			};
			match cell_type {
				CellType::ABY => {
					let cell = mapped_design.get_cell(&node.mapped_id);
					for b in cell.connections["A"].iter() {
						if let Bit::Id(bitid) = b {
							bit_map[bitid.0 as usize][0].push(node.id);
						}
						break;
					}
					for b in cell.connections["B"].iter() {
						if let Bit::Id(bitid) = b {
							bit_map[bitid.0 as usize][1].push(node.id);
						}
						break;
					}
					for b in cell.connections["Y"].iter() {
						if let Bit::Id(bitid) = b {
							bit_map[bitid.0 as usize][2].push(node.id);
						}
						break;
					}
				}
				CellType::Constant { .. } => {}
				CellType::Nop => {
					let cell = mapped_design.get_cell(&node.mapped_id);
					for b in cell.connections["A"].iter() {
						if let Bit::Id(bitid) = b {
							bit_map[bitid.0 as usize][0].push(node.id);
						}
						break;
					}
					for b in cell.connections["Y"].iter() {
						if let Bit::Id(bitid) = b {
							bit_map[bitid.0 as usize][2].push(node.id);
						}
						break;
					}
				}
			}
		}
		for node in &mut self.nodes {
			if node.node_type != NodeType::PortBody {
				continue;
			}
			let port = mapped_design.get_port(&node.mapped_id);
			match port.direction {
				Direction::Input => port.bits.iter().for_each(|b| {
					if let Bit::Id(bitid) = b {
						bit_map[bitid.0 as usize][2].push(node.id);
					}
				}),
				Direction::Output => port.bits.iter().for_each(|b| {
					if let Bit::Id(bitid) = b {
						bit_map[bitid.0 as usize][0].push(node.id);
					}
				}),
				Direction::Inout => {
					port.bits.iter().for_each(|b| {
						if let Bit::Id(bitid) = b {
							bit_map[bitid.0 as usize][0].push(node.id);
							bit_map[bitid.0 as usize][2].push(node.id);
						}
					});
				}
			}
		}
		for bit in 2..bit_map.len() {
			for a_input in &bit_map[bit][0] {
				attach_map[*a_input][0].extend(bit_map[bit][2].iter());
			}
			for b_input in &bit_map[bit][1] {
				attach_map[*b_input][1].extend(bit_map[bit][2].iter());
			}
			for y_output in &bit_map[bit][2] {
				attach_map[*y_output][2].extend(bit_map[bit][0].iter());
				attach_map[*y_output][2].extend(bit_map[bit][1].iter());
			}
		}
		attach_map
	}

	fn insert_constant_on_input(
		&mut self,
		attached_map: &mut Vec<Vec<Vec<NodeId>>>,
		nodeid: NodeId,
		terminal_id: usize,
		bits: Vec<Bit>,
	) {
		let new_constant = self.new_node(
			"$constant",
			NodeType::CellBody {
				cell_type: CellType::Constant {
					value: bits.get_constant(),
				},
			},
		);
		attached_map[nodeid][terminal_id] = vec![new_constant];
		attached_map.push(vec![vec![], vec![], vec![nodeid]]);
	}

	fn fill_out_body_nodes(
		&mut self,
		attached_map: &mut Vec<Vec<Vec<NodeId>>>,
		mapped_design: &MappedDesign,
	) {
		for nodeid in 0..self.nodes.len() {
			let node = &self.nodes[nodeid];
			match &node.node_type {
				NodeType::CellInput { .. }
				| NodeType::CellOutput { .. }
				| NodeType::PortInput
				| NodeType::PortOutput => panic!("Didn't expect to have these yet."),
				NodeType::PortBody => {
					let port = mapped_design.get_port(&node.mapped_id);
					if port.bits.is_all_constants() {
						assert!(port.direction == Direction::Output);
						self.insert_constant_on_input(attached_map, nodeid, 0, port.bits.clone());
						continue;
					}
					assert!(
						port.bits.is_all_connections(),
						"Currently doesn't support mixed bits and constants on a port"
					);
				}
				NodeType::CellBody { cell_type } => {
					assert!(*cell_type == CellType::ABY);
					let cell = mapped_design.get_cell(&node.mapped_id);
					if cell.connections["A"].is_all_constants() {
						self.insert_constant_on_input(
							attached_map,
							nodeid,
							0,
							cell.connections["A"].clone(),
						);
						continue;
					}
					assert!(
						cell.connections["A"].is_all_connections(),
						"Currently doesn't support mixed bits and constants on a port"
					);
					if cell.connections["B"].is_all_constants() {
						self.insert_constant_on_input(
							attached_map,
							nodeid,
							0,
							cell.connections["B"].clone(),
						);
						continue;
					}
					assert!(
						cell.connections["B"].is_all_connections(),
						"Currently doesn't support mixed bits and constants on a port"
					);
				}
			}
		}
	}

	/** Map all the body nodes into their appropriate structure.  */
	fn elaborate_body_nodes_wire_design(
		&mut self,
		attached_map: &mut Vec<Vec<Vec<NodeId>>>,
		mapped_design: &MappedDesign,
	) {
		let mut nodeid: usize = 0;
		while nodeid < self.nodes.len() {
			let node = &self.nodes[nodeid];
			match &node.node_type {
				NodeType::CellInput { .. } => {
					let to_connect_body_id = *attached_map[nodeid][0]
						.get(0)
						.or(attached_map[nodeid][1].get(0))
						.unwrap();
					let to_connect_id = self.nodes[to_connect_body_id].fanout[0];
					self.connect(to_connect_id, nodeid);
				}
				NodeType::CellOutput { .. } => {}
				NodeType::PortInput => {
					let to_connect_body_id = attached_map[nodeid]
						.get(0)
						.or(attached_map[nodeid].get(1))
						.unwrap()[0];
					let to_connect_id = self.nodes[to_connect_body_id].fanout[0];
					self.connect(to_connect_id, nodeid);
				}
				NodeType::PortOutput => {}
				NodeType::PortBody => {
					let port = mapped_design.get_port(&node.mapped_id);
					let mapped_id = node.mapped_id.clone();
					if port.direction == Direction::Input || port.direction == Direction::Inout {
						let id = self.new_node(&mapped_id, NodeType::PortOutput);
						self.connect(nodeid, id);
						attached_map.push(attached_map[nodeid].clone());
					}
					if port.direction == Direction::Output || port.direction == Direction::Inout {
						let id = self.new_node(&mapped_id, NodeType::PortInput);
						self.connect(id, nodeid);
						attached_map.push(attached_map[nodeid].clone());
					}
				}
				NodeType::CellBody { cell_type } => match cell_type {
					CellType::ABY => {
						let mapped_id = node.mapped_id.clone();
						let a = self.new_node(
							&mapped_id,
							NodeType::CellInput {
								port: "A".to_owned(),
							},
						);
						let b: usize = self.new_node(
							&mapped_id,
							NodeType::CellInput {
								port: "B".to_owned(),
							},
						);
						let y = self.new_node(
							&mapped_id,
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
					CellType::Constant { .. } => {
						let id = self.new_node(
							"$constant",
							NodeType::CellOutput {
								port: "Y".to_owned(),
							},
						);
						self.connect(nodeid, id);
						attached_map.push(attached_map[nodeid].clone());
					}
					CellType::Nop => {
						let mapped_id = node.mapped_id.clone();
						let a = self.new_node(
							&mapped_id,
							NodeType::CellInput {
								port: "A".to_owned(),
							},
						);
						let y = self.new_node(
							&mapped_id,
							NodeType::CellOutput {
								port: "Y".to_owned(),
							},
						);
						self.connect(a, nodeid);
						self.connect(nodeid, y);
						attached_map.push(vec![attached_map[nodeid][0].clone(), vec![], vec![]]);
						attached_map.push(vec![vec![], vec![], attached_map[nodeid][2].clone()]);
					}
				},
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
					for fiid in node.fanin.clone() {
						self.disconnect(fiid, *nodeid);
						let a = self.new_node(
							"$nop",
							NodeType::CellInput {
								port: "A".to_string(),
							},
						);
						let nop = self.new_node(
							"$nop",
							NodeType::CellBody {
								cell_type: CellType::Nop,
							},
						);
						let y = self.new_node(
							"$nop",
							NodeType::CellOutput {
								port: "Y".to_owned(),
							},
						);
						self.connect(fiid, a);
						self.connect(a, nop);
						self.connect(nop, y);
						self.connect(y, *nodeid);
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
						if !signals.is_empty() {
							for i in 0..signals.len() - 1 {
								if signals[i] == signals[i + 1] {
									found = true;
									break;
								}
							}
						}
						if !found {
							continue;
						}

						let node = &self.nodes[cell_input];
						let fiid = node.fanin[0];
						self.disconnect(fiid, cell_input);
						let a = self.new_node(
							"$nop",
							NodeType::CellInput {
								port: "A".to_string(),
							},
						);
						let nop = self.new_node(
							"$nop",
							NodeType::CellBody {
								cell_type: CellType::Nop,
							},
						);
						let y = self.new_node(
							"$nop",
							NodeType::CellOutput {
								port: "Y".to_owned(),
							},
						);
						self.connect(fiid, a);
						self.connect(a, nop);
						self.connect(nop, y);
						self.connect(y, cell_input);
					}
				}
				_ => {}
			}
		}
	}

	fn calculate_and_validate_signal_choices(&self) -> Vec<Option<i32>> {
		self.get_signal_choice_final()
	}

	fn elaborate_signal_choices(&self, signal_choices: &mut Vec<Option<i32>>) {
		// Check that we now only have 0 or 1 option for a signal
		let topo_order = self.get_topo_order();
		topo_order
			.iter()
			.map(|x| {
				(
					*x,
					self.node_type(*x).clone(),
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
				NodeType::CellInput { .. } => {
					if signal_choices[node.fanin[0]].is_some() {
						signal_choices[nodeid] = signal_choices[node.fanin[0]].clone();
						continue;
					}
				}
				NodeType::CellOutput { .. } => {
					if node
						.fanout
						.iter()
						.any(|foid| signal_choices[*foid].is_some())
					{
						signal_choices[nodeid] = signal_choices[node.fanin[0]].clone();
						continue;
					}
				}
				NodeType::PortInput
				| NodeType::PortOutput
				| NodeType::PortBody
				| NodeType::CellBody { .. } => {
					continue;
				}
			}
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
	}

	fn signals_correctness_check(&self, signal_choices: &Vec<Option<i32>>) {
		// Final correctness check
		for nodeid in 0..self.nodes.len() {
			println!("{:?}", nodeid);
			let node = &self.nodes[nodeid];
			match node.node_type {
				NodeType::CellInput { .. } | NodeType::PortInput => {
					assert!(signal_choices[node.id].is_some());
					for fiid in &node.fanin {
						assert_eq!(signal_choices[node.id], signal_choices[*fiid]);
					}
				}
				NodeType::CellOutput { .. } | NodeType::PortOutput => {
					assert!(signal_choices[node.id].is_some());
					for foid in &node.fanout {
						assert_eq!(signal_choices[node.id], signal_choices[*foid]);
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
				NodeType::CellBody { .. } => assert!(signal_choices[node.id].is_none()),
			}
		}
	}

	pub fn build_from(&mut self, mapped_design: &MappedDesign) {
		self.initialize_body_nodes(mapped_design);
		let mut attached_map = self.get_base_attach_map(mapped_design);
		self.fill_out_body_nodes(&mut attached_map, mapped_design);
		self.elaborate_body_nodes_wire_design(&mut attached_map, mapped_design);
		self.insert_nop_to_sanitize_ports();
		let mut signal_choices = self.calculate_and_validate_signal_choices();
		self.elaborate_signal_choices(&mut signal_choices);
		println!("--------------1");
		for x in signal_choices.iter().enumerate() {
			println!("{:?}", x)
		}
		println!("--------------2");
		for x in &self.nodes {
			println!("{:?}", x);
		}
		println!("--------------3");
		self.signals_correctness_check(&signal_choices);
		self.signals = signal_choices;
	}

	fn set_signal(&self, signals: &mut Vec<Option<i32>>, nodeid: NodeId, signal: i32) {
		let node = &self.nodes[nodeid];
		if signals[nodeid].is_some() {
			return;
		}
		match &node.node_type {
			NodeType::CellInput { .. } | NodeType::PortInput => {
				let mut driver = NodeId::MAX;
				for fiid in &node.fanin {
					if self.is_driver(nodeid, *fiid) {
						driver = *fiid;
						break;
					}
				}
				if signals[driver].is_none() {
					self.set_signal(signals, driver, signal);
				}
			}
			NodeType::CellOutput { .. } | NodeType::PortOutput => {
				signals[nodeid] = Some(signal);
				for foid in &node.fanout {
					signals[*foid] = Some(signal);
				}
			}
			NodeType::PortBody => {}
			NodeType::CellBody { .. } => {}
		}
	}

	fn get_other_input_nodes(&self, nodeid: NodeId) -> Vec<NodeId> {
		match self.node_type(nodeid) {
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
		match self.node_type(nodeid) {
			NodeType::CellBody { .. } | NodeType::PortBody => {
				return vec![];
			}
			_ => {}
		}
		let mut retval = vec![];
		let mut queue = BTreeSet::new();
		let mut seen = HashSet::new();
		queue.insert(nodeid);
		while !queue.is_empty() {
			let curid = queue.pop_first().unwrap();
			if !seen.insert(curid) {
				continue;
			}
			match &self.node_type(curid) {
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
				NodeType::PortBody | NodeType::CellBody { .. } => {
					panic!("Implementer is a fucking moron.")
				}
			}
		}
		retval
	}

	fn get_attached_nodes(&self, nodeid: NodeId) -> Vec<NodeId> {
		match self.node_type(nodeid) {
			NodeType::CellBody { .. } | NodeType::PortBody => {
				return vec![];
			}
			_ => {}
		}
		let mut queue = BTreeSet::new();
		let mut seen = HashSet::new();
		let mut retval = vec![];
		queue.insert(nodeid);
		while !queue.is_empty() {
			let curid = queue.pop_first().unwrap();
			if !seen.insert(curid) {
				continue;
			}
			retval.push(curid);
			match self.node_type(curid) {
				NodeType::CellInput { .. } => {
					queue.extend(self.nodes[curid].fanin.iter());
				}
				NodeType::CellOutput { .. } => {
					queue.extend(self.nodes[curid].fanout.iter());
				}
				NodeType::PortInput => {
					queue.extend(self.nodes[curid].fanin.iter());
					queue.extend(self.nodes[self.nodes[curid].fanout[0]].fanout.iter());
				}
				NodeType::PortOutput => {
					queue.extend(self.nodes[curid].fanout.iter());
					queue.extend(self.nodes[self.nodes[curid].fanin[0]].fanin.iter());
				}
				NodeType::CellBody { .. } | NodeType::PortBody => {
					panic!("Implementer is a fucking moron")
				}
			}
		}
		retval
	}

	fn get_signal_choices(&self) -> Vec<Vec<i32>> {
		let mut signal_choices = vec![vec![]; self.nodes.len()];
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
		for nodeid in 0..self.nodes.len() {
			if !signal_choices[nodeid].is_empty() {
				continue;
			}
			let localio = self.get_local_cell_io_network(nodeid);
			let attached = self.get_attached_nodes(nodeid);
			let mut choice = vec![];
			for localid in localio.iter() {
				if attached.contains(localid) {
					continue;
				}
				choice.extend(signal_choices[*localid].iter());
			}
			signal_choices[nodeid] = choice;
		}
		signal_choices
	}

	fn get_signal_choice_final(&self) -> Vec<Option<i32>> {
		let mut signal_choices = vec![None; self.nodes.len()];
		for node in &self.nodes {
			if node.node_type == NodeType::PortBody {
				let choice = signal_lookup_table::lookup_id(&node.mapped_id).unwrap();
				signal_choices[node.id] = Some(choice);
				if let Some(fiid) = node.fanin.get(0) {
					signal_choices[*fiid] = Some(choice);
				}
				if let Some(foid) = node.fanout.get(0) {
					signal_choices[*foid] = Some(choice);
				}
			}
		}
		for nodeid in 0..self.nodes.len() {
			if signal_choices[nodeid].is_some() {
				continue;
			}
			let attached = self.get_attached_nodes(nodeid);
			let mut choice = None;
			for attachedid in attached.iter() {
				if *attachedid == nodeid {
					continue;
				}
				if signal_choices[*attachedid].is_some() {
					if choice.is_some() && choice != signal_choices[*attachedid] {
						panic!(
							"Got conflicting signal choices for node {} attached to {:?} with previous choice {:?} and new choice {:?}.",
							nodeid, attached, choice, signal_choices[*attachedid],
						)
					}
					choice = signal_choices[*attachedid].clone();
				}
			}
			signal_choices[nodeid] = choice;
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
				.unwrap_or(-1) + 1
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
				NodeType::CellBody { cell_type } => match cell_type {
					CellType::ABY => {
						let sig_left = self.signals[node.fanin[0]].unwrap();
						let sig_right = self.signals[node.fanin[1]].unwrap();
						let sig_out = self.signals[node.fanout[0]].unwrap();
						let op =
							get_arithmetic_op(&mapped_design.get_cell(&node.mapped_id).cell_type);
						logic_map[nodeid] = Some(logical_design.add_arithmetic_comb(
							(Signal::Id(sig_left), op, Signal::Id(sig_right)),
							Signal::Id(sig_out),
						))
					}
					CellType::Constant { value } => {
						logic_map[nodeid] = Some(logical_design.add_constant_comb(
							vec![Signal::Id(self.signals[node.fanout[0]].unwrap())],
							vec![*value],
						));
					}
					CellType::Nop => {
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
				},
			}
		}
		for nodeid_ref in topo_order.iter() {
			let nodeid = *nodeid_ref;
			let node = &self.nodes[nodeid];
			if node.node_type.is_cell_body() || node.node_type.is_port_body() {
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
		physical_design.build_from(&logical_design);
		serializable_design.build_from(&physical_design, &logical_design);
		let blueprint_json = serde_json::to_string(&serializable_design).unwrap();
		println!("{}", blueprint_json);
	}

	#[test]
	fn design_test2() {
		let file = File::open("./test_designs/output/test2.json").unwrap();
		let reader = BufReader::new(file);
		let mapped_design: MappedDesign = serde_json::from_reader(reader).unwrap();
		let mut checked_design = CheckedDesign::new();
		let mut logical_design = LogicalDesign::new();
		let mut physical_design = PhysicalDesign::new();
		let mut serializable_design = SerializableDesign::new();
		checked_design.build_from(&mapped_design);
		logical_design.build_from(&checked_design, &mapped_design);
		physical_design.build_from(&logical_design);
		serializable_design.build_from(&physical_design, &logical_design);
		let blueprint_json = serde_json::to_string(&serializable_design).unwrap();
		println!("{}", blueprint_json);
	}

	#[test]
	fn design_test3() {
		let file = File::open("./test_designs/output/test3.json").unwrap();
		let reader = BufReader::new(file);
		let mapped_design: MappedDesign = serde_json::from_reader(reader).unwrap();
		let mut checked_design = CheckedDesign::new();
		let mut logical_design = LogicalDesign::new();
		let mut physical_design = PhysicalDesign::new();
		let mut serializable_design = SerializableDesign::new();
		checked_design.build_from(&mapped_design);
		logical_design.build_from(&checked_design, &mapped_design);
		physical_design.build_from(&logical_design);
		serializable_design.build_from(&physical_design, &logical_design);
		let blueprint_json = serde_json::to_string(&serializable_design).unwrap();
		println!("{}", blueprint_json);
	}

	#[test]
	fn design_complex_expr() {
		let file = File::open("./test_designs/output/complex_expr.json").unwrap();
		let reader = BufReader::new(file);
		let mapped_design: MappedDesign = serde_json::from_reader(reader).unwrap();
		let mut checked_design = CheckedDesign::new();
		let mut logical_design = LogicalDesign::new();
		let mut physical_design = PhysicalDesign::new();
		let mut serializable_design = SerializableDesign::new();
		checked_design.build_from(&mapped_design);
		logical_design.build_from(&checked_design, &mapped_design);
		physical_design.build_from(&logical_design);
		serializable_design.build_from(&physical_design, &logical_design);
		let blueprint_json = serde_json::to_string(&serializable_design).unwrap();
		println!("{}", blueprint_json);
	}
}

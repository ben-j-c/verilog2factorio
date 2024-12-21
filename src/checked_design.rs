use std::{
	any::Any,
	cell::{self, RefCell},
	collections::{BTreeMap, HashMap, HashSet},
	process::{id, Output},
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
	logical_design::LogicalDesign,
	mapped_design::{CellType, Direction, FromBinStr, Port},
};
use crate::{
	mapped_design::{self, Bit, MappedDesign, PortName},
	signal_lookup_table::lookup_id,
};

pub struct CheckedDesign {
	nodes: Vec<Node>,
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

impl CheckedDesign {
	pub fn new() -> Self {
		Self {
			nodes: vec![],
			cache: RefCell::new(CheckedDesignCache {}),
		}
	}

	fn new_node(&mut self, mapped_id: &str, bits: Vec<Bit>, node_type: NodeType) -> NodeId {
		todo!()
	}

	fn connect(&mut self, send: NodeId, recv: NodeId) {
		if !self.nodes[send].fanout.contains(&recv) {
			self.nodes[send].fanout.push(recv);
			self.nodes[recv].fanin.push(send);
		}
	}

	pub fn build_from(&mut self, mapped_design: &MappedDesign) {
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
		mapped_design.for_all_cells(|_, name, cell| {
			self.new_node(name, vec![], NodeType::CellBody);
		});
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
		let mut attached_map: Vec<Vec<Vec<NodeId>>> = vec![vec![vec![]; 3]; self.nodes.len()];
		for node in &self.nodes {
			match &node.node_type {
				NodeType::CellInput { port } => panic!("Didn't expect to have these yet."),
				NodeType::CellOutput { port } => panic!("Didn't expect to have these yet."),
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
									&& (port.direction == Direction::Output
										|| port.direction == Direction::Input)
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
				NodeType::PortBody => {
					let port = mapped_design.get_port(&node.mapped_id);
					for bit in &port.bits {
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
				}
			}
		}
		let mut nodeid: usize = 0;
		while nodeid < self.nodes.len() {
			let node = &self.nodes[nodeid];
			match &node.node_type {
				NodeType::CellInput { port } => {
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
				NodeType::CellOutput { port } => {
					assert!(attached_map[nodeid][0].len() == 0);
					assert!(attached_map[nodeid][1].len() == 0);
					assert!(attached_map[nodeid][2].len() > 0);
					for to_connect_body_id in attached_map[nodeid][2].iter() {
						let to_connect_id = self.nodes[*to_connect_body_id].fanin[0];
						self.connect(nodeid, to_connect_id);
					}
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
					for to_connect_body_id in attached_map[nodeid][2].iter() {
						let to_connect_id = self.nodes[*to_connect_body_id].fanin[0];
						self.connect(nodeid, to_connect_id);
					}
				}
				NodeType::PortBody => {
					let port = mapped_design.get_port(&node.mapped_id);
					let mapped_id = node.mapped_id.clone();
					if port.direction == Direction::Input || port.direction == Direction::Inout {
						let id = self.new_node(&mapped_id, port.bits.clone(), NodeType::PortOutput);
						self.connect(id, nodeid);
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
					let b = self.new_node(
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
					attached_map.push(vec![vec![], vec![], attached_map[nodeid][0].clone()]);
				}
			}
			nodeid += 1;
		}
	}

	pub fn apply_onto(&self, logical_design: &mut LogicalDesign) {
		for node in &self.nodes {}
	}
}

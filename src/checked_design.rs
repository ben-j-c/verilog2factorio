use std::{
	any::Any,
	cell::RefCell,
	collections::{BTreeMap, HashMap},
	process::id,
	vec,
};

type NodeId = usize;

use crate::mapped_design::FromBinStr;
use crate::{
	mapped_design::{self, Bit, MappedDesign, PortName},
	signal_lookup_table::lookup_id,
};

pub struct CheckedDesign {
	nodes: Vec<Node>,
	cache: RefCell<CheckedDesignCache>,
}

struct CheckedDesignCache {
	index_io: HashMap<PortName, NodeId>,
}

enum CellOrPort {
	Cell(String, String),
	Port(String),
}

fn get_node_bit(mapped_design: &MappedDesign, cell_or_port: &CellOrPort, idx: usize) -> Bit {
	todo!()
}

impl CheckedDesign {
	pub fn new() -> Self {
		Self {
			nodes: vec![],
			cache: RefCell::new(CheckedDesignCache {
				index_io: HashMap::new(),
			}),
		}
	}

	pub fn build_from(&mut self, mapped_design: &MappedDesign) {
		let mut bit_map = HashMap::<u64, Vec<CellOrPort>>::new();
		let mut max_bit: u64 = 0;
		mapped_design.for_all_top_level_io(|_, name, port| {
			let id = self.nodes.len();
			for bit in &port.bits {
				let bitid = match bit {
					Bit::Id(bitid) => bitid,
					_ => continue,
				};
				max_bit = max_bit.max(bitid.0);
				match bit_map.entry(bitid.0) {
					std::collections::hash_map::Entry::Occupied(mut o) => {
						o.get_mut().push(CellOrPort::Port(name.to_owned()))
					}
					std::collections::hash_map::Entry::Vacant(v) => {
						v.insert(vec![]).push(CellOrPort::Port(name.to_owned()))
					}
				};
			}
			if let Some(game_signal) = lookup_id(name) {
				self.cache.borrow_mut().index_io.insert(name.to_owned(), id);
				self.nodes.push(match port.direction {
					mapped_design::Direction::Input => Node::Input {
						id,
						game_signal,
						fanout: vec![],
					},
					mapped_design::Direction::Output => Node::Output {
						id,
						game_signal,
						fanin: NodeId::MAX,
					},
					mapped_design::Direction::Inout => Node::Inout {
						id,
						game_signal,
						fanin: vec![],
						fanout: vec![],
					},
				});
			} else {
				panic!(
					"{:?} on top level design doesn't match a named game signal",
					port
				)
			}
		});
		mapped_design.for_all_cells(|_, name, cell| {
			let id = self.nodes.len();
			let bits_used = match cell.cell_type.as_str() {
				"$mul" => {
					self.nodes.push(Node::Mul {
						id,
						a: NodeId::MAX,
						b: NodeId::MAX,
						y: NodeId::MAX,
						a_width: cell.parameters["A_WIDTH"].from_bin_str().unwrap(),
						b_width: cell.parameters["B_WIDTH"].from_bin_str().unwrap(),
						y_width: cell.parameters["Y_WIDTH"].from_bin_str().unwrap(),
						a_signed: cell.parameters["A_SIGNED"].from_bin_str().unwrap() == 1,
						b_signed: cell.parameters["B_SIGNED"].from_bin_str().unwrap() == 1,
					});
					cell.connections["A"]
						.iter()
						.map(|bit| (bit, "A".to_owned()))
						.chain(
							cell.connections["B"]
								.iter()
								.map(|bit| (bit, "B".to_owned())),
						)
						.chain(
							cell.connections["Y"]
								.iter()
								.map(|bit| (bit, "Y".to_owned())),
						)
				}
				_ => panic!("{:?} can't be implemented", cell),
			};
			for (bit, port) in bits_used {
				let bitid = match bit {
					Bit::Id(bitid) => bitid,
					_ => continue,
				};
				max_bit = max_bit.max(bitid.0);
				match bit_map.entry(bitid.0) {
					std::collections::hash_map::Entry::Occupied(mut o) => {
						o.get_mut().push(CellOrPort::Cell(name.to_owned(), port))
					}
					std::collections::hash_map::Entry::Vacant(v) => v
						.insert(vec![])
						.push(CellOrPort::Cell(name.to_owned(), port)),
				};
			}
		});
		let mut bit_processed = vec![false; (max_bit + 1) as usize];
		for bitid in 0..max_bit as usize {
			if bit_processed[bitid] {
				continue;
			}
			bit_processed[bitid] = true;
			let attached_nodes = bit_map.get(&(bitid as u64)).unwrap();
			let mut idx: Vec<usize> = vec![0; attached_nodes.len()];
			while can_step_idx(idx, attached_nodes) {
				let mut current_bit: Option<mapped_design::Bit> = None;
				for (attached_node_index, local_bit_index) in idx.iter().enumerate() {
					let an = &attached_nodes[attached_node_index];
					current_bit = match current_bit {
						Some(cur_bitid) => current_bit,
						None => Some(get_node_bit(mapped_design, an, *local_bit_index)),
					};
					if current_bit.unwrap() != get_node_bit(mapped_design, an, *local_bit_index) {
						panic!("I only support 32-bit connections")
					}
				}
			}
		}
	}
}

#[derive(Debug)]
enum Node {
	Input {
		id: NodeId,
		game_signal: i32,
		fanout: Vec<NodeId>,
	},
	Output {
		id: NodeId,
		game_signal: i32,
		fanin: Vec<NodeId>,
	},
	Inout {
		id: NodeId,
		game_signal: i32,
		fanin: Vec<NodeId>,
		fanout: Vec<NodeId>,
	},
	Mul {
		id: NodeId,
		a: NodeId,
		b: NodeId,
		y: NodeId,
		a_width: usize,
		b_width: usize,
		y_width: usize,
		a_signed: bool,
		b_signed: bool,
	},
}

use std::{cell::RefCell, collections::HashMap, vec};

type NodeId = usize;

use crate::{
	mapped_design::{self, MappedDesign, PortName},
	signal_lookup_table::lookup_id,
};

pub struct CheckedDesign {
	nodes: Vec<Node>,
	cache: RefCell<CheckedDesignCache>,
}

struct CheckedDesignCache {
	index_io: HashMap<PortName, NodeId>,
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
		mapped_design.for_all_top_level_io(|_, name, port| {
			if let Some(game_signal) = lookup_id(name) {
				let id = self.nodes.len();
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
		mapped_design.for_all_cells(|_, cell| match cell.cell_type.as_str() {
			"$mul" => {
				let id = self.nodes.len();
				self.nodes.push(Node::Mul {
					id,
					a: NodeId::MAX,
					b: NodeId::MAX,
					y: NodeId::MAX,
				});
				cell.connections
			}
			_ => panic!("{:?} can't be implemented", cell),
		});
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
		fanin: NodeId,
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
	},
}

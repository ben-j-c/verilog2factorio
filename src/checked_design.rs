use std::{
	any::Any,
	cell::{self, RefCell},
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

struct CheckedDesignCache {}

enum NodeType {
	CellInput { port: String },
	CellOutput { port: String },
	Input,
	CellBody,
	Output,
	Inout,
}

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

	pub fn build_from(&mut self, mapped_design: &MappedDesign) {
		mapped_design.for_all_top_level_io(|_, name, port| {
			if let Some(_) = lookup_id(name) {
				self.new_node(
					name,
					port.bits.clone(),
					match port.direction {
						mapped_design::Direction::Input => NodeType::Input,
						mapped_design::Direction::Output => NodeType::Output,
						mapped_design::Direction::Inout => NodeType::Inout,
					},
				);
			} else {
				panic!(
					"{:?} on top level design doesn't match a named game signal",
					port
				)
			}
		});
		mapped_design.for_all_cells(|_, name, cell| {
			let id = self.nodes.len();
			match cell.cell_type.as_str() {
				"$mul" => {
					self.new_node(name, vec![], NodeType::CellBody);
				}
				_ => panic!("{:?} can't be implemented", cell),
			};
		})
	}
}

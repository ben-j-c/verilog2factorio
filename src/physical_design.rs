use std::{collections::HashMap, hash::Hash};

use crate::logical_design::{self as ld, LogicalDesign};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct CombinatorId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct WireId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct PoleId(usize);

#[derive(Debug)]
struct Combinator {
	id: CombinatorId,
	logic: ld::NodeId,
	position: (f64, f64),
	orientation: u32,
}

#[derive(Debug)]
struct Wire {
	id: WireId,
	logic: ld::NodeId,
	node1_id: CombinatorId,
	node2_id: CombinatorId,
	terminal1_id: u32,
	terminal2_id: u32,
}

#[derive(Debug)]
struct Pole {
	id: PoleId,
	logic: ld::NodeId,
	position: (f64, f64),
}

#[derive(Debug)]
struct PhysicalDesign {
	combs: Vec<Combinator>,
	wires: Vec<Wire>,
	poles: Vec<Pole>,

	idx_combs: HashMap<ld::NodeId, CombinatorId>,
	idx_wires: HashMap<ld::NodeId, WireId>,
	idx_poles: HashMap<ld::NodeId, PoleId>,
	idx_space: HashMap<(i32, i32), ld::NodeId>,

	logical: ld::LogicalDesign,
}

impl PhysicalDesign {
	pub fn new(logical: LogicalDesign) -> Self {
		let mut combs = vec![];
		let mut idx_combs = HashMap::new();
		logical.for_all(|_, ld_node| match &ld_node.function {
			ld::NodeFunction::Arithmetic { .. }
			| ld::NodeFunction::Decider { .. }
			| ld::NodeFunction::Constant { .. }
			| ld::NodeFunction::Lamp { .. } => {
				let id = CombinatorId(combs.len());
				combs.push(Combinator {
					id,
					logic: ld_node.id,
					position: (0.0, 0.0), // Later we spread them out
					orientation: 4,       // This shouldn't have to change
				});
				idx_combs.insert(ld_node.id, id);
			}
			ld::NodeFunction::WireSum => { /* Do nothing for now */ }
		});

		let mut idx_space = HashMap::new();
		logical.for_all_topological_order(|ld_node| {
			match ld_node.function {
				ld::NodeFunction::Arithmetic { .. }
				| ld::NodeFunction::Decider { .. }
				| ld::NodeFunction::Constant { .. }
				| ld::NodeFunction::Lamp { .. } => {}
				ld::NodeFunction::WireSum => { /* Do nothing for now */ }
			}
		});

		let ret = PhysicalDesign {
			combs,
			wires: vec![],
			poles: vec![],
			idx_combs,
			idx_wires: HashMap::new(),
			idx_poles: HashMap::new(),
			idx_space,
			logical,
		};
		ret
	}
}

use std::{
	cell::RefCell,
	cmp::max,
	collections::{HashMap, HashSet, LinkedList},
	hash::Hash,
};

use crate::mapped_design::{self, MappedDesign};

#[derive(Debug, Clone)]
pub enum ArithmeticOperator {
	Mult,
	Div,
	Add,
	Sub,
	Mod,
	Exp,
	Sll,
	Srl,
	And,
	Or,
	Xor,
}

#[derive(Debug, Clone)]
pub enum DeciderOperator {
	LessThan,
	GreaterThan,
	Equal,
	NotEqual,
	GreaterThanEqual,
	LessThanEqual,
}

#[derive(Debug, Clone)]
pub enum DeciderRowConjDisj {
	And,
	Or,
	FirstRow,
}

#[derive(Debug, Clone)]
pub enum Signal {
	Virtual(i32),
	Physical(i32),
	Everything,
	Anything,
	Each,
	Constant(i32),
	None,
}

#[derive(Debug, Clone)]
pub enum NodeFunction {
	Arithmetic {
		op: ArithmeticOperator,
		input_1: Signal,
		input_2: Signal,
	},
	Decider {
		expressions: Vec<(Signal, DeciderOperator, Signal)>,
		expression_conj_disj: Vec<DeciderRowConjDisj>,
		use_input_count: Vec<bool>,
	},
	Constant {
		enabled: bool,
		constants: Vec<i32>,
	},
	Lamp {
		expression: (Signal, DeciderOperator, Signal),
	},
	WireSum,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeId(usize);

impl From<NodeId> for usize {
	fn from(v: NodeId) -> Self {
		v.0
	}
}

impl From<usize> for NodeId {
	fn from(v: usize) -> Self {
		NodeId(v)
	}
}

#[derive(Debug, Clone)]
pub struct Node {
	pub id: NodeId,
	pub function: NodeFunction,
	pub fanin: Vec<NodeId>,
	pub fanout: Vec<NodeId>,
	pub output: Vec<Signal>,
}

struct LogicalDesignCache {
	topological_order: Vec<NodeId>,
	root_nodes: Vec<NodeId>,
	leaf_nodes: Vec<NodeId>,
	depth: HashMap<NodeId, i32>,
	max_depth: i32,
	rev_depth: HashMap<NodeId, i32>,
	idx_depth: HashMap<i32, Vec<NodeId>>,
	idx_rev_depth: HashMap<i32, Vec<NodeId>>,
	valid: bool,
}

pub struct LogicalDesign {
	nodes: Vec<Node>,
	cache: RefCell<LogicalDesignCache>,
}

impl std::fmt::Debug for LogicalDesign {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("LogicalDesign")
			.field("nodes", &self.nodes)
			.finish()
	}
}

impl LogicalDesign {
	pub fn new() -> Self {
		LogicalDesign {
			nodes: vec![],
			cache: RefCell::new(LogicalDesignCache {
				topological_order: vec![],
				root_nodes: vec![],
				leaf_nodes: vec![],
				depth: HashMap::new(),
				max_depth: 0,
				rev_depth: HashMap::new(),
				idx_depth: HashMap::new(),
				idx_rev_depth: HashMap::new(),
				valid: true,
			}),
		}
	}

	pub fn build_from(&mut self, mapped_design: &MappedDesign) {}

	pub fn add_node(&mut self, function: NodeFunction, output: Vec<Signal>) -> NodeId {
		self.cache.get_mut().valid = false;
		let id = NodeId(self.nodes.len());
		self.nodes.push(Node {
			id,
			function,
			output,
			fanin: vec![],
			fanout: vec![],
		});
		id
	}

	pub fn connect(&mut self, out_node: NodeId, in_node: NodeId) {
		self.cache.get_mut().valid = false;
		self.nodes[out_node.0].fanout.push(in_node);
		self.nodes[in_node.0].fanin.push(out_node);
	}

	pub fn add_arithmetic_comb(
		&mut self,
		expr: (Signal, ArithmeticOperator, Signal),
		output: Signal,
	) -> NodeId {
		self.add_node(
			NodeFunction::Arithmetic {
				op: expr.1,
				input_1: expr.0,
				input_2: expr.2,
			},
			vec![output],
		)
	}

	pub fn add_decider_comb(&mut self) -> NodeId {
		self.add_node(
			NodeFunction::Decider {
				expressions: vec![],
				expression_conj_disj: vec![],
				use_input_count: vec![],
			},
			vec![],
		)
	}

	pub fn add_decider_comb_input(
		&mut self,
		id: NodeId,
		expr: (Signal, DeciderOperator, Signal),
		conj_disj: DeciderRowConjDisj,
	) {
		match &mut self.nodes[id.0].function {
			NodeFunction::Decider {
				expressions,
				expression_conj_disj,
				..
			} => {
				expressions.push(expr);
				expression_conj_disj.push(conj_disj)
			}
			_ => assert!(
				false,
				"Tried to add DeciderCombinator output to non DeciderCombinator node"
			),
		}
	}

	pub fn add_decider_comb_output(
		&mut self,
		id: NodeId,
		output: Signal,
		use_input_count_for_output: bool,
	) {
		match &mut self.nodes[id.0].function {
			NodeFunction::Decider {
				use_input_count, ..
			} => {
				use_input_count.push(use_input_count_for_output);
				self.nodes[id.0].output.push(output);
			}
			_ => assert!(
				false,
				"Tried to add DeciderCombinator output to non-DeciderCombinator node"
			),
		}
	}

	pub fn add_constant_comb(&mut self, output: Vec<Signal>, counts: Vec<i32>) -> NodeId {
		assert_eq!(
			output.len(),
			counts.len(),
			"Tried to crea constant combinator with mismatched outputs and counts"
		);
		self.add_node(
			NodeFunction::Constant {
				enabled: true,
				constants: counts,
			},
			output,
		)
	}

	pub fn add_lamp(&mut self, expr: (Signal, DeciderOperator, Signal)) -> NodeId {
		self.add_node(NodeFunction::Lamp { expression: expr }, vec![])
	}

	pub fn add_wire(&mut self, fanin: Vec<NodeId>, fanout: Vec<NodeId>) -> NodeId {
		let id = self.add_node(NodeFunction::WireSum, vec![Signal::Everything]);
		for node_in in fanin {
			self.connect(node_in, id);
		}
		for node_out in fanout {
			self.connect(id, node_out);
		}
		id
	}

	pub fn add_wire_floating(&mut self) -> NodeId {
		self.add_node(NodeFunction::WireSum, vec![Signal::Everything])
	}

	pub fn for_all<F>(&self, mut func: F)
	where
		F: FnMut(&Self, &Node),
	{
		for node in &self.nodes {
			func(self, node);
		}
	}

	fn update_cache(&self) {
		if self.cache.borrow().valid {
			return;
		}
		let mut topo_seen = HashSet::new();
		let mut topological_order = vec![];
		let mut root_nodes = vec![];
		let mut leaf_nodes = vec![];
		let mut depth = HashMap::new();
		let mut global_max_depth = -1;
		let mut idx_depth = HashMap::<i32, Vec<NodeId>>::new();

		for node in &self.nodes {
			if node.fanin.is_empty() {
				root_nodes.push(node.id);
			}
			if node.fanout.is_empty() {
				leaf_nodes.push(node.id);
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
			let mut max_depth = -1;
			for fiid in &self.nodes[id.0].fanin {
				if let Some(d) = depth.get(fiid) {
					max_depth = max(max_depth, *d);
				}
			}
			depth.insert(id, max_depth + 1);
			global_max_depth = max(max_depth + 1, global_max_depth);
			match idx_depth.entry(max_depth + 1) {
				std::collections::hash_map::Entry::Occupied(mut e) => {
					e.get_mut().push(id);
				}
				std::collections::hash_map::Entry::Vacant(e) => {
					e.insert(vec![id]);
				}
			};
			for foid in &self.nodes[id.0].fanout {
				if self.nodes[foid.0]
					.fanin
					.iter()
					.all(|fiid| topo_seen.contains(fiid))
				{
					queue.push_back(*foid);
				}
			}
		}

		let mut rev_depth = HashMap::new();
		let mut idx_rev_depth = HashMap::<i32, Vec<NodeId>>::new();
		topo_seen.clear();
		for id in &leaf_nodes {
			queue.push_back(*id);
		}
		while !queue.is_empty() {
			let id = queue.pop_front().unwrap();
			topo_seen.insert(id);
			let mut max_depth = -1;
			for foid in &self.nodes[id.0].fanout {
				if let Some(d) = rev_depth.get(foid) {
					max_depth = max(max_depth, *d);
				}
			}
			rev_depth.insert(id, max_depth + 1);
			match idx_rev_depth.entry(max_depth + 1) {
				std::collections::hash_map::Entry::Occupied(mut e) => {
					e.get_mut().push(id);
				}
				std::collections::hash_map::Entry::Vacant(e) => {
					e.insert(vec![id]);
				}
			};
			for fiid in &self.nodes[id.0].fanin {
				if self.nodes[fiid.0]
					.fanout
					.iter()
					.all(|foid| topo_seen.contains(foid))
				{
					queue.push_back(*fiid);
				}
			}
		}

		self.cache.replace(LogicalDesignCache {
			topological_order,
			root_nodes,
			leaf_nodes,
			depth,
			max_depth: global_max_depth,
			rev_depth,
			idx_depth,
			idx_rev_depth,
			valid: true,
		});
	}

	pub fn for_all_topological_order<F>(&self, mut func: F)
	where
		F: FnMut(&Node),
	{
		self.update_cache();
		for node in &self.cache.borrow().topological_order {
			func(&self.nodes[node.0]);
		}
	}

	pub fn for_all_depth<F>(&self, depth: i32, mut func: F)
	where
		F: Fn(&Node),
	{
		self.update_cache();
		for node in &self.cache.borrow().idx_depth[&depth] {
			func(&self.nodes[node.0]);
		}
	}

	pub fn for_all_rev_depth<F>(&self, depth: i32, mut func: F)
	where
		F: Fn(&Node),
	{
		self.update_cache();
		for node in &self.cache.borrow().idx_rev_depth[&depth] {
			func(&self.nodes[node.0]);
		}
	}

	pub fn max_depth(&self) -> i32 {
		self.update_cache();
		return self.cache.borrow().max_depth;
	}

	pub fn for_all_roots<F>(&self, mut func: F)
	where
		F: FnMut(&Node),
	{
		self.update_cache();
		for node in &self.cache.borrow().root_nodes {
			func(&self.nodes[node.0]);
		}
	}

	pub fn get_rev_depth(&self, id: NodeId) -> i32 {
		*self.cache.borrow().rev_depth.get(&id).unwrap()
	}

	pub fn get_node(&self, id: NodeId) -> &Node {
		&self.nodes[id.0]
	}

	pub fn mut_node(&mut self, id: NodeId) -> &mut Node {
		self.cache.borrow_mut().valid = false;
		&mut self.nodes[id.0]
	}
}

#[cfg(test)]
pub fn get_simple_logical_design() -> LogicalDesign {
	let mut d = LogicalDesign::new();
	d
}

#[cfg(test)]
mod test {
	use super::*;
	use ArithmeticOperator as Aop;
	use DeciderOperator as Dop;
	use Signal as Sig;

	#[test]
	fn new() {
		let d = LogicalDesign::new();
		assert_eq!(d.nodes.len(), 0);
	}

	#[test]
	fn single_combinator() {
		let mut d = LogicalDesign::new();
		let lamp = d.add_lamp((Sig::Virtual(0), Dop::Equal, Sig::Virtual(1)));
		assert_eq!(d.nodes.len(), 1);
		assert_eq!(lamp.0, 0);
		assert_eq!(d.nodes[lamp.0].id, lamp);
	}

	#[test]
	fn output_to_input() {
		let mut d = LogicalDesign::new();
		let constant = d.add_constant_comb(vec![Sig::Virtual(2)], vec![100]);
		let lamp = d.add_lamp((Sig::Virtual(2), Dop::Equal, Sig::Constant(100)));
		d.connect(constant, lamp);
		assert_eq!(d.nodes.len(), 2);

		assert_eq!(constant.0, 0);
		assert_eq!(lamp.0, 1);

		assert_eq!(d.nodes[constant.0].id, constant);
		assert_eq!(d.nodes[lamp.0].id, lamp);

		assert!(d.nodes[constant.0].fanout.contains(&lamp));
		assert!(d.nodes[lamp.0].fanin.contains(&constant));

		assert_eq!(d.nodes[constant.0].fanout.len(), 1);
		assert_eq!(d.nodes[lamp.0].fanin.len(), 1);

		d.for_all_depth(0, |n| {
			assert_eq!(n.id, constant);
		});

		d.for_all_depth(1, |n| {
			assert_eq!(n.id, lamp);
		});

		d.for_all_rev_depth(0, |n| {
			assert_eq!(n.id, lamp);
		});

		d.for_all_rev_depth(1, |n| {
			assert_eq!(n.id, constant);
		});

		d.for_all_roots(|n| {
			assert_eq!(n.id, constant);
		});

		let mut id = 0;
		d.for_all_topological_order(|n| {
			assert_eq!(n.id.0, id);
			id += 1;
		});

		assert_eq!(d.get_node(constant).output.len(), 1);
	}
}

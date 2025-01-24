//! This module is the core API that a user should use to create combinator designs.
//! Here is an example usage to multiply the outputs from two constants and output to a lamp.
//! ```rust
//! use ArithmeticOperator as Aop;
//! use DeciderOperator as Dop;
//! use Signal as Sig;
//! let mut d = LogicalDesign::new(); // 1
//! let constant1 = d.add_constant_comb(vec![Sig::Id(0)], vec![100]); //2
//! let constant2 = d.add_constant_comb(vec![Sig::Id(1)], vec![4]);
//! let mult = d.add_arithmetic_comb((Sig::Id(1), Aop::Mult, Sig::Id(0)), Sig::Id(10)); //3
//! let lamp = d.add_lamp((Sig::Id(10), Dop::Equal, Sig::Constant(400)));
//! let _wire_pre_mult = d.add_wire_red(vec![constant1, constant2], vec![mult]); // 4
//! let _wire_post_mult = d.add_wire_red(vec![mult], vec![lamp]);
//! ```
//! To elaborate on this example:
//!   1. Create a new blank design.
//!   2. Add a constant combinator that outputs a `signal-0` with a value 100. Here only a single output is specified, but you can easily add more output signals by including extra elements in both vectors.
//!   3. Create an arithmetic combinator with the expression `signal-A := signal-1 * signal-0`
//!   4. Create a wire that connects the outputs from `constant1` and `constant2` to the input of `mult`
//!
//! See [Signal] for more information about the exact meanings of signal IDs.
//!
//! See [LogicalDesign] for more details on how to build designs.

use std::{
	cell::RefCell,
	cmp::max,
	collections::{BTreeSet, HashMap, HashSet, LinkedList},
	hash::Hash,
	slice::Iter,
	usize,
};

use crate::{
	checked_design::CheckedDesign,
	connected_design::CoarseExpr,
	mapped_design::{BitSliceOps, MappedDesign},
};

// Supported Arithmetic Combinator operations as in the game.
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

// Supported Decider Combinator operations as in the game.
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum DeciderOperator {
	LessThan,
	GreaterThan,
	Equal,
	NotEqual,
	GreaterThanEqual,
	LessThanEqual,
}

/// Each Decider condition must have one of these enums. The first row must have `FirstRow`
/// Subsequent rows can have either `And` (conjunction) or `Or` (disjunction).
/// Each row is considered an element in an expression that follows order of operations
/// it follows that all conjunctions will be executed, then disjunctions. e.g, the sequence
/// [FirstRow, And, And, Or, And] is equivalent to "a AND b AND c OR d AND e"
///
/// This enum can be thought of as an OR or an AND prefix to every expression in the decider.
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum DeciderRowConjDisj {
	And,
	Or,
	FirstRow,
}

/// Signals supported by the game.
///
/// See [crate::signal_lookup_table] for exact mapping between ids and names in game.
#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum Signal {
	/// Regular signal representing virtual or physical signals.
	Id(i32),
	/// The everything signal. Game specific meaning.
	Everything,
	/// The anything signal. Game specific meaning.
	Anything,
	/// The each signal. Game specific meaning.
	Each,
	/// A constant found on the right hand side of an expression in either a decider or arithmetic combinator.
	Constant(i32),
	/// An unset signal. i.e., a blank spot on the left hand side of an expression in a combinator.
	None,
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum WireColour {
	Red,
	Green,
}

/// All nodes in a design have a function. For combinators/lamps, these map one-to-one to the game. Wires represent connectivity between combinators/lamps.
/// The design must have a network matching any of these patterns:
///
/// Network :=
///		"" // the empty network
///		Comb // a single combinator
///		Comb -> WireSum -> {Network} // A combinator prefixed to a network
///		{Network} -> WireSum -> Comb // A combinator postfixed to a network
///
/// A WireSum and Combinator can have as many fanin/fanout as they need.
/// Rules for wires:
///  - A WireSum colour must match the terminals it is attached to.
///  - A WireSum must be connected between combinators, combinators can not connect directly to eachother.
///
/// Note: I may or may not check any of these invariants. I am too lazy to find it in the code. Some things might fail, some things may not until you import and realize its wrong.
#[derive(Debug, Clone)]
pub enum NodeFunction {
	/// Game entity.
	Arithmetic {
		op: ArithmeticOperator,
		input_1: Signal,
		input_2: Signal,
	},
	/// Game entity.
	Decider {
		expressions: Vec<(Signal, DeciderOperator, Signal)>,
		expression_conj_disj: Vec<DeciderRowConjDisj>,
		use_input_count: Vec<bool>,
	},
	/// Game entity.
	Constant { enabled: bool, constants: Vec<i32> },
	/// Game entity.
	Lamp {
		expression: (Signal, DeciderOperator, Signal),
	},
	/// All fanin/fanout of this node will be connected with a wire of this colour. A single node can have multiple
	WireSum(WireColour),
}

/// An id that is unique in a design. Can be used to uniquely identify a node in a design. Use this id to make connections.
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

/// The internal representation of a design. No warranty on changing these directly.
#[derive(Debug, Clone)]
pub struct Node {
	pub id: NodeId,
	pub function: NodeFunction,
	pub fanin_red: Vec<NodeId>,
	pub fanout_red: Vec<NodeId>,
	pub fanin_green: Vec<NodeId>,
	pub fanout_green: Vec<NodeId>,
	pub output: Vec<Signal>,

	pub description: Option<String>,
}

impl Node {
	/// Iterate the fanin of a specific colour.
	pub fn iter_fanin(&self, colour: WireColour) -> Iter<NodeId> {
		match colour {
			WireColour::Red => self.fanin_red.iter(),
			WireColour::Green => self.fanin_green.iter(),
		}
	}

	/// Iterate the fanout of a specific colour.
	pub fn iter_fanout(&self, colour: WireColour) -> Iter<NodeId> {
		match colour {
			WireColour::Red => self.fanout_red.iter(),
			WireColour::Green => self.fanout_green.iter(),
		}
	}

	/// Iterate the fanout of both colours.
	pub fn iter_fanout_both(
		&self,
	) -> std::iter::Chain<std::slice::Iter<'_, NodeId>, std::slice::Iter<'_, NodeId>> {
		self.iter_fanout(WireColour::Red)
			.chain(self.iter_fanout(WireColour::Green))
	}

	/// Iterate the fanin of both colours.
	pub fn iter_fanin_both(
		&self,
	) -> std::iter::Chain<std::slice::Iter<'_, NodeId>, std::slice::Iter<'_, NodeId>> {
		self.iter_fanin(WireColour::Red)
			.chain(self.iter_fanin(WireColour::Green))
	}

	/// Does this node not have any nodes that feed into it?
	pub fn fanin_empty(&self) -> bool {
		self.fanin_green.is_empty() && self.fanin_red.is_empty()
	}

	/// Does this node not feed into any nodes?
	pub fn fanout_empty(&self) -> bool {
		self.fanout_green.is_empty() && self.fanout_red.is_empty()
	}
}

/// Performance? never heard of her.
#[allow(dead_code)]
struct LogicalDesignCache {
	topological_order: Vec<NodeId>,
	root_nodes: Vec<NodeId>,
	leaf_nodes: Vec<NodeId>,
	depth: Vec<i32>,
	max_depth: i32,
	rev_depth: Vec<i32>,
	idx_depth: HashMap<i32, Vec<NodeId>>,
	idx_rev_depth: HashMap<i32, Vec<NodeId>>,
	valid: bool,
}

/// A design you want to build up and save.
pub struct LogicalDesign {
	pub(crate) nodes: Vec<Node>,
	cache: RefCell<LogicalDesignCache>,
	pub(crate) description: String,
}

impl std::fmt::Debug for LogicalDesign {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("LogicalDesign")
			.field("nodes", &self.nodes)
			.finish()
	}
}

#[allow(dead_code)]
impl LogicalDesign {
	/// New blank design.
	pub fn new() -> Self {
		LogicalDesign {
			nodes: vec![],
			cache: RefCell::new(LogicalDesignCache {
				topological_order: vec![],
				root_nodes: vec![],
				leaf_nodes: vec![],
				depth: vec![],
				max_depth: 0,
				rev_depth: vec![],
				idx_depth: HashMap::new(),
				idx_rev_depth: HashMap::new(),
				valid: true,
			}),
			description: "".to_string(),
		}
	}

	/// Automatically build from a [`CheckedDesign`]
	pub fn build_from(&mut self, checked_design: &CheckedDesign, mapped_design: &MappedDesign) {
		checked_design.apply_onto(self, mapped_design); // lmfao
	}

	/// Add a node with the specific [`NodeFunction`] and the specific output [`Signal`]. Generally a low level API.
	pub fn add_node(&mut self, function: NodeFunction, output: Vec<Signal>) -> NodeId {
		self.cache.get_mut().valid = false;
		let id = NodeId(self.nodes.len());
		self.nodes.push(Node {
			id,
			function,
			output,
			fanin_red: vec![],
			fanout_red: vec![],
			fanin_green: vec![],
			fanout_green: vec![],
			description: None,
		});
		id
	}

	/// Connect the red wires from `out_node` to `in_node`. e.g., if `out_node` is an arithmetic combinator and `in_node` is a red wire, then the red terminal on the comb will be attached to a wire.
	/// A subsequent call to this function where that red wire is now the `out_node` and some other combinator is the `in_node` will result in the arithmetic comb having an output wire
	/// attached to the input of the second comb.
	pub fn connect_red(&mut self, out_node: NodeId, in_node: NodeId) {
		self.cache.get_mut().valid = false;
		self.nodes[out_node.0].fanout_red.push(in_node);
		self.nodes[in_node.0].fanin_red.push(out_node);
	}

	/// See [`connect_red`] but replace red with green.
	pub fn connect_green(&mut self, out_node: NodeId, in_node: NodeId) {
		self.cache.get_mut().valid = false;
		self.nodes[out_node.0].fanout_green.push(in_node);
		self.nodes[in_node.0].fanin_green.push(out_node);
	}

	/// Add a arithmetic combinator to this design. The expression matches what you see in game.
	///
	/// Returns the id for that new combinator.
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

	/// Add a combinator fitting the pattern that I call "no-operation". Converts the input signal to an output signal using an arithmetic combinator.
	///
	/// Returns the id for that new combinator.
	pub fn add_nop(&mut self, input: Signal, output: Signal) -> NodeId {
		self.add_node(
			NodeFunction::Arithmetic {
				op: ArithmeticOperator::Add,
				input_1: input,
				input_2: Signal::Constant(0),
			},
			vec![output],
		)
	}

	/// Add a combinator fitting the pattern that I call "negation". Equivalent to `!x` in Rust.
	///
	/// Returns the id for that new combinator.
	pub fn add_neg(&mut self, input: Signal, output: Signal) -> NodeId {
		let ret = self.add_decider_comb();
		self.add_decider_comb_input(
			ret,
			(input, DeciderOperator::Equal, Signal::Constant(0)),
			DeciderRowConjDisj::FirstRow,
		);
		self.add_decider_comb_output(ret, output, false);
		ret
	}

	/** Inputs are returned as wires, outputs are retured as combinators. */
	///
	/// The returned tuple is (D wire id, CLK wire id, Output combinator id)
	pub fn add_latch(&mut self, data: Signal, clk: Signal) -> (NodeId, NodeId, NodeId) {
		let in_control = {
			let ic = self.add_decider_comb();
			self.add_decider_comb_input(
				ic,
				(clk, DeciderOperator::GreaterThan, Signal::Constant(0)),
				DeciderRowConjDisj::FirstRow,
			);
			self.add_decider_comb_output(ic, data, true);
			ic
		};

		let mem_cell = {
			let mc = self.add_decider_comb();
			self.add_decider_comb_input(
				mc,
				(clk, DeciderOperator::Equal, Signal::Constant(0)),
				DeciderRowConjDisj::FirstRow,
			);
			self.add_decider_comb_output(mc, data, true);
			mc
		};

		let clk_wire = self.add_wire_green(vec![], vec![in_control, mem_cell]);
		self.add_wire_red(vec![in_control, mem_cell], vec![mem_cell]);
		let data_in_wire = self.add_wire_red(vec![], vec![in_control]);

		(data_in_wire, clk_wire, mem_cell)
	}

	/** Create a standard D-Flip-Flop in this design. Returned NodeIds match the order of the function signature.
	Inputs are returned as wires, outputs are retured as combinators. */
	pub fn add_dff(
		&mut self,
		input: Signal,
		clk: Signal,
		output: Signal,
	) -> (NodeId, NodeId, NodeId) {
		let clk_buf_1 = self.add_neg(clk, clk);
		let clk_buf_2 = self.add_nop(clk, clk);
		let (latch_in_wire_1, clk_wire_1, latch_out_1) = self.add_latch(input, clk);
		let (latch_in_wire_2, clk_wire_2, latch_out_2) = self.add_latch(input, clk);

		self.connect_red(latch_out_1, latch_in_wire_2);
		self.connect_green(clk_buf_1, clk_wire_1);
		self.connect_green(clk_buf_2, clk_wire_2);
		let clk_wire = self.add_wire_red(vec![], vec![clk_buf_1, clk_buf_2]);

		if input == output {
			(latch_in_wire_1, clk_wire, latch_out_2)
		} else {
			let final_out = self.add_nop(input, output);
			self.add_wire_red(vec![latch_out_2], vec![final_out]);
			(latch_in_wire_1, clk_wire, final_out)
		}
	}

	/// A pattern for bit manipulations. ehhhhh too lazy to spec it now. Raise an issue if you want me to.
	pub fn add_swizzle(
		&mut self,
		input: Vec<Signal>,
		fi_exprs: Vec<Option<CoarseExpr>>,
		output: Signal,
	) -> (Vec<NodeId>, NodeId) {
		let mut retval = Vec::with_capacity(fi_exprs.len());
		let mut last_comb = None;
		for (idx, expr_opt) in fi_exprs.iter().enumerate() {
			if expr_opt.is_none() {
				// A constant
				let constant_nop = self.add_nop(input[idx], output);
				retval.push(self.add_wire_red(vec![], vec![constant_nop]));
				if let Some(prev) = last_comb {
					self.add_wire_red(vec![prev, constant_nop], vec![]);
				}
				last_comb = Some(constant_nop);
				continue;
			}
			let (mask, shift) = expr_opt.as_ref().unwrap().unwrap_mask_shift();
			let mask_comb = self.add_arithmetic_comb(
				(input[idx], ArithmeticOperator::And, Signal::Constant(mask)),
				output,
			);
			retval.push(self.add_wire_red(vec![], vec![mask_comb]));
			if shift == 0 {
				if let Some(prev) = last_comb {
					self.add_wire_red(vec![prev, mask_comb], vec![]);
				}
				last_comb = Some(mask_comb);
			} else {
				let shift_comb = self.add_arithmetic_comb(
					(
						output,
						if shift > 0 {
							ArithmeticOperator::Sll
						} else {
							ArithmeticOperator::Srl
						},
						Signal::Constant(shift.abs()),
					),
					output,
				);
				self.add_wire_red(vec![mask_comb], vec![shift_comb]);
				if let Some(prev) = last_comb {
					self.add_wire_red(vec![prev, shift_comb], vec![]);
				}
				last_comb = Some(shift_comb);
			}
		}
		(retval, last_comb.unwrap())
	}

	/// Add an empty Decider Combinator to this design. You can then configure its input rows and outputs using [`add_decider_comb_input`] and [`add_decider_comb_output].
	///
	/// Returns the id for that new combinator.
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

	/// Add an input row to the specified decider combinator. Each row is given as a (Signal, DeciderOperator, Signal),
	/// and it is combined with previous rows according to the specified [DeciderRowConjDisj].
	/// This allows you to build complex logical expressions spanning multiple signals and conditions.
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

	/// Add an output signal to the specified decider combinator. The decider will output the given signal if all configured
	/// input rows, combined under their [DeciderRowConjDisj] logic, evaluate to true.
	/// If `use_input_count_for_output` is true, the combinator outputs the count of matched inputs; otherwise, it outputs 1.
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

	/// Add a new constant combinator to this design, with the specified output signals and their matching constant values.
	/// For example, (vec![Signal::Id(0)], vec![100]) defines a combinator that always output 100 for the virtual signal, signal-0.
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

	/// Add a lamp entity to this design, configured to light up based on the provided condition tuple (Signal, DeciderOperator, Signal).
	/// When the condition is satisfied, the lamp will glow in-game.
	pub fn add_lamp(&mut self, expr: (Signal, DeciderOperator, Signal)) -> NodeId {
		self.add_node(NodeFunction::Lamp { expression: expr }, vec![])
	}

	/// It has the same behaviour as a wire in game. fanin/fanout MUST be anything other than another wire.
	///
	/// Returns the id for the wire you created.
	pub fn add_wire_red(&mut self, fanin: Vec<NodeId>, fanout: Vec<NodeId>) -> NodeId {
		let id = self.add_node(
			NodeFunction::WireSum(WireColour::Red),
			vec![Signal::Everything],
		);
		for node_in in fanin {
			self.connect_red(node_in, id);
		}
		for node_out in fanout {
			self.connect_red(id, node_out);
		}
		id
	}

	/// It has the same behaviour as a wire in game. fanin/fanout MUST be anything other than another wire.
	///
	/// Returns the id for the wire you created.
	pub fn add_wire_green(&mut self, fanin: Vec<NodeId>, fanout: Vec<NodeId>) -> NodeId {
		let id = self.add_node(
			NodeFunction::WireSum(WireColour::Green),
			vec![Signal::Everything],
		);
		for node_in in fanin {
			self.connect_green(node_in, id);
		}
		for node_out in fanout {
			self.connect_green(id, node_out);
		}
		id
	}

	/// See [`add_wire_red`]
	pub fn add_wire_floating_red(&mut self) -> NodeId {
		self.add_node(
			NodeFunction::WireSum(WireColour::Red),
			vec![Signal::Everything],
		)
	}

	pub(crate) fn for_all<F>(&self, mut func: F)
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
		let mut depth = vec![0; self.nodes.len()];
		let mut global_max_depth = -1;
		let mut idx_depth = HashMap::<i32, Vec<NodeId>>::new();

		for node in &self.nodes {
			if node.fanin_empty() {
				root_nodes.push(node.id);
			}
		}
		while topo_seen.len() != self.nodes.len() {
			let mut queue = LinkedList::new();
			for id in &root_nodes {
				queue.push_back(*id);
			}
			while !queue.is_empty() {
				let id = queue.pop_front().unwrap();
				if topo_seen.contains(&id) {
					continue;
				}
				topo_seen.insert(id);
				topological_order.push(id);
				let mut max_depth = -1;
				for fiid in self.nodes[id.0].iter_fanin_both() {
					max_depth = max(max_depth, depth[fiid.0]);
				}
				depth[id.0] = max_depth + 1;
				global_max_depth = max(max_depth + 1, global_max_depth);
				match idx_depth.entry(max_depth + 1) {
					std::collections::hash_map::Entry::Occupied(mut e) => {
						e.get_mut().push(id);
					}
					std::collections::hash_map::Entry::Vacant(e) => {
						e.insert(vec![id]);
					}
				};
				for foid in self.nodes[id.0].iter_fanout_both() {
					if self.nodes[foid.0]
						.iter_fanin_both()
						.all(|fiid| (topo_seen.contains(fiid)))
					{
						queue.push_back(*foid);
					}
				}
			}
			root_nodes.clear();
			for id in &topo_seen {
				for foid in self.nodes[id.0].iter_fanout_both() {
					if !topo_seen.contains(foid) {
						root_nodes.push(*foid);
					}
				}
			}
		}
		assert_eq!(topo_seen.len(), self.nodes.len());

		root_nodes.clear();
		leaf_nodes.clear();
		for node in &self.nodes {
			if node.fanin_empty() {
				root_nodes.push(node.id);
			}
			if node.fanout_empty() {
				leaf_nodes.push(node.id);
			}
		}
		let mut rev_depth = vec![0; self.nodes.len()];
		let mut idx_rev_depth = HashMap::<i32, Vec<NodeId>>::new();
		topo_seen.clear();
		while topo_seen.len() != self.nodes.len() {
			let mut queue = LinkedList::new();
			for id in &leaf_nodes {
				queue.push_back(*id);
			}
			while !queue.is_empty() {
				let id = queue.pop_front().unwrap();
				if topo_seen.contains(&id) {
					continue;
				}
				topo_seen.insert(id);
				let mut max_depth = -1;
				for foid in self.nodes[id.0].iter_fanout_both() {
					max_depth = max(max_depth, rev_depth[foid.0]);
				}
				rev_depth[id.0] = max_depth + 1;
				match idx_rev_depth.entry(max_depth + 1) {
					std::collections::hash_map::Entry::Occupied(mut e) => {
						e.get_mut().push(id);
					}
					std::collections::hash_map::Entry::Vacant(e) => {
						e.insert(vec![id]);
					}
				};
				for fiid in self.nodes[id.0].iter_fanin_both() {
					if self.nodes[fiid.0]
						.iter_fanout_both()
						.all(|foid| topo_seen.contains(foid))
					{
						queue.push_back(*fiid);
					}
				}
			}
			leaf_nodes.clear();
			for id in &topo_seen {
				for fiid in self.nodes[id.0].iter_fanin_both() {
					if !topo_seen.contains(fiid) {
						leaf_nodes.push(*fiid);
					}
				}
			}
		}

		assert_eq!(topo_seen.len(), self.nodes.len());

		leaf_nodes.clear();
		for node in &self.nodes {
			if node.fanout_empty() {
				leaf_nodes.push(node.id);
			}
		}

		println!("{:?}", topological_order);

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

	pub(crate) fn for_all_topological_order<F>(&self, mut func: F)
	where
		F: FnMut(&Node),
	{
		self.update_cache();
		for node in &self.cache.borrow().topological_order {
			func(&self.nodes[node.0]);
		}
	}

	pub(crate) fn for_all_depth<F>(&self, depth: i32, func: F)
	where
		F: Fn(&Node),
	{
		self.update_cache();
		for node in &self.cache.borrow().idx_depth[&depth] {
			func(&self.nodes[node.0]);
		}
	}

	pub(crate) fn for_all_rev_depth<F>(&self, depth: i32, func: F)
	where
		F: Fn(&Node),
	{
		self.update_cache();
		for node in &self.cache.borrow().idx_rev_depth[&depth] {
			func(&self.nodes[node.0]);
		}
	}

	pub(crate) fn max_depth(&self) -> i32 {
		self.update_cache();
		return self.cache.borrow().max_depth;
	}

	pub(crate) fn for_all_roots<F>(&self, mut func: F)
	where
		F: FnMut(&Node),
	{
		self.update_cache();
		for node in &self.cache.borrow().root_nodes {
			func(&self.nodes[node.0]);
		}
	}

	pub(crate) fn get_rev_depth(&self, id: NodeId) -> i32 {
		self.cache.borrow().rev_depth[id.0]
	}

	pub fn get_node(&self, id: NodeId) -> &Node {
		&self.nodes[id.0]
	}

	pub(crate) fn mut_node(&mut self, id: NodeId) -> &mut Node {
		self.cache.borrow_mut().valid = false;
		&mut self.nodes[id.0]
	}

	pub(crate) fn assert_is_wire_sum(&self, id: NodeId) {
		match self.get_node(id).function {
			NodeFunction::WireSum(_c) => {}
			_ => assert!(false, "Expected wire sum node, but got something else."),
		}
	}

	pub(crate) fn assert_is_not_wire_sum(&self, id: NodeId) {
		if let NodeFunction::WireSum(_colour) = &self.get_node(id).function {
			assert!(false, "Expected anything but a wire sum node.")
		}
	}

	pub(crate) fn is_wire(&self, id: NodeId) -> bool {
		if let NodeFunction::WireSum(_colour) = &self.get_node(id).function {
			true
		} else {
			false
		}
	}

	pub fn append_description(&mut self, nodeid: NodeId, description: &str) {
		self.nodes[nodeid.0].description = match &self.nodes[nodeid.0].description {
			Some(x) => Some(format!("{}\n{}", x, description)),
			None => Some(description.to_owned()),
		}
	}

	pub(crate) fn set_description(&mut self, description: String) {
		self.description = description
	}

	/// A pattern for a lookup table. ehhhhh too lazy to spec it now. Raise an issue if you want me to.
	pub fn add_lut(
		&mut self,
		sig_in: Vec<Signal>,
		sig_out: Signal,
		lut: Vec<bool>,
		width: usize,
	) -> (Vec<NodeId>, NodeId) {
		assert_eq!(width, sig_in.len());
		let mut counter = vec![false; width];

		let lut_comb = self.add_decider_comb();
		self.add_decider_comb_output(lut_comb, sig_out, false);
		let retwire = self.add_wire_red(vec![], vec![lut_comb]);

		let mut first = true;
		loop {
			let lut_idx: usize = counter.get_constant();
			if lut[lut_idx] {
				let mut first_condition = true;
				for (bit_idx, bit) in counter.iter().enumerate() {
					let sig_left = sig_in[bit_idx];
					let conj_disj = if first {
						DeciderRowConjDisj::FirstRow
					} else if first_condition {
						DeciderRowConjDisj::Or
					} else {
						DeciderRowConjDisj::And
					};
					let sig_right = if *bit {
						Signal::Constant(1)
					} else {
						Signal::Constant(0)
					};
					self.add_decider_comb_input(
						lut_comb,
						(sig_left, DeciderOperator::Equal, sig_right),
						conj_disj,
					);
					first_condition = false;
					first = false;
				}
			}

			if counter.iter().all(|x| *x) {
				break;
			}
			let mut carry = true;
			for x in &mut counter {
				let x_new = *x ^ carry;
				carry = *x && carry;
				*x = x_new;
			}
		}
		(vec![retwire; width], lut_comb)
	}

	pub(crate) fn have_shared_wire(&self, ldid_1: NodeId, ldid_2: NodeId) -> bool {
		let node = &self.nodes[ldid_1.0];
		for wire in node.iter_fanin_both() {
			if self.get_local_cell_io_network(*wire).contains(&ldid_2) {
				return true;
			}
		}
		for wire in node.iter_fanout_both() {
			if self.get_local_cell_io_network(*wire).contains(&ldid_2) {
				return true;
			}
		}
		return false;
	}

	fn get_wire_colour(&self, nodeid: NodeId) -> WireColour {
		match self.nodes[nodeid.0].function {
			NodeFunction::WireSum(colour) => colour,
			_ => panic!("Shouldn't call this function willy-nilly."),
		}
	}

	fn get_local_cell_io_network(&self, nodeid: NodeId) -> Vec<NodeId> {
		let colour = if !self.is_wire(nodeid) {
			return vec![];
		} else {
			self.get_wire_colour(nodeid)
		};
		let mut retval = vec![];
		let mut queue = BTreeSet::new();
		let mut seen = HashSet::new();
		queue.insert((nodeid, false));
		while !queue.is_empty() {
			let (curid, direction) = queue.pop_first().unwrap();
			if !seen.insert(curid) {
				continue;
			}
			let node = self.get_node(curid);
			match node.function {
				NodeFunction::WireSum(wire_colour) => {
					if wire_colour == colour {
						for foid in node.iter_fanout(colour) {
							queue.insert((*foid, false));
						}
						for fiid in node.iter_fanin(colour) {
							queue.insert((*fiid, true));
						}
					}
				}
				_ => {
					retval.push(curid);
					match direction {
						true => {
							for foid in node.iter_fanout(colour) {
								queue.insert((*foid, false));
							}
						}
						false => {
							for fiid in node.iter_fanin(colour) {
								queue.insert((*fiid, true));
							}
						}
					}
				}
			}
		}
		retval
	}
}

#[cfg(test)]
pub fn get_simple_logical_design() -> LogicalDesign {
	use ArithmeticOperator as Aop;
	use DeciderOperator as Dop;
	use Signal as Sig;
	let mut d = LogicalDesign::new();
	let constant1 = d.add_constant_comb(vec![Sig::Id(0)], vec![100]);
	let constant2 = d.add_constant_comb(vec![Sig::Id(1)], vec![4]);
	let mult = d.add_arithmetic_comb((Sig::Id(1), Aop::Mult, Sig::Id(0)), Sig::Id(10));
	let lamp = d.add_lamp((Sig::Id(10), Dop::Equal, Sig::Constant(400)));
	let _wire_pre_mult = d.add_wire_red(vec![constant1, constant2], vec![mult]);
	let _wire_post_mult = d.add_wire_red(vec![mult], vec![lamp]);
	d
}

#[cfg(test)]
pub fn get_complex_40_logical_design() -> LogicalDesign {
	use ArithmeticOperator as Aop;
	use DeciderOperator as Dop;
	use Signal as Sig;
	let mut d = LogicalDesign::new();
	let mut constants = vec![];
	for i in 0..20 {
		constants.push(d.add_constant_comb(vec![(Sig::Id(i))], vec![i + 1]));
	}
	let mut mults = vec![];
	for i in 0..10 {
		mults.push(d.add_arithmetic_comb(
			(Sig::Id(i * 2), Aop::Mult, Sig::Id(i * 2 + 1)),
			Sig::Id(20 + i),
		));
	}
	let mut lamps = vec![];
	for i in 0..10 {
		lamps.push(d.add_lamp((
			Sig::Id(20 + i),
			Dop::Equal,
			Sig::Constant((i * 2 + 1) * (i * 2 + 2)),
		)));
	}
	for i in 0..10 {
		d.add_wire_red(vec![constants[i * 2], constants[i * 2 + 1]], vec![mults[i]]);
		d.add_wire_red(vec![mults[i]], vec![lamps[i]]);
	}
	d
}

#[cfg(test)]
mod test {
	use crate::{
		physical_design::{PhysicalDesign, PlacementStrategy},
		serializable_design::SerializableDesign,
	};

	use super::*;
	#[allow(unused)]
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
		let lamp = d.add_lamp((Sig::Id(0), Dop::Equal, Sig::Id(1)));
		assert_eq!(d.nodes.len(), 1);
		assert_eq!(lamp.0, 0);
		assert_eq!(d.nodes[lamp.0].id, lamp);
	}

	#[test]
	fn output_to_input() {
		let mut d = LogicalDesign::new();
		let constant = d.add_constant_comb(vec![Sig::Id(2)], vec![100]);
		let lamp = d.add_lamp((Sig::Id(2), Dop::Equal, Sig::Constant(100)));
		d.connect_red(constant, lamp);
		assert_eq!(d.nodes.len(), 2);

		assert_eq!(constant.0, 0);
		assert_eq!(lamp.0, 1);

		assert_eq!(d.nodes[constant.0].id, constant);
		assert_eq!(d.nodes[lamp.0].id, lamp);

		assert!(d.nodes[constant.0].fanout_red.contains(&lamp));
		assert!(d.nodes[lamp.0].fanin_red.contains(&constant));

		assert_eq!(d.nodes[constant.0].fanout_red.len(), 1);
		assert_eq!(d.nodes[lamp.0].fanin_red.len(), 1);

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

	#[test]
	fn loopback() {
		let mut d = LogicalDesign::new();
		let counter = d.add_arithmetic_comb((Sig::Id(0), Aop::Add, Sig::Constant(0)), Sig::Id(0));
		let filter_pre =
			d.add_arithmetic_comb((Sig::Id(0), Aop::Mult, Sig::Constant(1)), Sig::Id(0));
		let filter_post =
			d.add_arithmetic_comb((Sig::Id(0), Aop::Mult, Sig::Constant(1)), Sig::Id(0));
		d.add_wire_red(vec![counter, filter_pre], vec![counter, filter_post]);
		d.for_all(|_, x| println!("{:?}", x));
		d.for_all_topological_order(|x| println!("{:?}", x));
		let mut p = PhysicalDesign::new();
		let mut s = SerializableDesign::new();
		p.build_from(&d, PlacementStrategy::default());
		s.build_from(&p, &d)
	}

	#[test]
	fn two_wire_sum() {
		let mut d = LogicalDesign::new();
		let a = d.add_arithmetic_comb((Sig::Id(10), Aop::Add, Sig::Constant(0)), Sig::Id(10));
		let b = d.add_arithmetic_comb((Sig::Id(10), Aop::Add, Sig::Constant(0)), Sig::Id(10));
		let c = d.add_arithmetic_comb((Sig::Id(10), Aop::Add, Sig::Constant(0)), Sig::Id(10));
		d.add_wire_red(vec![a], vec![c]);
		d.add_wire_red(vec![b], vec![c]);

		let mut p = PhysicalDesign::new();
		let mut s = SerializableDesign::new();
		p.build_from(&d, PlacementStrategy::default());
		s.build_from(&p, &d)
	}

	#[test]
	fn red_green_wires() {
		let mut d = LogicalDesign::new();
		let a = d.add_arithmetic_comb((Sig::Id(10), Aop::Add, Sig::Constant(0)), Sig::Id(10));
		let b = d.add_arithmetic_comb((Sig::Id(10), Aop::Add, Sig::Constant(0)), Sig::Id(10));
		let c = d.add_arithmetic_comb((Sig::Id(10), Aop::Add, Sig::Constant(0)), Sig::Id(10));
		d.add_wire_red(vec![a], vec![c]);
		d.add_wire_green(vec![b], vec![c]);

		let mut p = PhysicalDesign::new();
		let mut s = SerializableDesign::new();
		p.build_from(&d, PlacementStrategy::default());
		s.build_from(&p, &d);
		let blueprint_json = serde_json::to_string(&s).unwrap();
		println!("{}", blueprint_json);
	}

	#[test]
	fn dff() {
		let mut d = LogicalDesign::new();
		let (wire_data, wire_clk, comb_out) =
			d.add_dff(Signal::Id(0), Signal::Id(1), Signal::Id(2));
		let c1 = d.add_constant_comb(vec![Signal::Id(0)], vec![0]);
		let c2 = d.add_constant_comb(vec![Signal::Id(1)], vec![1]);
		let l1 = d.add_lamp((Signal::Id(2), Dop::NotEqual, Signal::Constant(0)));
		d.connect_red(c1, wire_data);
		d.connect_red(c2, wire_clk);
		d.add_wire_red(vec![comb_out], vec![l1]);

		let mut p = PhysicalDesign::new();
		let mut s = SerializableDesign::new();
		p.build_from(&d, PlacementStrategy::default());
		s.build_from(&p, &d);
		let blueprint_json = serde_json::to_string(&s).unwrap();
		println!("{}", blueprint_json);
	}

	#[test]
	fn latch() {
		let mut d = LogicalDesign::new();
		let (wire_data, wire_clk, comb_out) = d.add_latch(Signal::Id(0), Signal::Id(1));
		let c1 = d.add_constant_comb(vec![Signal::Id(0)], vec![0]);
		let c2 = d.add_constant_comb(vec![Signal::Id(1)], vec![1]);
		let l1 = d.add_lamp((Signal::Id(2), Dop::NotEqual, Signal::Constant(0)));
		d.connect_red(c1, wire_data);
		d.connect_red(c2, wire_clk);
		d.add_wire_red(vec![comb_out], vec![l1]);

		let mut p = PhysicalDesign::new();
		let mut s = SerializableDesign::new();
		p.build_from(&d, PlacementStrategy::default());
		s.build_from(&p, &d);
		let blueprint_json = serde_json::to_string(&s).unwrap();
		println!("{}", blueprint_json);
	}

	#[test]
	fn output_to_output() {
		let mut d = LogicalDesign::new();
		let nop1 = d.add_nop(Signal::Each, Signal::Each);
		let nop2 = d.add_nop(Signal::Each, Signal::Each);
		d.add_wire_red(vec![nop1, nop2], vec![]);

		let mut p = PhysicalDesign::new();
		let mut s = SerializableDesign::new();
		p.build_from(&d, PlacementStrategy::default());
		s.build_from(&p, &d);
		let blueprint_json = serde_json::to_string(&s).unwrap();
		println!("\n{}\n", blueprint_json);
	}

	#[test]
	fn input_to_input() {
		let mut d = LogicalDesign::new();
		let nop0 = d.add_nop(Signal::Each, Signal::Each);
		let nop1 = d.add_nop(Signal::Each, Signal::Each);
		let nop2 = d.add_nop(Signal::Each, Signal::Each);
		let nop3 = d.add_nop(Signal::Each, Signal::Each);
		d.add_wire_red(vec![], vec![nop1, nop2]);
		d.add_wire_red(vec![nop0], vec![nop1]);
		d.add_wire_red(vec![nop1], vec![nop3]);

		for x in &d.nodes {
			println!("{:?}", x);
		}

		let mut p = PhysicalDesign::new();
		let mut s = SerializableDesign::new();
		p.build_from(&d, PlacementStrategy::default());
		s.build_from(&p, &d);
		let blueprint_json = serde_json::to_string(&s).unwrap();
		println!("\n{}\n", blueprint_json);
	}
}

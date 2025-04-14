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
	collections::{BTreeSet, HashMap, HashSet},
	fmt::Display,
	hash::Hash,
	slice::Iter,
	usize, vec,
};

use itertools::{izip, Itertools};

use crate::{
	checked_design::CheckedDesign,
	connected_design::CoarseExpr,
	mapped_design::{BitSliceOps, MappedDesign},
	util::{hash_set, HashS},
};

pub const NET_RED_GREEN: (bool, bool) = (true, true);
pub const NET_RED: (bool, bool) = (true, false);
pub const NET_GREEN: (bool, bool) = (false, true);
pub const NET_NONE: (bool, bool) = (false, false);

// Supported Arithmetic Combinator operations as in the game.
#[derive(Debug, Clone, Copy)]
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
#[derive(Debug, Clone, Copy)]
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
#[derive(Debug, Clone, PartialEq, Eq)]
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

impl Signal {
	pub fn id(&self) -> i32 {
		match self {
			Signal::Id(id) => *id,
			_ => panic!("Unwrapped non-id signal as an id"),
		}
	}
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
		input_left_network: (bool, bool),
		input_right_network: (bool, bool),
	},
	/// Game entity.
	Decider {
		expressions: Vec<(Signal, DeciderOperator, Signal)>,
		expression_conj_disj: Vec<DeciderRowConjDisj>,
		input_left_networks: Vec<(bool, bool)>,
		input_right_networks: Vec<(bool, bool)>,
		output_network: Vec<(bool, bool)>,
		use_input_count: Vec<bool>,
		constants: Vec<Option<i32>>,
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

impl NodeFunction {
	pub(crate) fn unwrap_decider(
		&self,
	) -> (
		&Vec<(Signal, DeciderOperator, Signal)>,
		&Vec<DeciderRowConjDisj>,
		&Vec<(bool, bool)>,
		&Vec<(bool, bool)>,
		&Vec<(bool, bool)>,
		&Vec<bool>,
		&Vec<Option<i32>>,
	) {
		if let NodeFunction::Decider {
			expressions,
			expression_conj_disj,
			input_left_networks,
			input_right_networks,
			output_network,
			use_input_count,
			constants,
		} = self
		{
			(
				expressions,
				expression_conj_disj,
				input_left_networks,
				input_right_networks,
				output_network,
				use_input_count,
				constants,
			)
		} else {
			panic!("Unwrapped node as decider when it was, in fact, not one.");
		}
	}

	pub(crate) fn unwrap_arithmetic(
		&self,
	) -> (
		ArithmeticOperator,
		Signal,
		Signal,
		(bool, bool),
		(bool, bool),
	) {
		if let NodeFunction::Arithmetic {
			op,
			input_1,
			input_2,
			input_left_network,
			input_right_network,
		} = self
		{
			(
				*op,
				*input_1,
				*input_2,
				*input_left_network,
				*input_right_network,
			)
		} else {
			panic!("Unwrapped node as arithmetic combinator when it was, in fact, not one.");
		}
	}
}

/// An id that is unique in a design. Can be used to uniquely identify a node in a design. Use this id to make connections.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeId(pub(crate) usize);

impl NodeId {
	const NONE: NodeId = NodeId(usize::MAX);
}

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
	pub(crate) fn iter_fanin(&self, colour: WireColour) -> Iter<NodeId> {
		match colour {
			WireColour::Red => self.fanin_red.iter(),
			WireColour::Green => self.fanin_green.iter(),
		}
	}

	/// Iterate the fanout of a specific colour.
	pub(crate) fn iter_fanout(&self, colour: WireColour) -> Iter<NodeId> {
		match colour {
			WireColour::Red => self.fanout_red.iter(),
			WireColour::Green => self.fanout_green.iter(),
		}
	}

	/// Iterate the fanout of both colours.
	pub(crate) fn iter_fanout_both(
		&self,
	) -> std::iter::Chain<std::slice::Iter<'_, NodeId>, std::slice::Iter<'_, NodeId>> {
		self.iter_fanout(WireColour::Red)
			.chain(self.iter_fanout(WireColour::Green))
	}

	/// Iterate the fanin of both colours.
	pub(crate) fn iter_fanin_both(
		&self,
	) -> std::iter::Chain<std::slice::Iter<'_, NodeId>, std::slice::Iter<'_, NodeId>> {
		self.iter_fanin(WireColour::Red)
			.chain(self.iter_fanin(WireColour::Green))
	}

	/// Does this node not have any nodes that feed into it?
	pub(crate) fn fanin_empty(&self) -> bool {
		self.fanin_green.is_empty() && self.fanin_red.is_empty()
	}

	/// Does this node not feed into any nodes?
	pub(crate) fn fanout_empty(&self) -> bool {
		self.fanout_green.is_empty() && self.fanout_red.is_empty()
	}

	pub(crate) fn is_constant(&self) -> bool {
		match &self.function {
			NodeFunction::Constant { .. } => true,
			_ => false,
		}
	}

	pub(crate) fn is_decider(&self) -> bool {
		match &self.function {
			NodeFunction::Decider { .. } => true,
			_ => false,
		}
	}

	pub(crate) fn is_arithmetic(&self) -> bool {
		match &self.function {
			NodeFunction::Arithmetic { .. } => true,
			_ => false,
		}
	}
}

#[derive(Debug, PartialEq)]
pub enum ResetSpec {
	Sync(Signal),
	Async(Signal),
	Disabled,
}

pub struct MemoryReadPort {
	pub addr: Signal,
	pub data: Signal,
	pub clk: Option<Signal>,
	pub en: Option<Signal>,
	pub rst: ResetSpec,
	pub transparent: bool,
}

pub struct MemoryWritePort {
	pub addr: Signal,
	pub data: Signal,
	pub clk: Signal,
	pub en: Option<Signal>,
}

#[derive(Debug)]
pub struct MemoryPortReadFilled {
	pub addr_wire: NodeId,
	pub data: NodeId,
	pub clk_wire: Option<NodeId>,
	pub en_wire: Option<NodeId>,
	pub rst_wire: Option<NodeId>,
}

#[derive(Debug)]
pub struct MemoryPortWriteFilled {
	pub addr_wire: NodeId,
	pub data_wire: NodeId,
	pub clk_wire: Option<NodeId>,
	pub en_wire: Option<NodeId>,
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
impl Default for LogicalDesign {
	fn default() -> Self {
		Self::new()
	}
}

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
	pub(crate) fn build_from(
		&mut self,
		checked_design: &CheckedDesign,
		mapped_design: &MappedDesign,
	) {
		checked_design.apply_onto(self, mapped_design); // lmfao
	}

	/// Add a node with the specific [`NodeFunction`] and the specific output [`Signal`]. Generally a low level API.
	pub(crate) fn add_node(&mut self, function: NodeFunction, output: Vec<Signal>) -> NodeId {
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
		assert!(
			self.is_wire(out_node) != self.is_wire(in_node),
			"Can only connect wires to terminals, or terminals to wires. Not T to T or W to W."
		);
		self.cache.get_mut().valid = false;
		self.nodes[out_node.0].fanout_red.push(in_node);
		self.nodes[in_node.0].fanin_red.push(out_node);
	}

	/// See [`connect_red`] but replace red with green.
	pub fn connect_green(&mut self, out_node: NodeId, in_node: NodeId) {
		assert!(
			self.is_wire(out_node) != self.is_wire(in_node),
			"Can only connect wires to terminals, or terminals to wires. Not T to T or W to W."
		);
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
				input_left_network: NET_RED_GREEN,
				input_right_network: NET_RED_GREEN,
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
				input_2: Signal::None,
				input_left_network: NET_RED_GREEN,
				input_right_network: NET_RED_GREEN,
			},
			vec![output],
		)
	}

	/// Add a combinator fitting the pattern that I call "no-operation". Converts the input signal to an output signal using an arithmetic combinator.
	///
	/// Returns the id for that new combinator.
	pub fn add_nop_simple(&mut self) -> NodeId {
		self.add_nop(Signal::Each, Signal::Each)
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
			NET_RED_GREEN,
			NET_RED_GREEN,
		);
		self.add_decider_comb_output(ret, output, false, NET_RED_GREEN);
		ret
	}

	/** Inputs are returned as wires, outputs are retured as combinators. */
	///
	/// The returned tuple is (D wire id, CLK wire id, Output combinator id)
	pub(crate) fn add_latch(&mut self, data: Signal, clk: Signal) -> (NodeId, NodeId, NodeId) {
		let in_control = {
			let ic = self.add_decider_comb();
			self.add_decider_comb_input(
				ic,
				(clk, DeciderOperator::GreaterThan, Signal::Constant(0)),
				DeciderRowConjDisj::FirstRow,
				NET_RED_GREEN,
				NET_RED_GREEN,
			);
			self.add_decider_comb_output(ic, data, true, NET_RED_GREEN);
			ic
		};

		let mem_cell = {
			let mc = self.add_decider_comb();
			self.add_decider_comb_input(
				mc,
				(clk, DeciderOperator::Equal, Signal::Constant(0)),
				DeciderRowConjDisj::FirstRow,
				NET_RED_GREEN,
				NET_RED_GREEN,
			);
			self.add_decider_comb_output(mc, data, true, NET_RED_GREEN);
			mc
		};

		let clk_wire = self.add_wire_green(vec![], vec![in_control, mem_cell]);
		self.add_wire_red(vec![in_control, mem_cell], vec![mem_cell]);
		let data_in_wire = self.add_wire_red(vec![], vec![in_control]);

		(data_in_wire, clk_wire, mem_cell)
	}

	/** Create a standard D-Flip-Flop in this design. Returned NodeIds match the order of the function signature.
	Inputs are returned as wires, outputs are retured as combinators. */
	pub(crate) fn add_dff(
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

	pub(crate) fn add_adffe(
		&mut self,
		_input: Signal,
		_clk: Signal,
		_en: Signal,
		_arst: Signal,
		_output: Signal,
	) -> (NodeId, NodeId, NodeId, NodeId, NodeId) {
		todo!()
	}

	pub(crate) fn add_dffe(
		&mut self,
		input: Signal,
		clk: Signal,
		en: Signal,
		output: Signal,
	) -> (NodeId, NodeId, NodeId, NodeId) {
		// If the compiler was aware of either green or red wires, then clk_buf could be eliminated.
		// A problem that this could have is if `en` is not stable when clk is high, this is actually an important concern.
		//
		// en---r-----------------+
		//                        |
		// clk--r--clk_buf--g--clk_en_buf--r--dff--r--output
		//                                     |
		// input--r----------------------------+
		let (d_wire, clk_wire, q) = self.add_dff(input, clk, output);
		let clk_en_buf = self.add_decider_comb();
		self.add_decider_comb_input(
			clk_en_buf,
			(en, DeciderOperator::Equal, Signal::Constant(1)),
			DeciderRowConjDisj::FirstRow,
			NET_RED,
			NET_RED_GREEN,
		);
		self.add_decider_comb_output(clk_en_buf, clk, true, NET_GREEN);
		let clk_buf = self.add_nop(clk, clk);
		self.add_wire_green(vec![clk_buf], vec![clk_en_buf]);
		self.connect_red(clk_en_buf, clk_wire);
		let clk_in_wire = self.add_wire_red(vec![], vec![clk_buf]);
		let en_wire = self.add_wire_red(vec![], vec![clk_en_buf]);
		(d_wire, clk_in_wire, en_wire, q)
	}

	pub(crate) fn add_sdffe(
		&mut self,
		_input: Signal,
		_clk: Signal,
		_srst: Signal,
		_en: Signal,
		_output: Signal,
	) -> (NodeId, NodeId, NodeId, NodeId, NodeId) {
		todo!()
	}

	pub(crate) fn add_ram(
		&mut self,
		read_ports: Vec<MemoryReadPort>,
		write_ports: Vec<MemoryWritePort>,
		size: u32,
	) -> (Vec<MemoryPortReadFilled>, Vec<MemoryPortWriteFilled>) {
		let mut wr_addr_ret = NodeId::NONE;
		let mut wr_data_ret = NodeId::NONE;
		let mut wr_en_ret = NodeId::NONE;
		let mut wr_clk_ret = NodeId::NONE;

		assert!(!read_ports.is_empty());
		assert!(!write_ports.is_empty());
		assert!(write_ports
			.iter()
			.map(|p| p.clk)
			.all(|s| s == write_ports[0].clk));

		let mut preferred_output = None;
		for p in &read_ports {
			preferred_output = match preferred_output {
				Some(o) => Some(o),
				None => Some(p.data),
			}
		}
		let preferred_output = preferred_output.unwrap();

		// Make physical memory cells.
		let mut memory_cells = Vec::with_capacity(size as usize);
		for _ in 0..size {
			memory_cells.push(self.add_dffe(
				write_ports[0].data,
				write_ports[0].clk,
				Signal::Id(0),
				preferred_output,
			));
		}

		// Address decoding write data and priority selecting.
		let mut wr_data_select_decode = vec![Vec::with_capacity(size as usize); write_ports.len()];
		for (i, wport) in write_ports.iter().enumerate() {
			for physical_address in 0..size {
				let one_hot = self.add_decider_comb();
				self.add_decider_comb_input(
					one_hot,
					(
						wport.addr,
						DeciderOperator::Equal,
						Signal::Constant(physical_address as i32),
					),
					DeciderRowConjDisj::FirstRow,
					NET_RED_GREEN,
					NET_RED_GREEN,
				);
				for (j, wport2) in write_ports.iter().enumerate() {
					if j <= i {
						continue;
					}
					self.add_decider_comb_input(
						one_hot,
						(wport.addr, DeciderOperator::NotEqual, wport2.addr),
						DeciderRowConjDisj::And,
						NET_RED_GREEN,
						NET_RED_GREEN,
					);
				}
				self.add_decider_comb_output(one_hot, wport.data, true, NET_RED_GREEN);
				wr_data_select_decode[i].push(one_hot);
			}
		}

		// Address decoding write enable.
		let mut wr_enable_decode = Vec::with_capacity(size as usize);
		for physical_address in 0..size {
			let one_hot = self.add_decider_comb();
			for (i, wport) in write_ports.iter().enumerate() {
				self.add_decider_comb_input(
					one_hot,
					(
						wport.addr,
						DeciderOperator::Equal,
						Signal::Constant(physical_address as i32),
					),
					if i == 0 {
						DeciderRowConjDisj::FirstRow
					} else {
						DeciderRowConjDisj::Or
					},
					NET_RED_GREEN,
					NET_RED_GREEN,
				);
				if let Some(en_signal) = wport.en {
					self.add_decider_comb_input(
						one_hot,
						(en_signal, DeciderOperator::Equal, Signal::Constant(1)),
						DeciderRowConjDisj::And,
						NET_RED_GREEN,
						NET_RED_GREEN,
					);
				}
			}
			self.add_decider_comb_output(one_hot, Signal::Id(1), false, NET_RED_GREEN);
			wr_enable_decode.push(one_hot);
		}

		// Used to transform the input data signals into a common format.
		// OPTIMIZATION: Can remove this if theres only 1 write port.
		let mut wr_data_mapper = vec![Vec::with_capacity(size as usize); write_ports.len()];
		for _ in 0..size {
			for (i, wport) in write_ports.iter().enumerate() {
				wr_data_mapper[i].push(self.add_nop(wport.data, Signal::Id(0)));
			}
		}

		// Wire up write side
		for phy_addr in 0..size as usize {
			let (data_wire, clk_wire, en_wire, _q) = memory_cells[phy_addr];
			// Connect enable and data in
			self.connect_red(wr_enable_decode[phy_addr], en_wire);
			self.connect_red(wr_data_mapper[0][phy_addr], data_wire);

			// Clock daisy chain
			if phy_addr == 0 {
				wr_clk_ret = clk_wire;
			} else {
				let (_data, clk_wire_m1, _en, _q) = memory_cells[phy_addr - 1];
				// Peer into the soul of the current DFF and make direct connection with the wire of the previous.
				let clk_combs = self.get_fanout_red(clk_wire).iter().cloned().collect_vec();
				for comb in clk_combs {
					self.connect_red(clk_wire_m1, comb);
				}
			}

			// data/en decoder daisy chaining.
			{
				self.add_wire_red(
					vec![],
					vec![
						wr_enable_decode[phy_addr],
						wr_data_select_decode[0][phy_addr],
					],
				);
				if phy_addr == 0 {
					let common_ret = self.add_wire_red(vec![], vec![wr_enable_decode[phy_addr]]);
					wr_en_ret = common_ret;
					wr_addr_ret = common_ret;
					wr_data_ret = common_ret;
				} else {
					self.add_wire_red(
						vec![],
						vec![wr_enable_decode[phy_addr], wr_enable_decode[phy_addr - 1]],
					);
				}
			}

			// Data decode to mapper
			for i in 0..write_ports.len() {
				self.add_wire_red_simple(
					wr_data_select_decode[i][phy_addr],
					wr_data_mapper[i][phy_addr],
				);
			}

			// Mapper and address decode daisy chain
			for i in 1..write_ports.len() {
				// Input side daisy chain
				self.add_wire_red(
					vec![],
					vec![wr_data_mapper[i][phy_addr], wr_data_mapper[i - 1][phy_addr]],
				);
				// Output side daisy chain
				self.add_wire_red(
					vec![
						wr_data_select_decode[i][phy_addr],
						wr_data_select_decode[i - 1][phy_addr],
					],
					vec![],
				);
			}
		}

		// Now for reading side

		let mut rd_addr_ret = Vec::with_capacity(read_ports.len());
		let mut rd_data_ret = Vec::with_capacity(read_ports.len());
		let mut rd_en_ret = Vec::with_capacity(read_ports.len());
		let mut rd_clk_ret = Vec::with_capacity(read_ports.len());
		let mut rd_rst_ret = Vec::with_capacity(read_ports.len());

		let mut mux1s = vec![Vec::with_capacity(size as usize); read_ports.len()];
		for physical_address in 0..size {
			for (i, rdport) in read_ports.iter().enumerate() {
				let mux = self.add_decider_comb();
				mux1s[i].push(mux);
				self.add_decider_comb_input(
					mux,
					(
						rdport.addr,
						DeciderOperator::Equal,
						Signal::Constant(physical_address as i32),
					),
					DeciderRowConjDisj::FirstRow,
					NET_RED,
					NET_RED_GREEN,
				);
				self.add_decider_comb_output(mux, Signal::Everything, true, NET_GREEN);
			}
		}

		// read buffering
		let mut port_buffer_in_wires = Vec::with_capacity(read_ports.len());
		for p in read_ports.iter() {
			if let Some(clk) = p.clk {
				let (d_wire, clk_wire, en_wire, rst_wire, q) = match p.rst {
					ResetSpec::Sync(rst) => self.add_sdffe(
						p.data,
						clk,
						p.en.unwrap_or(Signal::Constant(1)),
						rst,
						p.data,
					),
					ResetSpec::Async(rst) => self.add_adffe(
						p.data,
						clk,
						p.en.unwrap_or(Signal::Constant(1)),
						rst,
						p.data,
					),
					ResetSpec::Disabled => {
						if let Some(en) = p.en {
							let (d_wire, clk_wire, en_wire, q) =
								self.add_dffe(p.data, clk, en, p.data);
							(d_wire, clk_wire, en_wire, NodeId(usize::MAX), q)
						} else {
							let (d_wire, clk_wire, q) = self.add_dff(p.data, clk, p.data);
							(d_wire, clk_wire, NodeId(usize::MAX), NodeId(usize::MAX), q)
						}
					}
				};
				if p.en.is_some() {
					rd_en_ret.push(Some(en_wire));
				} else {
					rd_en_ret.push(None);
				}
				rd_clk_ret.push(Some(clk_wire));
				port_buffer_in_wires.push(Some(d_wire));
				rd_data_ret.push(q);
				if p.rst != ResetSpec::Disabled {
					rd_rst_ret.push(Some(rst_wire));
				} else {
					rd_rst_ret.push(None);
				}
			} else {
				rd_clk_ret.push(None);
				rd_rst_ret.push(None);
				rd_en_ret.push(None);
			}
		}

		// Wiring up read side.
		// Wire memory_cells to read address decoders
		for physical_addr in 0..size as usize {
			let (_, _, _, q) = memory_cells[physical_addr];
			let mux1 = mux1s[0][physical_addr];
			self.add_wire_green(vec![q], vec![mux1]);
			// Daisy chain down read port muxes
			for i in 1..read_ports.len() {
				let mux1_i = mux1s[physical_addr][i];
				let mux1_i_m1 = mux1s[physical_addr][i - 1];
				self.add_wire_green(vec![], vec![mux1_i_m1, mux1_i]);
			}
		}
		// Wire the address decode up and down on both sides
		for i in 0..read_ports.len() {
			let address_wire = self.add_wire_red(vec![], vec![mux1s[0][i]]);
			rd_addr_ret.push(address_wire);
			// Daisy chain up port address muxes
			for physical_addr in 1..size as usize {
				self.add_wire_red(
					vec![],
					vec![mux1s[i][physical_addr - 1], mux1s[i][physical_addr]],
				);
				self.add_wire_red(
					vec![mux1s[i][physical_addr - 1], mux1s[i][physical_addr]],
					vec![],
				);
			}
		}

		let mut rd_ret = Vec::with_capacity(read_ports.capacity());
		for i in 0..read_ports.len() {
			rd_ret.push(MemoryPortReadFilled {
				addr_wire: rd_addr_ret[i],
				data: rd_data_ret[i],
				clk_wire: rd_clk_ret[i],
				en_wire: rd_en_ret[i],
				rst_wire: rd_rst_ret[i],
			});
		}
		let mut wr_ret = Vec::with_capacity(write_ports.capacity());
		for i in 0..write_ports.len() {
			wr_ret.push(MemoryPortWriteFilled {
				addr_wire: wr_addr_ret,
				data_wire: wr_data_ret,
				clk_wire: if i == 0 { Some(wr_clk_ret) } else { None },
				en_wire: write_ports[i].en.map(|_| wr_en_ret),
			});
		}

		(rd_ret, wr_ret)
	}

	pub(crate) fn add_rom(
		&mut self,
		ports: Vec<MemoryReadPort>,
		rom_values: Vec<i32>,
		density: Option<i32>,
	) -> Vec<MemoryPortReadFilled> {
		let mut addresses_ret = vec![];
		let mut data_ret = vec![];
		let mut en_ret = vec![];
		let mut clk_ret = vec![];
		let mut rst_ret = vec![];

		assert!(
			rom_values.len() <= u32::MAX as usize,
			"Can't support more than {} distinct addresses (32-bit restriction)",
			u32::MAX
		);
		let mut preferred_output = None;
		for p in &ports {
			preferred_output = match preferred_output {
				Some(o) => Some(o),
				None => Some(p.data),
			}
		}
		let preferred_output = preferred_output.unwrap();
		let mut rom_combs = vec![];
		if let Some(packing) = &density {
			for values in &rom_values.iter().chunks(*packing as usize) {
				let mut signals = vec![];
				let mut constants = vec![];
				for (i, v) in values.enumerate() {
					signals.push(Signal::Id(i as i32));
					constants.push(*v);
				}
				rom_combs.push(self.add_constant_comb(signals, constants));
			}
		} else {
			for value in rom_values.iter() {
				rom_combs.push(self.add_constant_comb(vec![preferred_output], vec![*value]));
			}
		}

		for p in &ports {
			let mut last_wire_input = self.add_wire_floating_red();
			let mut mux1s = vec![];
			addresses_ret.push(last_wire_input);
			for (addr, constant) in rom_combs.iter().enumerate() {
				let mux = self.add_decider_comb();
				mux1s.push(mux);
				if let Some(packing) = &density {
					self.add_decider_comb_input(
						mux,
						(
							p.addr,
							DeciderOperator::GreaterThanEqual,
							Signal::Constant(addr as i32 * packing),
						),
						DeciderRowConjDisj::FirstRow,
						NET_RED,
						NET_RED_GREEN,
					);
					self.add_decider_comb_input(
						mux,
						(
							p.addr,
							DeciderOperator::LessThan,
							Signal::Constant(addr as i32 * packing + packing),
						),
						DeciderRowConjDisj::And,
						NET_RED,
						NET_RED_GREEN,
					);
				} else {
					self.add_decider_comb_input(
						mux,
						(
							p.addr,
							DeciderOperator::Equal,
							Signal::Constant(addr as i32),
						),
						DeciderRowConjDisj::FirstRow,
						NET_RED,
						NET_RED_GREEN,
					);
				}
				self.add_decider_comb_output(mux, Signal::Everything, true, NET_GREEN);
				self.add_wire_green(vec![*constant], vec![mux]);
				self.connect_red(last_wire_input, mux);
				last_wire_input = self.add_wire_red(vec![], vec![mux]);
			}
			if let Some(packing) = &density {
				let mut mux2s = vec![];
				for (idx, mux1) in mux1s.iter().enumerate() {
					if idx == 0 {
						continue;
					}
					self.add_wire_green(vec![*mux1, mux1s[idx - 1]], vec![]);
				}
				let addr_lower = self.add_arithmetic_comb(
					(p.addr, ArithmeticOperator::Mod, Signal::Constant(*packing)),
					p.addr,
				);
				self.connect_red(*addresses_ret.last().unwrap(), addr_lower);
				for i in 0..*packing as usize {
					mux2s.push(self.add_decider_comb());
					self.add_decider_comb_input(
						mux2s[i],
						(p.addr, DeciderOperator::Equal, Signal::Constant(i as i32)),
						DeciderRowConjDisj::FirstRow,
						NET_RED,
						NET_RED_GREEN,
					);
					self.add_decider_comb_output(mux2s[i], Signal::Id(i as i32), true, NET_GREEN);
					if i == 0 {
						self.add_wire_red(vec![addr_lower], vec![mux2s[0]]);
						self.add_wire_green(vec![mux1s[0]], vec![mux2s[0]]);
					} else {
						self.add_wire_red(vec![], vec![mux2s[i], mux2s[i - 1]]);
						self.add_wire_green(vec![], vec![mux2s[i], mux2s[i - 1]]);
						self.add_wire_red(vec![mux2s[i], mux2s[i - 1]], vec![]);
					}
				}
				let collapse = self.add_nop(Signal::Each, p.data);
				self.add_wire_red(vec![mux2s[0]], vec![collapse]);
				data_ret.push(collapse);
			} else {
				for (idx, mux1) in mux1s.iter().enumerate() {
					if idx == 0 {
						continue;
					}
					self.add_wire_red(vec![*mux1, mux1s[idx - 1]], vec![]);
				}
				if p.data != preferred_output {
					let nop = self.add_nop(preferred_output, p.data);
					self.add_wire_red(vec![mux1s[0]], vec![nop]);
					data_ret.push(nop);
				} else {
					data_ret.push(mux1s[0]);
				}
			};
		}
		// buffering
		for (i, p) in ports.iter().enumerate() {
			if let Some(clk) = p.clk {
				let (d_wire, clk_wire, en_wire, rst_wire, q) = match p.rst {
					ResetSpec::Sync(rst) => self.add_sdffe(
						p.data,
						clk,
						p.en.unwrap_or(Signal::Constant(1)),
						rst,
						p.data,
					),
					ResetSpec::Async(rst) => self.add_adffe(
						p.data,
						clk,
						p.en.unwrap_or(Signal::Constant(1)),
						rst,
						p.data,
					),
					ResetSpec::Disabled => {
						if let Some(en) = p.en {
							let (d_wire, clk_wire, en_wire, q) =
								self.add_dffe(p.data, clk, en, p.data);
							(d_wire, clk_wire, en_wire, NodeId(usize::MAX), q)
						} else {
							let (d_wire, clk_wire, q) = self.add_dff(p.data, clk, p.data);
							(d_wire, clk_wire, NodeId(usize::MAX), NodeId(usize::MAX), q)
						}
					}
				};
				if p.en.is_some() {
					en_ret.push(Some(en_wire));
				} else {
					en_ret.push(None);
				}
				clk_ret.push(Some(clk_wire));
				self.connect_red(data_ret[i], d_wire);
				data_ret[i] = q;
				if p.rst != ResetSpec::Disabled {
					rst_ret.push(Some(rst_wire));
				} else {
					rst_ret.push(None);
				}
			} else {
				clk_ret.push(None);
				rst_ret.push(None);
				en_ret.push(None);
			}
		}
		let mut ret = vec![];
		for (data, addr, clk, rst, en) in izip!(data_ret, addresses_ret, clk_ret, rst_ret, en_ret) {
			ret.push(MemoryPortReadFilled {
				addr_wire: addr,
				data,
				clk_wire: clk,
				en_wire: en,
				rst_wire: rst,
			});
		}
		ret
	}

	/// A pattern for bit manipulations. ehhhhh too lazy to spec it now. Raise an issue if you want me to.
	pub(crate) fn add_swizzle(
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
				input_left_networks: vec![],
				input_right_networks: vec![],
				output_network: vec![],
				constants: vec![],
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
		left_network: (bool, bool),
		right_network: (bool, bool),
	) {
		match &mut self.nodes[id.0].function {
			NodeFunction::Decider {
				expressions,
				expression_conj_disj,
				input_left_networks,
				input_right_networks,
				..
			} => {
				expressions.push(expr);
				expression_conj_disj.push(conj_disj);
				input_left_networks.push(left_network);
				input_right_networks.push(right_network);
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
		network: (bool, bool),
	) {
		match &mut self.nodes[id.0].function {
			NodeFunction::Decider {
				use_input_count,
				output_network,
				constants,
				..
			} => {
				use_input_count.push(use_input_count_for_output);
				output_network.push(network);
				constants.push(None);
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
			"Tried to create constant combinator with mismatched outputs and counts"
		);

		self.add_node(
			NodeFunction::Constant {
				enabled: true,
				constants: counts,
			},
			output,
		)
	}

	pub fn set_ith_output_count(&mut self, id: NodeId, i: usize, count: i32) {
		match &mut self.nodes[id.0].function {
			NodeFunction::Constant { constants, .. } => {
				constants[i] = count;
			}
			_ => assert!(false, "Tried to set count for non-constant combinator node"),
		}
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
		for x in &fanin {
			assert!(!self.is_wire(*x), "Can't add wires to wires.")
		}
		for x in &fanout {
			assert!(!self.is_wire(*x), "Can't add wires to wires.")
		}
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
		for x in &fanin {
			assert!(!self.is_wire(*x), "Can't add wires to wires.")
		}
		for x in &fanout {
			assert!(!self.is_wire(*x), "Can't add wires to wires.")
		}
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

	/// It has the same behaviour as a wire in game. fanin/fanout MUST be anything other than another wire.
	///
	/// Returns the id for the wire you created.
	pub fn add_wire_red_simple(&mut self, fanin: NodeId, fanout: NodeId) -> NodeId {
		assert!(!self.is_wire(fanin), "Can't add wires to wires.");
		assert!(!self.is_wire(fanout), "Can't add wires to wires.");
		let id = self.add_node(
			NodeFunction::WireSum(WireColour::Red),
			vec![Signal::Everything],
		);
		self.connect_red(fanin, id);
		self.connect_red(id, fanout);
		id
	}

	/// It has the same behaviour as a wire in game. fanin/fanout MUST be anything other than another wire.
	///
	/// Returns the id for the wire you created.
	pub fn add_wire_green_simple(&mut self, fanin: NodeId, fanout: NodeId) -> NodeId {
		assert!(!self.is_wire(fanin), "Can't add wires to wires.");
		assert!(!self.is_wire(fanout), "Can't add wires to wires.");
		let id = self.add_node(
			NodeFunction::WireSum(WireColour::Green),
			vec![Signal::Everything],
		);
		self.connect_green(fanin, id);
		self.connect_green(id, fanout);
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

	pub fn get_node(&self, id: NodeId) -> &Node {
		&self.nodes[id.0]
	}

	#[allow(dead_code)]
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
		self.add_decider_comb_output(lut_comb, sig_out, false, NET_RED_GREEN);
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
						NET_RED_GREEN,
						NET_RED_GREEN,
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

	#[allow(dead_code)]
	pub(crate) fn have_shared_wire(&self, ldid_1: NodeId, ldid_2: NodeId) -> bool {
		let (input, output) = self.have_shared_wire_discriminate_side(ldid_1, ldid_2);
		input || output
	}

	pub(crate) fn have_shared_wire_discriminate_side(
		&self,
		ldid_1: NodeId,
		ldid_2: NodeId,
	) -> (bool, bool) {
		let mut input_side = false;
		let mut output_side = false;
		let node = &self.nodes[ldid_1.0];
		for wire in node.iter_fanin_both() {
			if self
				.get_node(*wire)
				.iter_fanin_both()
				.chain(self.get_node(*wire).iter_fanout_both())
				.any(|id| *id == ldid_2)
			{
				input_side = true;
				break;
			}
		}

		for wire in node.iter_fanout_both() {
			if self
				.get_node(*wire)
				.iter_fanin_both()
				.chain(self.get_node(*wire).iter_fanout_both())
				.any(|id| *id == ldid_2)
			{
				output_side = true;
				break;
			}
		}
		(input_side, output_side)
	}

	pub(crate) fn have_shared_wire_detailed(
		&self,
		id1: NodeId,
		id2: NodeId,
		colour: WireColour,
		id1_input: bool,
		id2_input: bool,
	) -> bool {
		let node = &self.nodes[id1.0];
		if id1_input {
			if id2_input {
				return node
					.iter_fanin(colour)
					.map(|wire| {
						self.get_node(*wire)
							.iter_fanin_both()
							.chain(self.get_node(*wire).iter_fanout(colour))
							.any(|id| *id == id2)
					})
					.any(|b| b);
			} else {
				return node
					.iter_fanin(colour)
					.map(|wire| {
						self.get_node(*wire)
							.iter_fanin_both()
							.chain(self.get_node(*wire).iter_fanin(colour))
							.any(|id| *id == id2)
					})
					.any(|b| b);
			}
		} else {
			if id2_input {
				return node
					.iter_fanout(colour)
					.map(|wire| {
						self.get_node(*wire)
							.iter_fanin_both()
							.chain(self.get_node(*wire).iter_fanout(colour))
							.any(|id| *id == id2)
					})
					.any(|b| b);
			} else {
				return node
					.iter_fanout(colour)
					.map(|wire| {
						self.get_node(*wire)
							.iter_fanin_both()
							.chain(self.get_node(*wire).iter_fanin(colour))
							.any(|id| *id == id2)
					})
					.any(|b| b);
			}
		}
	}

	pub(crate) fn get_connected_combs(&self, ldid: NodeId) -> Vec<NodeId> {
		self.assert_is_not_wire_sum(ldid);
		let node = &self.nodes[ldid.0];
		let mut ret = hash_set();
		for wire in node.iter_fanin_both().chain(node.iter_fanout_both()) {
			let wire_node = &self.nodes[wire.0];
			for connected in wire_node
				.iter_fanin_both()
				.chain(wire_node.iter_fanout_both())
			{
				if *connected == ldid {
					continue;
				}
				ret.insert(*connected);
			}
		}
		ret.into_iter().collect_vec()
	}

	pub(crate) fn get_fanin_network(&self, ldid: NodeId, colour: WireColour) -> HashS<NodeId> {
		let node = &self.nodes[ldid.0];
		let mut retval = hash_set();
		self.assert_is_not_wire_sum(ldid);
		for wire in node.iter_fanin(colour) {
			let localio = self.get_local_cell_io_network(*wire).into_iter().collect();
			retval = retval.union(&localio).copied().collect();
		}
		retval
	}

	pub(crate) fn get_fanout_network(&self, ldid: NodeId, colour: WireColour) -> HashS<NodeId> {
		let node = &self.nodes[ldid.0];
		let mut retval = hash_set();
		self.assert_is_not_wire_sum(ldid);
		for wire in node.iter_fanout(colour) {
			let localio = self.get_local_cell_io_network(*wire).into_iter().collect();
			retval = retval.union(&localio).copied().collect();
		}
		retval
	}

	fn get_wire_colour(&self, nodeid: NodeId) -> WireColour {
		match self.nodes[nodeid.0].function {
			NodeFunction::WireSum(colour) => colour,
			_ => panic!("Shouldn't call this function willy-nilly."),
		}
	}

	fn get_local_cell_io_network(&self, nodeid: NodeId) -> Vec<NodeId> {
		let network = self.get_wire_network(nodeid);
		if network.is_empty() {
			return network;
		}
		let colour = self.get_wire_colour(nodeid);
		network
			.iter()
			.map(|wire_id| {
				self.nodes[wire_id.0]
					.iter_fanin(colour)
					.chain(self.nodes[wire_id.0].iter_fanout(colour))
			})
			.flatten()
			.copied()
			.collect_vec()
	}

	pub(crate) fn get_wire_network(&self, nodeid: NodeId) -> Vec<NodeId> {
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
					retval.push(curid);
					if wire_colour == colour {
						for foid in node.iter_fanout(colour) {
							queue.insert((*foid, false));
						}
						for fiid in node.iter_fanin(colour) {
							queue.insert((*fiid, true));
						}
					}
				}
				_ => match direction {
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
				},
			}
		}
		retval
	}

	pub fn set_description_node(&mut self, id: NodeId, desc: String) {
		self.nodes[id.0].description = Some(desc);
	}

	fn get_fanout_red(&self, id: NodeId) -> &[NodeId] {
		self.nodes[id.0].fanout_red.as_slice()
	}

	#[allow(dead_code)]
	fn get_fanout_green(&self, id: NodeId) -> &[NodeId] {
		self.nodes[id.0].fanout_green.as_slice()
	}

	#[allow(dead_code)]
	fn get_fanin_red(&self, id: NodeId) -> &[NodeId] {
		self.nodes[id.0].fanin_red.as_slice()
	}

	#[allow(dead_code)]
	fn get_fanin_green(&self, id: NodeId) -> &[NodeId] {
		self.nodes[id.0].fanin_green.as_slice()
	}
}

impl Display for LogicalDesign {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_str("LogicalDesign begin\n")?;
		for x in &self.nodes {
			let disp = format!("{:?}", x);
			f.write_str(" ")?;
			f.write_str(disp.as_str())?;
			f.write_str("\n")?;
		}
		f.write_str("END")?;
		Ok(())
	}
}

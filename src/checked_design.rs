use std::{
	cell::RefCell,
	collections::{BTreeMap, BTreeSet, BinaryHeap, HashMap, HashSet, LinkedList},
	ops::AddAssign,
	usize,
};

type NodeId = usize;

mod checked_design_optimizations;

use itertools::{chain, izip, Itertools};

use crate::{
	checked_design::checked_design_optimizations::Optimization,
	connected_design::{CoarseExpr, ConnectedDesign},
	logical_design::{
		self, decider_parser::DeciderSop, ArithmeticOperator, DeciderOperator, LogicalDesign,
		MemoryReadPort, MemoryWritePort, Polarity, ResetSpec, Signal,
	},
	mapped_design::{BitSliceOps, Direction, FromBinStr, IntoBoolVec},
	match_or_continue, signal_lookup_table, unwrap_or_continue,
	util::{self, hash_map, hash_set, index_of, HashS},
};
use crate::{mapped_design::MappedDesign, util::HashM};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ImplementableOp {
	AndBitwise,
	OrBitwise,
	XorBitwise,
	Shl,
	Sshr,
	Srl,
	Mul,
	Div,
	Mod,
	Pow,
	Add,
	Sub,
	LessThan,
	GreaterThan,
	Equal,
	NotEqual,
	GreaterThanEqual,
	LessThanEqual,
	ReduceAnd,
	ReduceOr,
	Neg,
	Not,
	V2FRollingAccumulate,
	V2FProgRam,
	DFF,
	SDFF,
	SDFFE,
	ADFFE,
	ADFF,
	DFFE,
	LUT(usize),
	PMux(bool, usize),
	Mux,
	Memory,
	MemoryPrim,
	Rom,
	Sop(usize),
	SopNot(usize),

	Swizzle, // Imaginary cell
}

impl ImplementableOp {
	pub(crate) fn simple_string(&self) -> &'static str {
		match self {
			ImplementableOp::AndBitwise => "AndBitwise",
			ImplementableOp::OrBitwise => "OrBitwise",
			ImplementableOp::XorBitwise => "XorBitwise",
			ImplementableOp::Shl => "Shl",
			ImplementableOp::Sshr => "Sshr",
			ImplementableOp::Srl => "Srl",
			ImplementableOp::Mul => "Mul",
			ImplementableOp::Div => "Div",
			ImplementableOp::Mod => "Mod",
			ImplementableOp::Pow => "Pow",
			ImplementableOp::Add => "Add",
			ImplementableOp::Sub => "Sub",
			ImplementableOp::LessThan => "LessThan",
			ImplementableOp::GreaterThan => "GreaterThan",
			ImplementableOp::Equal => "Equal",
			ImplementableOp::NotEqual => "NotEqual",
			ImplementableOp::GreaterThanEqual => "GreaterThanEqual",
			ImplementableOp::LessThanEqual => "LessThanEqual",
			ImplementableOp::ReduceAnd => "ReduceAnd",
			ImplementableOp::ReduceOr => "ReduceOr",
			ImplementableOp::Neg => "Neg",
			ImplementableOp::Not => "Not",
			ImplementableOp::V2FRollingAccumulate => "V2FRollingAccumulate",
			ImplementableOp::V2FProgRam => "V2FProgRam",
			ImplementableOp::DFF => "DFF",
			ImplementableOp::SDFF => "SDFF",
			ImplementableOp::SDFFE => "SDFFE",
			ImplementableOp::ADFFE => "ADFFE",
			ImplementableOp::ADFF => "ADFF",
			ImplementableOp::DFFE => "DFFE",
			ImplementableOp::LUT(_) => "LUT",
			ImplementableOp::PMux(_, _) => "PMux",
			ImplementableOp::Mux => "Mux",
			ImplementableOp::Memory => "Memory",
			ImplementableOp::MemoryPrim => "MemoryPrim",
			ImplementableOp::Sop(_) => "Sop",
			ImplementableOp::SopNot(_) => "SopNot",
			ImplementableOp::Swizzle => "Swizzle",
			ImplementableOp::Rom => "Rom",
		}
	}

	pub(crate) fn get_body_type(&self) -> BodyType {
		let multipart = |op: ImplementableOp| BodyType::MultiPart { op };
		match self {
			ImplementableOp::AndBitwise => BodyType::ABY,
			ImplementableOp::OrBitwise => BodyType::ABY,
			ImplementableOp::XorBitwise => BodyType::ABY,
			ImplementableOp::Shl => multipart(*self),
			ImplementableOp::Sshr => BodyType::ABY,
			ImplementableOp::Srl => multipart(*self),
			ImplementableOp::Mul => BodyType::ABY,
			ImplementableOp::Div => BodyType::ABY,
			ImplementableOp::Mod => BodyType::ABY,
			ImplementableOp::Pow => BodyType::ABY,
			ImplementableOp::Add => BodyType::ABY,
			ImplementableOp::Sub => BodyType::ABY,
			ImplementableOp::GreaterThan => BodyType::ABY,
			ImplementableOp::LessThan => BodyType::ABY,
			ImplementableOp::Equal => BodyType::ABY,
			ImplementableOp::NotEqual => BodyType::ABY,
			ImplementableOp::GreaterThanEqual => BodyType::ABY,
			ImplementableOp::LessThanEqual => BodyType::ABY,
			ImplementableOp::V2FRollingAccumulate => BodyType::AY,
			ImplementableOp::DFF => multipart(*self),
			ImplementableOp::Swizzle => multipart(*self),
			ImplementableOp::LUT(_) => multipart(*self),
			ImplementableOp::Memory => multipart(*self),
			ImplementableOp::PMux(_, _) => multipart(*self),
			ImplementableOp::Mux => multipart(*self),
			ImplementableOp::ReduceAnd => multipart(*self),
			ImplementableOp::ReduceOr => multipart(*self),
			ImplementableOp::Neg => BodyType::AY,
			ImplementableOp::Not => BodyType::AY,
			ImplementableOp::SDFF => multipart(*self),
			ImplementableOp::SDFFE => multipart(*self),
			ImplementableOp::ADFFE => multipart(*self),
			ImplementableOp::ADFF => multipart(*self),
			ImplementableOp::DFFE => multipart(*self),
			ImplementableOp::Sop(_) => multipart(*self),
			ImplementableOp::SopNot(_) => multipart(*self),
			ImplementableOp::V2FProgRam => multipart(*self),
			ImplementableOp::MemoryPrim => multipart(*self),
			ImplementableOp::Rom => multipart(*self),
		}
	}

	pub fn get_arithmetic_op(&self) -> Option<ArithmeticOperator> {
		use ImplementableOp as Op;
		match self {
			Op::AndBitwise => Some(ArithmeticOperator::And),
			Op::OrBitwise => Some(ArithmeticOperator::Or),
			Op::XorBitwise => Some(ArithmeticOperator::Xor),
			Op::Shl => Some(ArithmeticOperator::Shl),
			Op::Sshr => Some(ArithmeticOperator::Sshr),
			Op::Mul => Some(ArithmeticOperator::Mult),
			Op::Div => Some(ArithmeticOperator::Div),
			Op::Mod => Some(ArithmeticOperator::Mod),
			Op::Pow => Some(ArithmeticOperator::Exp),
			Op::Add => Some(ArithmeticOperator::Add),
			Op::Sub => Some(ArithmeticOperator::Sub),
			_ => None,
		}
	}

	pub fn is_dff(&self) -> bool {
		matches!(
			self,
			ImplementableOp::DFF
				| ImplementableOp::SDFF
				| ImplementableOp::ADFF
				| ImplementableOp::ADFFE
				| ImplementableOp::DFFE
				| ImplementableOp::SDFFE
		)
	}

	pub fn is_memory(&self) -> bool {
		matches!(self, ImplementableOp::V2FProgRam | ImplementableOp::Memory)
	}

	pub fn get_decider_op(&self) -> Option<DeciderOperator> {
		match self {
			ImplementableOp::LessThan => Some(DeciderOperator::LessThan),
			ImplementableOp::GreaterThan => Some(DeciderOperator::GreaterThan),
			ImplementableOp::Equal => Some(DeciderOperator::Equal),
			ImplementableOp::NotEqual => Some(DeciderOperator::NotEqual),
			ImplementableOp::GreaterThanEqual => Some(DeciderOperator::GreaterThanEqual),
			ImplementableOp::LessThanEqual => Some(DeciderOperator::LessThanEqual),
			_ => None,
		}
	}
}

pub struct CheckedDesign {
	nodes: Vec<Node>,
	/// The selected game signal to use for this node. Each participating (inputs/outputs) node needs one
	signals: Vec<Signal>,
	/// This is used to calculate the fanin & fanout of any terminal (port, cell IO) to any other terminal.
	connected_design: ConnectedDesign,
	/// Nodes in connected_design -> nodes in this struct.
	connected_id_map: Vec<NodeId>,
	/// Optimization for merging constants and expressions into a combinator
	coarse_exprs: Vec<Option<CoarseExpr>>,
	associated_logic: RefCell<Vec<Option<logical_design::NodeId>>>,

	clocks: HashS<NodeId>,

	nop_cache: HashM<NodeId, NodeId>,

	// Flags
	pub promote_all_nets_to_ports: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum NodeType {
	CellInput { port: String, connected_id: usize },
	CellOutput { port: String, connected_id: usize },
	PortInput { connected_id: usize },
	PortOutput { connected_id: usize },
	PortBody { is_keepnet: bool },
	CellBody { cell_type: BodyType },
	Pruned,
}

impl NodeType {
	fn is_output(&self) -> bool {
		match self {
			NodeType::CellOutput { .. } => true,
			NodeType::PortOutput { .. } => true,
			_ => false,
		}
	}

	fn is_cell_body(&self) -> bool {
		matches!(self, NodeType::CellBody { .. })
	}

	fn mapped_terminal_name(&self) -> &str {
		match self {
			NodeType::CellInput { port, .. } => port,
			NodeType::CellOutput { port, .. } => port,
			_ => panic!(),
		}
	}

	fn get_cell_type(&self) -> BodyType {
		match self {
			NodeType::CellBody { cell_type } => cell_type.clone(),
			_ => {
				unreachable!()
			},
		}
	}

	#[allow(dead_code)]
	pub fn simple_name(&self) -> &'static str {
		match self {
			NodeType::CellInput { .. } => "CellInput",
			NodeType::CellOutput { .. } => "CellOutput",
			NodeType::PortInput { .. } => "PortInput",
			NodeType::PortOutput { .. } => "PortOutput",
			NodeType::PortBody { is_keepnet: false } => "PortBody",
			NodeType::PortBody { is_keepnet: true } => "PortBodyKeepnet",
			NodeType::CellBody { .. } => "CellBody",
			NodeType::Pruned => "Pruned",
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum BodyType {
	ABY,
	AY,
	MultiPart { op: ImplementableOp }, // This should be a superset of ABY and AY and Constant
	Constant { value: i32 },
	Nop,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum TimingBoundary {
	None,
	Pre,
	Post,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FoldedData {
	Vec(Vec<Option<DeciderSop>>),
	Single(DeciderSop),
	None,
}

impl FoldedData {
	fn unwrap_vec(&self) -> &[Option<DeciderSop>] {
		match self {
			FoldedData::Vec(pmux) => pmux.as_slice(),
			_ => panic!(),
		}
	}
	fn unwrap_vec_mut(&mut self) -> &mut [Option<DeciderSop>] {
		match self {
			FoldedData::Vec(pmux) => pmux.as_mut_slice(),
			_ => panic!(),
		}
	}

	fn is_empty(&self) -> bool {
		matches!(self, FoldedData::None)
	}
}

#[derive(Debug, Clone)]
struct Node {
	id: NodeId,
	node_type: NodeType,
	mapped_id: String,
	fanin: Vec<NodeId>,
	fanout: Vec<NodeId>,
	keep: bool,
	constants: Vec<Option<i32>>,
	folded_expressions: FoldedData,
	timing_boundary: TimingBoundary,
}

impl Default for CheckedDesign {
	fn default() -> Self {
		Self::new()
	}
}

impl CheckedDesign {
	pub fn new() -> Self {
		Self {
			nodes: vec![],
			signals: vec![],
			connected_design: ConnectedDesign::new(),
			connected_id_map: vec![],
			coarse_exprs: vec![],
			associated_logic: RefCell::new(vec![]),
			clocks: hash_set(),
			nop_cache: hash_map(),
			promote_all_nets_to_ports: false,
		}
	}

	fn node_type(&self, nodeid: NodeId) -> &NodeType {
		&self.nodes[nodeid].node_type
	}

	fn is_driver(&self, nodeid: NodeId, to_check: NodeId) -> bool {
		self.node_type(to_check).is_output() && self.nodes[nodeid].fanin.contains(&to_check)
	}

	fn new_node(&mut self, mapped_id: &str, node_type: NodeType) -> NodeId {
		let new_id = self.nodes.len();
		let connected_id = match node_type {
			NodeType::CellInput { connected_id, .. } => Some(connected_id),
			NodeType::CellOutput { connected_id, .. } => Some(connected_id),
			NodeType::PortInput { connected_id, .. } => Some(connected_id),
			NodeType::PortOutput { connected_id, .. } => Some(connected_id),
			NodeType::PortBody { .. } => None,
			NodeType::CellBody { .. } => None,
			NodeType::Pruned => unreachable!(),
		};
		self.nodes.push(Node {
			id: new_id,
			node_type,
			mapped_id: mapped_id.to_owned(),
			fanin: vec![],
			fanout: vec![],
			constants: vec![],
			folded_expressions: FoldedData::None,
			keep: false,
			timing_boundary: TimingBoundary::None,
		});
		if let Some(connected_id) = connected_id {
			if connected_id != usize::MAX {
				self.connected_id_map[connected_id] = new_id;
			}
		}
		self.coarse_exprs.push(None);

		new_id
	}

	fn set_node_coarse_expr(&mut self, id: NodeId, expr: CoarseExpr) {
		self.coarse_exprs[id] = Some(expr);
	}

	fn connect(&mut self, send: NodeId, recv: NodeId) {
		let t1 = self.node_type(send);
		let t2 = self.node_type(recv);
		match (t1, t2) {
			(NodeType::CellInput { .. }, NodeType::CellBody { .. }) => {},
			(NodeType::CellOutput { .. }, NodeType::CellInput { .. }) => {},
			(NodeType::CellOutput { .. }, NodeType::PortInput { .. }) => {},
			(NodeType::PortInput { .. }, NodeType::PortBody { .. }) => {},
			(NodeType::PortOutput { .. }, NodeType::CellInput { .. }) => {},
			(NodeType::PortOutput { .. }, NodeType::PortInput { .. }) => {},
			(NodeType::PortBody { .. }, NodeType::PortOutput { .. }) => {},
			(NodeType::CellBody { .. }, NodeType::CellOutput { .. }) => {},
			_ => assert!(false),
		};
		#[cfg(debug_assertions)]
		{
			match (t1, t2) {
				(NodeType::CellOutput { .. }, NodeType::CellInput { .. }) => {
					let xx = &self.nodes[send].fanin.get(0);
					let yy = &self.nodes[recv].fanout.get(0);
					if xx.is_some() && yy.is_some() {
						if let NodeType::CellBody { cell_type } = self.node_type(*xx.unwrap()) {
							if matches!(cell_type, BodyType::Nop) {
								assert_ne!(xx, yy);
							}
						}
					}
				},
				_ => {},
			}
		}
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

	fn initialize_nodes(&mut self, mapped_design: &MappedDesign) {
		mapped_design.for_all_top_level_io(|_, name, _port| {
			let id = self.new_node(name, NodeType::PortBody { is_keepnet: false });
			self.nodes[id].keep = true;
		});
		for (netname, net) in mapped_design.iter_netnames() {
			if !self.promote_all_nets_to_ports {
				if let Some(keep_str) = net.attributes.get("keep") {
					if keep_str.from_bin_str() != Some(1) {
						continue;
					}
				} else {
					continue;
				}
			}
			if mapped_design.is_port(netname) {
				continue;
			}

			let id = self.new_node(netname, NodeType::PortBody { is_keepnet: true });
			self.nodes[id].keep = true;
		}
		mapped_design.for_all_cells(|_, name, cell| {
			let id = self.new_node(
				name,
				NodeType::CellBody {
					cell_type: cell.cell_type.get_body_type(),
				},
			);
			self.nodes[id].keep = cell
				.attributes
				.get("keep")
				.and_then(|value| value.from_bin_str())
				.unwrap_or_default()
				> 0;
		});
		let n_nodes = self.nodes.len();
		for nodeid in 0..n_nodes {
			let node = &self.nodes[nodeid];
			let mapped_id = node.mapped_id.clone();
			match self.node_type(nodeid) {
				NodeType::PortBody { is_keepnet: false } => {
					let mapped_port = mapped_design.get_port(&node.mapped_id);
					if mapped_port.direction == Direction::Output {
						let id = self.new_node(
							&mapped_id,
							NodeType::PortInput {
								connected_id: self
									.connected_design
									.get_node_id(&mapped_id, &"A".to_owned()),
							},
						);
						self.connect(id, nodeid);
					}
					if mapped_port.direction == Direction::Input {
						let id = self.new_node(
							&mapped_id,
							NodeType::PortOutput {
								connected_id: self
									.connected_design
									.get_node_id(&mapped_id, &"Y".to_owned()),
							},
						);
						self.connect(nodeid, id);
					}
				},
				NodeType::PortBody { is_keepnet: true } => {
					// Keepnets are just outputs
					let id = self.new_node(
						&mapped_id,
						NodeType::PortInput {
							connected_id: self
								.connected_design
								.get_node_id(&mapped_id, &"A".to_owned()),
						},
					);
					self.connect(id, nodeid);
				},
				NodeType::CellBody { .. } => {
					let cell = mapped_design.get_cell(&mapped_id);

					for (_idx, connected_id, direction, terminal_name) in cell
						.get_terminal_names()
						.iter()
						.enumerate()
						.map(|(idx, terminal_name)| {
							(
								idx,
								self.connected_design.get_node_id(&mapped_id, terminal_name),
								self.connected_design.terminal_direction(terminal_name),
								terminal_name,
							)
						})
						.collect_vec()
					{
						match direction {
							Direction::Input => {
								let id = self.new_node(
									&mapped_id,
									NodeType::CellInput {
										port: terminal_name.clone(),
										connected_id,
									},
								);
								self.connect(id, nodeid);
								if cell.cell_type.is_memory() {
									let terminal_boundary =
										cell.memory_terminal_to_timing_boundary(&terminal_name);
									self.nodes[id].timing_boundary = terminal_boundary;
								}
							},
							Direction::Output => {
								let id = self.new_node(
									&mapped_id,
									NodeType::CellOutput {
										port: terminal_name.clone(),
										connected_id,
									},
								);
								self.connect(nodeid, id);
								if cell.cell_type.is_dff() {
									self.nodes[id].timing_boundary = TimingBoundary::Pre;
									self.nodes[nodeid].timing_boundary = TimingBoundary::Post;
								}
								if cell.cell_type.is_memory() {
									let terminal_boundary =
										cell.memory_terminal_to_timing_boundary(&terminal_name);
									self.nodes[id].timing_boundary = terminal_boundary;
								}
							},
							Direction::Inout => panic!("Not supported"),
						};
					}
				},
				_ => {
					unreachable!()
				},
			}
		}
	}

	fn make_preliminary_connections(&mut self) {
		for nodeid in 0..self.nodes.len() {
			match self.node_type(nodeid) {
				NodeType::PortOutput { connected_id }
				| NodeType::CellOutput { connected_id, .. } => {
					for fanout in self.connected_design.fanout[*connected_id]
						.iter()
						.map(|cid| self.connected_id_map[*cid])
						.collect_vec()
					{
						self.connect(nodeid, fanout);
					}
				},
				_ => {},
			}
		}
	}

	fn resolve_coarse_exprs(&mut self) {
		let mut cache = hash_map();
		for nodeid in 0..self.nodes.len() {
			let connected_id = *match self.node_type(nodeid) {
				NodeType::CellInput { connected_id, .. } | NodeType::PortInput { connected_id } => {
					connected_id
				},
				_ => {
					continue;
				},
			};
			let node = &self.nodes[nodeid];
			let fanin = node.fanin.clone();
			let exprs = &self.connected_design.expr[connected_id].clone();
			if exprs.len() == 2
				&& exprs[0].is_driver()
				&& exprs[1].is_constant()
				&& exprs[1].unwrap_constant_value().get_constant::<i32>() == 0
				&& fanin.len() == 1
			{
				let fiid_cnxn = match self.node_type(fanin[0]) {
					NodeType::CellOutput { connected_id, .. }
					| NodeType::PortOutput { connected_id } => *connected_id,
					_ => unreachable!(),
				};
				let driver = self.connected_design.node_info[fiid_cnxn].n_bits();
				let sink = exprs[0].n_bits();
				if driver <= sink {
					continue;
				}
			}
			if let Some(id) = cache.get(exprs) {
				self.connect(*id, nodeid);
				for expr in exprs {
					let fiid = match expr {
						CoarseExpr::DriverChunk { driver_ioid, .. } => {
							self.connected_id_map[*driver_ioid]
						},
						_ => continue,
					};
					self.disconnect(fiid, nodeid);
				}
				continue;
			}
			if exprs.len() > 1 {
				// Insert swizzle that takes in the fanin and outputs a single
				let body = self.new_node(
					"$swizzle",
					NodeType::CellBody {
						cell_type: BodyType::MultiPart {
							op: ImplementableOp::Swizzle,
						},
					},
				);
				let output = self.new_node(
					"$swizzle",
					NodeType::CellOutput {
						port: "Y".to_owned(),
						connected_id,
					},
				);
				self.connect(body, output);
				self.connect(output, nodeid);
				cache.insert(exprs.clone(), output);

				let mut counter = 0;
				for expr in exprs {
					let fiid = match expr {
						CoarseExpr::DriverChunk { driver_ioid, .. } => {
							self.connected_id_map[*driver_ioid]
						},
						_ => continue,
					};
					assert!(
						fanin.contains(&fiid),
						"Found driver in coarse expression that isn't expected in the wired design."
					);
					self.disconnect(fiid, nodeid);
					let input = self.new_node(
						"$swizzle",
						NodeType::CellInput {
							port: format!("A{}", counter),
							connected_id,
						},
					);
					self.connect(input, body);
					self.connect(fiid, input);
					self.set_node_coarse_expr(input, expr.clone());
					counter += 1;
				}
				let mut constant_val: i32 = 0;
				for expr in exprs {
					match expr {
						CoarseExpr::DriverChunk { .. } => {},
						CoarseExpr::ConstantChunk { shift, value } => {
							constant_val = (constant_val + value.get_constant::<i32>()) << *shift;
						},
					}
				}
				if constant_val != 0 {
					let input = self.new_node(
						"$swizzle",
						NodeType::CellInput {
							port: format!("A{}", counter),
							connected_id,
						},
					);
					self.connect(input, body);
					let new_constant = self.new_node(
						"$constant",
						NodeType::CellBody {
							cell_type: BodyType::Constant {
								value: constant_val,
							},
						},
					);
					let new_constant_output = self.new_node(
						"$constant",
						NodeType::CellOutput {
							port: "Y".to_owned(),
							connected_id: usize::MAX,
						},
					);
					self.connect(new_constant, new_constant_output);
					self.connect(new_constant_output, input);
				}
			} else if let CoarseExpr::ConstantChunk { shift, value } = &exprs[0] {
				let constant_val: i32 = value.get_constant();
				let new_constant = self.new_node(
					"$constant",
					NodeType::CellBody {
						cell_type: BodyType::Constant {
							value: constant_val << shift,
						},
					},
				);
				let new_constant_output = self.new_node(
					"$constant",
					NodeType::CellOutput {
						port: "Y".to_owned(),
						connected_id: usize::MAX,
					},
				);
				self.connect(new_constant, new_constant_output);
				self.connect(new_constant_output, nodeid);
			} else if let CoarseExpr::DriverChunk {
				driver_ioid,
				shift,
				bit_start,
				bit_end,
			} = &exprs[0]
			{
				if *shift == 0 && *bit_start == 0 && (*bit_end == 32 || fanin.len() == 1) {
					let fiid_cnxn = match self.node_type(fanin[0]) {
						NodeType::CellOutput { connected_id, .. }
						| NodeType::PortOutput { connected_id } => *connected_id,
						_ => unreachable!(),
					};
					let driver = self.connected_design.node_info[fiid_cnxn].n_bits();
					let sink = exprs[0].n_bits();
					if driver == sink {
						continue;
					}
				}
				let fiid = self.connected_id_map[*driver_ioid];
				assert_eq!(node.fanin.len(), 1);
				assert_eq!(fiid, node.fanin[0]);
				self.disconnect(fiid, nodeid);
				let body = self.new_node(
					"$swizzle",
					NodeType::CellBody {
						cell_type: BodyType::MultiPart {
							op: ImplementableOp::Swizzle,
						},
					},
				);
				let output = self.new_node(
					"$swizzle",
					NodeType::CellOutput {
						port: "Y".to_owned(),
						connected_id,
					},
				);
				let input = self.new_node(
					"$swizzle",
					NodeType::CellInput {
						port: "A".to_owned(),
						connected_id,
					},
				);
				self.set_node_coarse_expr(input, exprs[0].clone());
				self.connect(fiid, input);
				self.connect(input, body);
				self.connect(body, output);
				self.connect(output, nodeid);
				cache.insert(exprs.clone(), output);
			}
		}
	}

	pub fn enforce_network_requirements(&mut self, mapped_design: &MappedDesign) {
		let (_, _, _, localio_group_nodes) = self.get_groupings();
		let mut signal_choices = vec![Signal::None; self.nodes.len()];
		for local_group in &localio_group_nodes {
			let mut required_signals = hash_set::<i32>();
			for nodeid in local_group {
				if !matches!(
					self.node_type(*nodeid),
					NodeType::PortInput { .. } | NodeType::PortOutput { .. }
				) {
					continue;
				}
				let choice = signal_lookup_table::lookup_id(&self.nodes[*nodeid].mapped_id)
					.or_else(|| {
						let signal_name = mapped_design
							.get_net_attribute(&self.nodes[*nodeid].mapped_id, "v2f_signal")?;
						Some(
							signal_lookup_table::lookup_id_ignore_case(signal_name).expect(
								&format!(
								"{} has property v2f_signal=\"{}\", but it matches no game signal",
								self.nodes[*nodeid].mapped_id, signal_name
							),
							),
						)
					});
				let choice = if let Some(c) = choice {
					c
				} else {
					continue;
				};
				if !required_signals.contains(&choice) {
					required_signals.insert(choice);
					signal_choices[*nodeid] = choice.try_into().expect("Impl error");
					continue;
				}
				// This port cant use this signal internally, so we must rename it with a nop.
				if matches!(self.node_type(*nodeid), NodeType::PortOutput { .. }) {
					for foid in self.nodes[*nodeid].fanout.clone() {
						self.insert_nop_between(*nodeid, foid);
					}
				}
				if matches!(self.node_type(*nodeid), NodeType::PortInput { .. }) {
					for fiid in self.nodes[*nodeid].fanin.clone() {
						self.insert_nop_between(fiid, *nodeid);
					}
				}
			}
		}
	}

	fn calculate_and_validate_signal_choices(&self, mapped_design: &MappedDesign) -> Vec<Signal> {
		let mut signal_choices = vec![Signal::None; self.nodes.len()];
		let mut fallback_id = 0;
		for node in &self.nodes {
			if matches!(node.node_type, NodeType::PortBody { .. }) {
				let mut choice = Signal::None;
				if let Some(fiid) = node.fanin.first() {
					let attached = self.get_attached_nodes(*fiid);
					for attachedid in attached.iter() {
						if signal_choices[*attachedid].is_some() {
							if choice.is_some() && choice != signal_choices[*attachedid] {
								panic!(
								"Conflicting signal choices for node {} attached to {:?} with previous choice {:?} and new choice {:?}.",
								fiid, attached, choice, signal_choices[*attachedid])
							}
							choice = signal_choices[*attachedid];
						}
					}
				}
				if let Some(foid) = node.fanout.first() {
					let attached = self.get_attached_nodes(*foid);
					for attachedid in attached.iter() {
						if signal_choices[*attachedid].is_some() {
							if choice.is_some() && choice != signal_choices[*attachedid] {
								panic!(
								"Conflicting signal choices for node {} attached to {:?} with previous choice {:?} and new choice {:?}.",
								foid, attached, choice, signal_choices[*attachedid])
							}
							choice = signal_choices[*attachedid];
						}
					}
				}
				if choice.is_none() {
					choice = signal_lookup_table::lookup_id(&node.mapped_id)
						.or_else(|| {
							let signal_name =
								mapped_design.get_net_attribute(&node.mapped_id, "v2f_signal")?;
							Some(
								signal_lookup_table::lookup_id_ignore_case(signal_name).expect(
									&format!(
								"{} has property v2f_signal=\"{}\", but it matches no game signal",
								node.mapped_id, signal_name
							),
								),
							)
						})
						.unwrap_or_else(|| {
							let ret = fallback_id;
							fallback_id += 1;
							fallback_id %= signal_lookup_table::n_ids();
							ret
						})
						.try_into()
						.unwrap();
				}
				signal_choices[node.id] = choice;
				if let Some(fiid) = node.fanin.first() {
					signal_choices[*fiid] = choice;
				}
				if let Some(foid) = node.fanout.first() {
					signal_choices[*foid] = choice;
				}
			}
			if matches!(node.node_type, NodeType::CellBody { .. }) {
				for (idx, val_opt) in node.constants.iter().enumerate() {
					if let Some(val) = val_opt {
						signal_choices[node.fanin[idx]] = Signal::Constant(*val)
					}
				}
			}
		}
		for nodeid in 0..self.nodes.len() {
			if signal_choices[nodeid].is_some() {
				continue;
			}
			let attached = self.get_attached_nodes(nodeid);
			let mut choice = Signal::None;
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
					choice = signal_choices[*attachedid];
				}
			}
			signal_choices[nodeid] = choice;
		}
		signal_choices
	}

	fn elaborate_signal_choices(
		&self,
		signal_choices: &mut Vec<Signal>,
		mapped_design: &MappedDesign,
	) -> Result<(), Vec<NodeId>> {
		let topo_order = self.get_topo_order(mapped_design);
		for nodeid in topo_order {
			let choice = signal_choices[nodeid];
			if choice.is_some() {
				continue;
			}
			let node = &self.nodes[nodeid];
			match node.node_type {
				NodeType::CellInput { .. } => {
					if signal_choices[node.fanin[0]].is_some() {
						signal_choices[nodeid] = signal_choices[node.fanin[0]];
						continue;
					}
				},
				NodeType::CellOutput { .. } => {
					if let Some(sink_signal) = node
						.fanout
						.iter()
						.map(|foid| signal_choices[*foid])
						.find(Signal::is_some)
					{
						signal_choices[nodeid] = sink_signal;
						continue;
					}
				},
				NodeType::PortInput { .. }
				| NodeType::PortOutput { .. }
				| NodeType::PortBody { .. }
				| NodeType::CellBody { .. } => {
					continue;
				},
				NodeType::Pruned => {
					continue;
				},
			}
			let local_io = self.get_local_cell_io_network(nodeid);
			let set_io = local_io
				.iter()
				.fold(HashSet::<i32>::new(), |mut set, ioid| {
					if let Signal::Id(sig) = signal_choices[*ioid] {
						set.insert(sig);
					};
					set
				});
			let mut sig = 0;
			for id in local_io.iter() {
				while set_io.contains(&sig) {
					sig += 1;
				}
				let sig_decision: Signal = match sig.try_into() {
					Ok(s) => s,
					Err(_) => {
						return Err(local_io);
					},
				};
				if self.set_signal(signal_choices, *id, sig_decision) {
					sig += 1;
				}
			}
		}
		Ok(())
	}

	fn signals_correctness_check(&self, signal_choices: &[Signal]) {
		// Final correctness check
		for nodeid in 0..self.nodes.len() {
			let node = &self.nodes[nodeid];
			match node.node_type {
				NodeType::CellInput { .. } | NodeType::PortInput { .. } => {
					assert!(signal_choices[node.id].is_some());
					assert!(
						matches!(signal_choices[node.id], Signal::Constant(_))
							== node.fanin.is_empty()
					);
					for fiid in &node.fanin {
						assert_eq!(signal_choices[node.id], signal_choices[*fiid]);
					}
				},
				NodeType::CellOutput { .. } | NodeType::PortOutput { .. } => {
					assert!(signal_choices[node.id].is_some());
					assert!(matches!(signal_choices[node.id], Signal::Id(_)));
					for foid in &node.fanout {
						assert_eq!(signal_choices[node.id], signal_choices[*foid]);
					}
				},
				NodeType::PortBody { .. } => {
					assert!(signal_choices[node.id].is_some());
					assert!(matches!(signal_choices[node.id], Signal::Id(_)));
					for foid in &node.fanout {
						assert_eq!(signal_choices[node.id], signal_choices[*foid]);
					}
					for fiid in &node.fanin {
						assert_eq!(signal_choices[node.id], signal_choices[*fiid]);
					}
				},
				NodeType::CellBody { .. } => {
					assert!(signal_choices[node.id].is_none());
					let mut choices = hash_map();
					for fiid in &node.fanin {
						let choice = signal_choices[*fiid];
						if self.nodes[*fiid].fanin.is_empty() {
							continue;
						}
						let driver = self.nodes[*fiid].fanin[0];
						if choices.contains_key(&choice) {
							//assert!(choices[&choice] == driver);
						} else {
							choices.insert(choice, driver);
						}
					}
				},
				NodeType::Pruned => {},
			}
		}
		for choice in signal_choices {
			let _sig: Signal = match choice {
				Signal::Id(id) => (*id).try_into().expect("Impl error"),
				_ => Signal::None,
			};
		}
	}

	pub fn build_from(&mut self, mapped_design: &MappedDesign) {
		self.connected_design.promote_all_nets_to_ports = self.promote_all_nets_to_ports;
		self.connected_design.build_from(mapped_design);
		self.connected_id_map = vec![usize::MAX; self.connected_design.max_id() + 1];
		self.initialize_nodes(mapped_design);
		self.make_preliminary_connections();
		self.resolve_coarse_exprs();
		self.enforce_network_requirements(mapped_design);

		self.check_connections();

		loop {
			let n_pruned = self.optimize_graph(mapped_design);
			if n_pruned > 0 {
				println!("Pruned {n_pruned} nodes while optimizing.");
			} else {
				break;
			}
		}
		self.check_connections();
		let mut signal_choices = vec![];
		loop {
			signal_choices = self.calculate_and_validate_signal_choices(mapped_design);
			match self.elaborate_signal_choices(&mut signal_choices, mapped_design) {
				Ok(_) => break,
				Err(local_io) => {
					self.partition_io_network(local_io);
					signal_choices.resize(self.nodes.len(), Signal::None);
				},
			}
		}
		self.signals_correctness_check(&signal_choices);
		self.signals = signal_choices;
		self.update_folded_data();
		self.check_connections();
		self.identify_clocks(mapped_design);
	}

	fn get_inputs_through_body(&self, id: NodeId) -> &[NodeId] {
		let node = &self.nodes[id];
		assert!(matches!(
			node.node_type,
			NodeType::PortOutput { .. } | NodeType::CellOutput { .. }
		));
		let body = &self.nodes[node.fanin[0]];
		&body.fanin
	}

	fn insert_nop_between(&mut self, first: NodeId, second: NodeId) {
		assert!(matches!(
			self.node_type(second),
			NodeType::PortInput { .. } | NodeType::CellInput { .. }
		));
		assert!(matches!(
			self.node_type(first),
			NodeType::PortOutput { .. } | NodeType::CellOutput { .. }
		));
		assert!(first != second);
		let y = if self.nop_cache.contains_key(&first) {
			let y = self.nop_cache[&first];
			let inputs = self.get_inputs_through_body(y);
			if inputs.contains(&second) {
				println!("{second} asking to add Nop to itself.");
				return;
			}
			y
		} else {
			let nop = self.new_node(
				"$nop",
				NodeType::CellBody {
					cell_type: BodyType::Nop,
				},
			);
			let a = self.new_node(
				"$nop",
				NodeType::CellInput {
					port: "A".to_string(),
					connected_id: usize::MAX,
				},
			);
			let y = self.new_node(
				"$nop",
				NodeType::CellOutput {
					port: "Y".to_owned(),
					connected_id: usize::MAX,
				},
			);
			self.connect(a, nop);
			self.connect(nop, y);
			self.connect(first, a);
			self.nop_cache.insert(first, y);
			y
		};
		self.disconnect(first, second);
		self.connect(y, second);
	}

	fn partition_io_network(&mut self, mut local_io: Vec<NodeId>) {
		use metis;
		{
			let mut seen = HashS::default();
			let mut idx = 0;
			while idx < local_io.len() {
				let id = local_io[idx];
				let node = &self.nodes[id];
				match &node.node_type {
					NodeType::CellInput { .. } | NodeType::PortInput { .. } => {
						if seen.contains(&node.fanout[0]) {
							//continue;
						} else {
							seen.insert(node.fanout[0]);
							local_io.push(node.fanout[0]);
						}
					},
					NodeType::CellOutput { .. } | NodeType::PortOutput { .. } => {
						if seen.contains(&node.fanin[0]) {
							//continue;
						} else {
							seen.insert(node.fanin[0]);
							local_io.push(node.fanin[0]);
						}
					},
					_ => {},
				}
				idx += 1;
			}
		}
		let id_to_idx = local_io
			.iter()
			.enumerate()
			.map(|(idx, id)| (*id, idx))
			.collect::<HashM<NodeId, usize>>();
		let mut connectivity = vec![vec![]; local_io.len()];
		for (idx, id) in local_io.iter().enumerate() {
			let attached = self.get_attached_nodes(*id);
			connectivity[idx] = attached.into_iter().map(|id| id_to_idx[&id]).collect();
			let node = &self.nodes[*id];
			match &node.node_type {
				NodeType::CellInput { .. } | NodeType::PortInput { .. } => {
					connectivity[idx].push(id_to_idx[&node.fanout[0]]);
				},
				NodeType::CellOutput { .. } | NodeType::PortOutput { .. } => {
					connectivity[idx].push(id_to_idx[&node.fanin[0]]);
				},
				NodeType::CellBody { .. } | NodeType::PortBody { .. } => {
					for fiid in &node.fanin {
						if id_to_idx.contains_key(fiid) {
							connectivity[idx].push(id_to_idx[fiid]);
						}
					}
					for foid in &node.fanout {
						if id_to_idx.contains_key(foid) {
							connectivity[idx].push(id_to_idx[foid]);
						}
					}
				},
				_ => {},
			}
		}
		let n_drivers = local_io
			.iter()
			.filter(|id| {
				matches!(
					self.node_type(**id),
					NodeType::PortOutput { .. } | NodeType::CellOutput { .. }
				)
			})
			.count();
		let min_n_parts = (n_drivers / signal_lookup_table::n_ids() as usize).max(2) as i32;
		let (adj, idx_adj) = crate::util::convert_connectivity_to_csr(&connectivity);
		let mut partition = vec![0; connectivity.len()];
		let graph = metis::Graph::new(1, min_n_parts, &idx_adj, &adj).unwrap();
		let graph = graph.set_option(metis::option::UFactor(150));
		let ret = graph.part_recursive(&mut partition).unwrap() / 2;

		println!("Have to incur ~{ret} nops to enforce network requirements.");
		for (idx, id) in local_io.iter().enumerate() {
			let node = &self.nodes[*id];
			match &node.node_type {
				NodeType::PortInput { .. } | NodeType::CellInput { .. } => {
					let has_been_cut = {
						let body_id = node.fanout[0];
						let source_id = if node.fanin.is_empty() {
							continue;
						} else {
							node.fanin[0]
						};
						let source_body_id = self.nodes[source_id].fanin[0];
						partition[idx] != partition[id_to_idx[&body_id]]
							|| partition[idx] != partition[id_to_idx[&source_id]]
							|| partition[idx] != partition[id_to_idx[&source_body_id]]
					};
					let fiid = node.fanin[0];
					if has_been_cut {
						self.insert_nop_between(fiid, *id);
					};
				},
				_ => {},
			}
		}
	}

	fn set_signal(&self, signals: &mut Vec<Signal>, nodeid: NodeId, signal: Signal) -> bool {
		let node = &self.nodes[nodeid];
		if signals[nodeid].is_some() {
			return false;
		}
		match &node.node_type {
			NodeType::CellInput { .. } | NodeType::PortInput { .. } => {
				let mut driver = NodeId::MAX;
				for fiid in &node.fanin {
					if self.is_driver(nodeid, *fiid) {
						driver = *fiid;
						break;
					}
				}
				if signals[driver].is_none() {
					self.set_signal(signals, driver, signal)
				} else {
					false
				}
			},
			NodeType::CellOutput { .. } | NodeType::PortOutput { .. } => {
				signals[nodeid] = signal;
				for foid in &node.fanout {
					signals[*foid] = signal;
				}
				true
			},
			NodeType::PortBody { .. } => false,
			NodeType::CellBody { .. } => false,
			NodeType::Pruned => false,
		}
	}

	fn get_other_input_nodes(&self, nodeid: NodeId) -> Vec<NodeId> {
		assert!(matches!(
			self.node_type(nodeid),
			NodeType::CellInput { .. } | NodeType::PortInput { .. }
		));
		let body_id = self.nodes[nodeid].fanout[0];
		self.nodes[body_id]
			.fanin
			.iter()
			.filter(|id| **id != nodeid)
			.copied()
			.collect()
	}

	fn get_other_output_nodes(&self, nodeid: NodeId) -> Vec<NodeId> {
		assert!(matches!(
			self.node_type(nodeid),
			NodeType::CellOutput { .. } | NodeType::PortOutput { .. }
		));
		let body_id = self.nodes[nodeid].fanin[0];
		self.nodes[body_id]
			.fanout
			.iter()
			.filter(|id| **id != nodeid)
			.copied()
			.collect()
	}

	fn get_local_cell_io_network(&self, nodeid: NodeId) -> Vec<NodeId> {
		match self.node_type(nodeid) {
			NodeType::CellBody { .. } | NodeType::PortBody { .. } => {
				return vec![];
			},
			_ => {},
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
				NodeType::CellInput { .. } | NodeType::PortInput { .. } => {
					retval.push(curid);
					queue.extend(self.get_other_input_nodes(curid));
					for fiid in &self.nodes[curid].fanin {
						queue.insert(*fiid);
					}
				},
				NodeType::CellOutput { .. } | NodeType::PortOutput { .. } => {
					retval.push(curid);
					queue.extend(self.get_other_output_nodes(curid));
					for foid in &self.nodes[curid].fanout {
						queue.insert(*foid);
					}
				},
				NodeType::PortBody { .. } | NodeType::CellBody { .. } => {
					panic!("Implementer is a fucking moron.")
				},
				NodeType::Pruned => {},
			}
		}
		retval
	}

	fn get_attached_nodes(&self, nodeid: NodeId) -> Vec<NodeId> {
		match self.node_type(nodeid) {
			NodeType::CellBody { .. } | NodeType::PortBody { .. } => {
				return vec![];
			},
			_ => {},
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
				},
				NodeType::CellOutput { .. } => {
					queue.extend(self.nodes[curid].fanout.iter());
				},
				NodeType::PortInput { .. } => {
					queue.extend(self.nodes[curid].fanin.iter());
					queue.extend(self.nodes[self.nodes[curid].fanout[0]].fanout.iter());
				},
				NodeType::PortOutput { .. } => {
					queue.extend(self.nodes[curid].fanout.iter());
					queue.extend(self.nodes[self.nodes[curid].fanin[0]].fanin.iter());
				},
				NodeType::CellBody { .. } | NodeType::PortBody { .. } => {
					panic!("Implementer is a fucking moron")
				},
				NodeType::Pruned => {},
			}
		}
		retval
	}

	fn get_groupings(&self) -> (Vec<i32>, Vec<i32>, Vec<Vec<NodeId>>, Vec<Vec<NodeId>>) {
		let mut localio_group_num = 0;
		let mut attached_group_num = 0;
		let mut attached_groups = vec![0; self.nodes.len()];
		let mut attached_group_nodes = vec![vec![]];
		let mut localio_groups = vec![0; self.nodes.len()];
		let mut localio_group_nodes = vec![vec![]];
		for nodeid in 0..self.nodes.len() {
			if localio_groups[nodeid] != 0 {
				continue;
			}
			let localio = self.get_local_cell_io_network(nodeid);
			if localio.is_empty() {
				continue;
			}
			let attached = self.get_attached_nodes(nodeid);
			localio_group_num += 1;
			attached_group_num += 1;
			let l_group = localio_group_num;
			let a_group = attached_group_num;
			for attached_id in &attached {
				attached_groups[*attached_id] = a_group;
			}
			for localio_id in &localio {
				localio_groups[*localio_id] = l_group;
			}
			attached_group_nodes.push(attached);
			localio_group_nodes.push(localio);
		}
		(
			attached_groups,
			localio_groups,
			attached_group_nodes,
			localio_group_nodes,
		)
	}

	fn get_topo_order(&self, mapped_design: &MappedDesign) -> Vec<NodeId> {
		let mut topo_seen = HashSet::new();
		let mut topological_order = vec![];
		let mut root_nodes = vec![];
		for node in &self.nodes {
			if node.fanin.is_empty()
				|| matches!(node.timing_boundary, TimingBoundary::Pre)
				|| matches!(
					node.node_type,
					NodeType::CellBody {
						cell_type: BodyType::Constant { .. }
					}
				) {
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
			if matches!(self.nodes[id].timing_boundary, TimingBoundary::Post) {
				continue;
			}
			for foid in &self.nodes[id].fanout {
				if matches!(self.nodes[*foid].timing_boundary, TimingBoundary::Pre) {
					continue;
				}
				if self.nodes[*foid]
					.fanin
					.iter()
					.filter(|fiid| {
						!matches!(self.nodes[**fiid].timing_boundary, TimingBoundary::Post)
					})
					.all(|fiid| topo_seen.contains(fiid))
				{
					queue.push_back(*foid);
				}
			}
		}
		#[cfg(debug_assertions)]
		if topological_order.len() != self.nodes.len() {
			for node in &self.nodes {
				if !topo_seen.contains(&node.id) {
					println!("{:?}", node);
				}
			}
		}
		if topological_order.len() != self.nodes.len() {
			let mut unseen = hash_set();
			for i in 0..self.nodes.len() {
				if !topo_seen.contains(&i) {
					unseen.insert(i);
				}
			}

			self.save_dot(mapped_design, Some(&unseen), "failed_checked_design.dot");
			assert_eq!(topological_order.len(), self.nodes.len());
		}

		topological_order
	}

	pub fn apply_onto(&self, logical_design: &mut LogicalDesign, mapped_design: &MappedDesign) {
		type LID = logical_design::NodeId;
		use logical_design::Signal;
		logical_design.set_description(mapped_design.get_top_source());
		let mut logic_map: Vec<Option<LID>> = vec![None; self.nodes.len()];
		for (nodeid, node) in self.nodes.iter().enumerate() {
			match &node.node_type {
				NodeType::Pruned => {},
				NodeType::CellOutput { .. } | NodeType::PortOutput { .. } => {
					if logic_map[nodeid].is_none() {
						logic_map[nodeid] = logic_map[node.fanin[0]];
					}
				},
				NodeType::PortInput { .. } | NodeType::CellInput { .. } => {
					if logic_map[nodeid].is_none() {
						logic_map[nodeid] = Some(logical_design.add_wire_floating_red());
						logical_design.connect_red(
							logic_map[nodeid].unwrap(),
							logic_map[node.fanout[0]].unwrap(),
						);
					}
				},
				NodeType::PortBody { .. } => {
					if !node.fanout.is_empty() {
						let new_id =
							logical_design.add_constant(vec![self.signals[nodeid]], vec![0]);
						logical_design.set_description_node(new_id, node.mapped_id.clone());
						if self.clocks.contains(&nodeid) {
							let (wire, gate) =
								logical_design.add_edge_detector(self.signals[nodeid]);
							logic_map[nodeid] = Some(gate);
							logical_design.connect_red(new_id, wire);
						} else {
							logic_map[nodeid] = Some(new_id);
						}

						logical_design.mark_as_port(
							new_id,
							Direction::Input,
							node.mapped_id.clone(),
							self.signals[nodeid],
						);
					} else if !node.fanin.is_empty() {
						let new_id = logical_design.add_lamp((
							self.signals[nodeid],
							logical_design::DeciderOperator::NotEqual,
							Signal::Constant(0),
						));
						logic_map[nodeid] = Some(new_id);
						logical_design.mark_as_port(
							new_id,
							Direction::Output,
							node.mapped_id.clone(),
							self.signals[nodeid],
						);
					}
				},
				NodeType::CellBody { cell_type } => match cell_type {
					BodyType::ABY => {
						let sig_left = self.signals[node.fanin[0]];
						let sig_right = self.signals[node.fanin[1]];
						let sig_out = self.signals[node.fanout[0]];
						let (input_w, output) = logical_design.add_binary_op(
							mapped_design.get_cell(&node.mapped_id).cell_type,
							sig_left,
							sig_right,
							sig_out,
						);
						logic_map[nodeid] = Some(output);
						logic_map[node.fanout[0]] = Some(output);
						logic_map[node.fanin[0]] = Some(input_w);
						logic_map[node.fanin[1]] = Some(input_w);
					},
					BodyType::Constant { value } => {
						logic_map[nodeid] = Some(
							logical_design
								.add_constant(vec![self.signals[node.fanout[0]]], vec![*value]),
						);
					},
					BodyType::Nop => {
						let sig_in = self.signals[node.fanin[0]];
						let sig_out = self.signals[node.fanout[0]];
						let (wire, nop) = logical_design.add_nop_with_wire(sig_in, sig_out);
						logic_map[nodeid] = Some(nop);
						logic_map[node.fanout[0]] = Some(nop);
						logic_map[node.fanin[0]] = Some(wire);
					},
					BodyType::AY => {
						let sig_in = self.signals[node.fanin[0]];
						let sig_out = self.signals[node.fanout[0]];
						let (wire, output) = logical_design.add_unary_op(
							mapped_design.get_cell(&node.mapped_id).cell_type,
							sig_in,
							sig_out,
						);
						logic_map[nodeid] = Some(output);
						logic_map[node.fanout[0]] = Some(output);
						logic_map[node.fanin[0]] = Some(wire);
					},
					BodyType::MultiPart { op } => {
						let impl_op = *op;

						let sig_in = node
							.fanin
							.iter()
							.map(|fiid| self.signals[*fiid])
							.collect_vec();
						let sig_out = node
							.fanout
							.iter()
							.map(|foid| self.signals[*foid])
							.collect_vec();
						let in_ports = node
							.fanin
							.iter()
							.map(|fiid| self.nodes[*fiid].node_type.mapped_terminal_name())
							.collect_vec();
						let out_ports = node
							.fanout
							.iter()
							.map(|foid| self.nodes[*foid].node_type.mapped_terminal_name())
							.collect_vec();
						let (ids_in, ids_out) = self.apply_multipart_op(
							nodeid,
							logical_design,
							mapped_design,
							impl_op,
							mapped_design.get_cell_option(&node.mapped_id),
							sig_in,
							sig_out,
							in_ports,
							out_ports,
						);
						for (idx_common, fiid) in node.fanin.iter().enumerate() {
							logic_map[*fiid] = Some(ids_in[idx_common]);
						}
						for (idx_common, foid) in node.fanout.iter().enumerate() {
							logic_map[*foid] = Some(ids_out[idx_common]);
						}
					},
				},
			}
		}
		let mut histogram: BTreeMap<&str, usize> = BTreeMap::<_, _>::new();
		let mut histogram2: BTreeMap<(&str, &str), usize> = BTreeMap::<_, _>::new();
		let find_impl_string = |node: &Node| {
			let cell_type = match &node.node_type {
				NodeType::CellBody { cell_type } => cell_type,
				_ => unreachable!(),
			};
			match cell_type {
				BodyType::ABY | BodyType::AY => mapped_design
					.get_cell(&node.mapped_id)
					.cell_type
					.simple_string(),
				BodyType::MultiPart { op } => op.simple_string(),
				BodyType::Constant { .. } => "Constant",
				BodyType::Nop => "Nop",
			}
		};
		for node in &self.nodes {
			if !matches!(node.node_type, NodeType::CellBody { .. }) {
				continue;
			}
			let impl_first = find_impl_string(node);
			histogram.entry(impl_first).or_default().add_assign(1);
			for pin_out in &node.fanout {
				for pin_in in &self.nodes[*pin_out].fanout {
					let body_id = self.nodes[*pin_in].fanout[0];
					let node_second = &self.nodes[body_id];
					if !matches!(node_second.node_type, NodeType::CellBody { .. }) {
						continue;
					}
					let impl_second = find_impl_string(node_second);
					histogram2
						.entry((impl_first, impl_second))
						.or_default()
						.add_assign(1);
				}
			}
		}
		{
			println!("Cell counts:");
			for (name, count) in histogram.iter() {
				println!("    {name}: {count}");
			}
			println!("Cell bigram:");
			for (name, count) in histogram2
				.iter()
				.sorted_by(|(_, a), (_, b)| a.cmp(b))
				.filter(|(_, v)| **v > 20)
			{
				println!("    {name:?}: {count}");
			}
		}

		for (nodeid, node) in self.nodes.iter().enumerate() {
			match &node.node_type {
				NodeType::CellInput { .. } | NodeType::PortInput { .. } => {
					if node.fanin.is_empty() {
						continue;
					}
					logical_design.connect_red(
						logic_map[node.fanin[0]].unwrap(),
						logic_map[nodeid].unwrap(),
					);
				},
				_ => {},
			}
		}
		for (nodeid, node) in self.nodes.iter().enumerate() {
			match &node.node_type {
				NodeType::Pruned => {},
				NodeType::CellInput { port, .. } => {
					let combid = **logical_design
						.get_node(logic_map[nodeid].unwrap())
						.iter_fanout_both()
						.collect_vec()
						.first()
						.unwrap();
					logical_design
						.append_description(combid, format!("{}:{}", node.mapped_id, port).as_str())
				},
				NodeType::PortInput { .. } => {
					let combid = **logical_design
						.get_node(logic_map[nodeid].unwrap())
						.iter_fanout_both()
						.collect_vec()
						.first()
						.unwrap();
					logical_design.append_description(combid, node.mapped_id.as_str())
				},
				NodeType::CellOutput { port, .. } => logical_design.append_description(
					logic_map[nodeid].unwrap(),
					format!("{}:{}", node.mapped_id, port).as_str(),
				),
				NodeType::PortOutput { .. } => {},
				NodeType::PortBody { .. } => logical_design
					.append_description(logic_map[nodeid].unwrap(), node.mapped_id.as_str()),
				NodeType::CellBody { .. } => {},
			}
		}
		#[cfg(false)]
		for (nodeid, node) in logical_design.nodes.iter_mut().enumerate() {
			if node.is_arithmetic() || node.is_decider() {
				node.description = Some(format!("NodeId({})", nodeid));
			}
			if node.is_constant() {
				match &mut node.description {
					Some(descr) => descr.add_assign(&format!("\nNodeId({})", nodeid)),
					None => {
						node.description = Some(format!("NodeId({})", nodeid));
					},
				}
			}
		}
		self.associated_logic.replace(logic_map);
		let mut bad_design = false;
		for node in &logical_design.nodes {
			if node.is_arithmetic() || node.is_decider() {
				if node.fanout_empty() {
					println!("ERROR: dangling combinator. Node:\n {node:?}");
					bad_design = true;
				}
			}
			if node.is_constant() {
				if node.fanout_empty() {
					println!("WARN: dangling constant. Node:\n {node:?}");
				}
			}
		}
		for node in &logical_design.nodes {
			for foid in &node.fanout_red {
				let node2 = &logical_design.nodes[foid.0];
				assert!(node2.fanin_red.contains(&node.id));
			}
			for foid in &node.fanout_green {
				let node2 = &logical_design.nodes[foid.0];
				assert!(node2.fanin_green.contains(&node.id));
			}
			for foid in &node.fanin_red {
				let node2 = &logical_design.nodes[foid.0];
				assert!(node2.fanout_red.contains(&node.id));
			}
			for foid in &node.fanin_green {
				let node2 = &logical_design.nodes[foid.0];
				assert!(node2.fanout_green.contains(&node.id));
			}
		}
		if bad_design {
			panic!("Shoudln't proceed due to bad logical design construction.");
		}
		self.report_timing_save_worst_path(logical_design, mapped_design);
	}

	fn apply_multipart_op(
		&self,
		nodeid: NodeId,
		logical_design: &mut LogicalDesign,
		_mapped_design: &MappedDesign,
		op: ImplementableOp,
		mapped_cell: Option<&crate::mapped_design::Cell>,
		sig_in: Vec<Signal>,
		sig_out: Vec<Signal>,
		mapped_in_port: Vec<&str>,
		mapped_out_port: Vec<&str>,
	) -> (Vec<logical_design::NodeId>, Vec<logical_design::NodeId>) {
		let constants_fallback = vec![None; self.nodes[nodeid].fanin.len()];
		let constants = if self.nodes[nodeid].constants.is_empty() {
			&constants_fallback
		} else {
			&self.nodes[nodeid].constants
		};
		let sig_in = izip!(constants.iter(), sig_in.iter())
			.map(|(val_opt, sig_in)| {
				if let Some(val) = val_opt {
					Signal::Constant(*val)
				} else {
					*sig_in
				}
			})
			.collect_vec();
		let (input_wires, outputs) = match op {
			ImplementableOp::Shl => {
				let mult = logical_design.add_arithmetic(
					(Signal::Id(0), ArithmeticOperator::Mult, Signal::Id(1)),
					sig_out[0],
				);
				let pow = logical_design.add_arithmetic(
					(Signal::Constant(2), ArithmeticOperator::Exp, sig_in[1]),
					Signal::Id(1),
				);
				let nop = logical_design.add_nop(sig_in[0], Signal::Id(0));
				logical_design.add_wire_red_simple(nop, mult);
				logical_design.add_wire_red_simple(pow, mult);
				(
					vec![
						logical_design.add_wire_red(vec![], vec![nop]),
						logical_design.add_wire_red(vec![], vec![pow]),
					],
					vec![mult],
				)
			},
			ImplementableOp::Swizzle => {
				let fi_exprs = self.nodes[nodeid]
					.fanin
					.iter()
					.map(|fiid| self.coarse_exprs[*fiid].clone())
					.collect_vec();
				let (input_wires, output_comb) =
					logical_design.add_swizzle(sig_in, fi_exprs, sig_out[0]);
				(input_wires, vec![output_comb])
			},
			ImplementableOp::LUT(_) => {
				let folded_expressions =
					if let FoldedData::Vec(x) = &self.nodes[nodeid].folded_expressions {
						x.as_slice()
					} else {
						&[]
					};
				let (input_wires, output_comb) = logical_design.add_lut(
					sig_in,
					sig_out[0],
					mapped_cell.unwrap().parameters["LUT"]
						.into_bool_vec()
						.unwrap()
						.into_iter()
						.rev()
						.collect_vec(),
					mapped_cell.unwrap().parameters["WIDTH"].unwrap_bin_str(),
					folded_expressions,
				);
				(input_wires, vec![output_comb])
			},
			ImplementableOp::Memory => {
				let cell = mapped_cell.unwrap();
				let n_rd_ports = cell.parameters["RD_PORTS"].unwrap_bin_str();
				let n_wr_ports = cell.parameters["WR_PORTS"].unwrap_bin_str();
				let rd_clk_polarity = cell.parameters["RD_CLK_POLARITY"]
					.chars()
					.map(|c| match c {
						'0' => Polarity::Negative,
						_ => Polarity::Positive,
					})
					.collect_vec();

				let wr_clk_polarity = cell.parameters["WR_CLK_POLARITY"]
					.chars()
					.map(|c| match c {
						'0' => Polarity::Negative,
						_ => Polarity::Positive,
					})
					.collect_vec();

				let mut rd_ports = vec![];
				for i in 0..n_rd_ports {
					let addr_idx = index_of(&mapped_in_port, &format!("RD_ADDR_{i}")).unwrap();
					let data_idx = index_of(&mapped_out_port, &format!("RD_DATA_{i}")).unwrap();
					let clk_idx = index_of(&mapped_in_port, &format!("RD_CLK_{i}"));
					let en_idx = index_of(&mapped_in_port, &format!("RD_EN_{i}"));
					let arst_idx = index_of(&mapped_in_port, &format!("RD_ARST_{i}"));
					let srst_idx = index_of(&mapped_in_port, &format!("RD_SRST_{i}"));
					let rst_spec = if let Some(idx) = arst_idx {
						ResetSpec::Async(sig_in[idx])
					} else if let Some(idx) = srst_idx {
						ResetSpec::Sync(sig_in[idx])
					} else {
						ResetSpec::Disabled
					};
					let mut idx_offset = 1;
					if clk_idx.is_some() {
						assert!(addr_idx + idx_offset == clk_idx.unwrap());
						idx_offset += 1;
					}
					if en_idx.is_some() {
						assert!(addr_idx + idx_offset == en_idx.unwrap());
						idx_offset += 1;
					}
					if arst_idx.is_some() {
						assert!(addr_idx + idx_offset == arst_idx.unwrap());
					} else if srst_idx.is_some() {
						assert!(addr_idx + idx_offset == srst_idx.unwrap());
					}
					rd_ports.push(MemoryReadPort {
						addr: sig_in[addr_idx],
						data: sig_out[data_idx],
						clk: clk_idx.map(|idx| sig_in[idx]),
						en: en_idx.map(|idx| sig_in[idx]),
						rst: rst_spec,
						transparent: false,
						clk_polarity: rd_clk_polarity[i],
					});
				}

				if n_wr_ports == 0 {
					let mut ret_in = vec![];
					let mut ret_out = vec![];
					let width: usize = cell.parameters["WIDTH"].unwrap_bin_str();
					assert!(width <= 32, "Too wide");
					let rom_values = cell.parameters["INIT"]
						.chars()
						.rev()
						.chunks(width)
						.into_iter()
						.map(|rom_value| {
							rom_value
								.collect_vec()
								.iter()
								.rev()
								.collect::<String>()
								.unwrap_bin_str() as i32
						})
						.collect_vec();
					let rd_ports = logical_design.add_rom(
						rd_ports,
						rom_values,
						cell.attributes
							.get("density")
							.map(|d| d.unwrap_bin_str() as i32),
					);
					for p in rd_ports {
						ret_in.push(p.addr_wire);
						ret_out.push(p.data);
						if let Some(clk_wire) = p.clk_wire {
							ret_in.push(clk_wire);
						}
						if let Some(en_wire) = p.en_wire {
							ret_in.push(en_wire);
						}
						if let Some(rst_wire) = p.rst_wire {
							ret_in.push(rst_wire);
						}
					}
					(ret_in, ret_out)
				} else {
					let mut wr_ports = vec![];
					for i in 0..n_wr_ports {
						let addr_idx = index_of(&mapped_in_port, &format!("WR_ADDR_{i}")).unwrap();
						let data_idx = index_of(&mapped_in_port, &format!("WR_DATA_{i}")).unwrap();
						let clk_idx = index_of(&mapped_in_port, &format!("WR_CLK_{i}")).unwrap();
						let en_idx = index_of(&mapped_in_port, &format!("WR_EN_{i}"));
						let mut idx_offset = 1;
						assert!(addr_idx + idx_offset == data_idx);
						idx_offset += 1;
						assert!(addr_idx + idx_offset == clk_idx);
						idx_offset += 1;
						if en_idx.is_some() {
							assert!(addr_idx + idx_offset == en_idx.unwrap());
						}
						wr_ports.push(MemoryWritePort {
							addr: sig_in[addr_idx],
							data: sig_in[data_idx],
							clk: sig_in[clk_idx],
							en: en_idx.map(|idx| sig_in[idx]),
							clk_polarity: wr_clk_polarity[i],
						});
					}
					let (rd_ports, wr_ports) = logical_design.add_ram(
						rd_ports,
						wr_ports,
						cell.attributes["SIZE"].unwrap_bin_str() as u32,
					);
					let mut ret_in = vec![];
					let mut ret_out = vec![];
					for p in rd_ports {
						ret_in.push(p.addr_wire);
						ret_out.push(p.data);
						if let Some(clk_wire) = p.clk_wire {
							ret_in.push(clk_wire);
						}
						if let Some(en_wire) = p.en_wire {
							ret_in.push(en_wire);
						}
						if let Some(rst_wire) = p.rst_wire {
							ret_in.push(rst_wire);
						}
					}
					for p in wr_ports {
						ret_in.push(p.addr_wire);
						ret_in.push(p.data_wire);
						if let Some(clk_wire) = p.clk_wire {
							ret_in.push(clk_wire);
						}
						if let Some(en_wire) = p.en_wire {
							ret_in.push(en_wire);
						}
					}
					(ret_in, ret_out)
				}
			},
			ImplementableOp::PMux(full_case, s_width) => {
				let b_start = !full_case as usize;
				let s_start = b_start + s_width;
				let s_folded_expr = if self.nodes[nodeid].folded_expressions == FoldedData::None {
					None
				} else {
					Some(self.nodes[nodeid].folded_expressions.unwrap_vec())
				};
				let (a, b, s, y) = logical_design.add_pmux(
					if full_case { None } else { Some(sig_in[0]) },
					&sig_in[b_start..s_start],
					&sig_in[s_start..],
					sig_out[0],
					s_folded_expr,
				);
				(chain!(a, b, s).collect_vec(), vec![y])
			},
			ImplementableOp::ReduceAnd => {
				let (input_wires, output_comb) = logical_design.add_reduce_and(&sig_in, sig_out[0]);
				(input_wires, vec![output_comb])
			},
			ImplementableOp::ReduceOr => {
				let (input_wires, output_comb) = logical_design.add_reduce_or(&sig_in, sig_out[0]);
				(input_wires, vec![output_comb])
			},
			ImplementableOp::Srl => {
				let (input_wires, output_comb) = logical_design.add_srl(&sig_in, sig_out[0]);
				(input_wires, vec![output_comb])
			},
			ImplementableOp::Mux => {
				let s_folded_expr =
					if let FoldedData::Single(s) = &self.nodes[nodeid].folded_expressions {
						Some(s)
					} else {
						None
					};
				if let [a, b, s] = sig_in[0..3] {
					let y = sig_out[0];
					let (a, b, s, y) = logical_design.add_mux(a, b, s, y, s_folded_expr);
					(vec![a, b, s], vec![y])
				} else {
					panic!("Got wrong number of args for mux.")
				}
			},
			ImplementableOp::DFF => {
				let (input_wire, clk_wire, output_comb) =
					logical_design.add_dff(sig_in[0], sig_in[1], sig_out[0]);
				(vec![input_wire, clk_wire], vec![output_comb])
			},
			ImplementableOp::SDFF => {
				let (wire_data, wire_clk, wire_srst, _wire_en, comb_q) = logical_design.add_sdffe(
					sig_in[0],
					sig_in[1],
					sig_in[2],
					Signal::Constant(1),
					sig_out[0],
				);
				(vec![wire_data, wire_clk, wire_srst], vec![comb_q])
			},
			ImplementableOp::SDFFE => {
				let (wire_data, wire_clk, wire_srst, wire_en, comb_q) = logical_design
					.add_sdffe(sig_in[0], sig_in[1], sig_in[2], sig_in[3], sig_out[0]);
				(vec![wire_data, wire_clk, wire_srst, wire_en], vec![comb_q])
			},
			ImplementableOp::ADFFE => {
				let reset_value = mapped_cell.unwrap().parameters["ARST_VALUE"]
					.from_bin_str()
					.unwrap() as i32;
				let arst_polarity = mapped_cell.unwrap().parameters["ARST_POLARITY"]
					.from_bin_str()
					.unwrap() as i32;
				let clk_polarity = mapped_cell.unwrap().parameters["CLK_POLARITY"]
					.from_bin_str()
					.unwrap() as i32;
				let en_polarity = mapped_cell.unwrap().parameters["EN_POLARITY"]
					.from_bin_str()
					.unwrap() as i32;
				let (wire_data, wire_clk, wire_en, wire_arst, comb_q) = logical_design.add_adffe(
					sig_in[0],
					sig_in[1],
					sig_in[2],
					sig_in[3],
					sig_out[0],
					reset_value,
					en_polarity == 0,
					clk_polarity == 0,
					arst_polarity == 0,
				);
				(vec![wire_data, wire_clk, wire_en, wire_arst], vec![comb_q])
			},
			ImplementableOp::ADFF => {
				let reset_value = mapped_cell.unwrap().parameters["ARST_VALUE"]
					.from_bin_str()
					.unwrap() as i32;
				let arst_polarity = mapped_cell.unwrap().parameters["ARST_POLARITY"]
					.from_bin_str()
					.unwrap() as i32;
				let clk_polarity = mapped_cell.unwrap().parameters["CLK_POLARITY"]
					.from_bin_str()
					.unwrap() as i32;
				let (dw, cw, aw, qc, _lb) = logical_design.add_adff_isolated(
					sig_in[0],
					sig_in[1],
					sig_in[2],
					sig_out[0],
					reset_value,
					clk_polarity == 0,
					arst_polarity == 0,
				);
				(vec![dw, cw, aw], vec![qc])
			},
			ImplementableOp::DFFE => {
				let (w1, w2, w3, c_q) =
					logical_design.add_dffe(sig_in[0], sig_in[1], sig_in[2], sig_out[0]);
				(vec![w1, w2, w3], vec![c_q])
			},
			ImplementableOp::Sop(width) => {
				assert_eq!(width, sig_in.len());
				let table = mapped_cell.unwrap().parameters["TABLE"]
					.into_bool_vec()
					.unwrap()
					.into_iter()
					.rev() // blow my brains out
					.collect_vec();
				let (wires, sop_comb) = logical_design.add_sop(
					sig_in,
					sig_out[0],
					table,
					mapped_cell.unwrap().parameters["DEPTH"].unwrap_bin_str(),
					&self.nodes[nodeid].folded_expressions,
				);
				(wires, vec![sop_comb])
			},
			ImplementableOp::SopNot(width) => {
				assert_eq!(width, sig_in.len());
				let table = mapped_cell.unwrap().parameters["TABLE"]
					.into_bool_vec()
					.unwrap()
					.into_iter()
					.rev() // blow my brains out
					.collect_vec();
				let (wires, sop_comb) = logical_design.add_sop_not(
					sig_in,
					sig_out[0],
					sig_out[1],
					table,
					mapped_cell.unwrap().parameters["DEPTH"].unwrap_bin_str(),
					&self.nodes[nodeid].folded_expressions,
				);
				(wires, vec![sop_comb, sop_comb])
			},
			ImplementableOp::V2FProgRam => {
				let cell = mapped_cell.unwrap();
				let n_rd_ports = cell.parameters["RD_PORTS"].unwrap_bin_str();
				let rd_clk_polarity = cell.parameters["RD_CLK_POLARITY"]
					.chars()
					.map(|c| match c {
						'0' => Polarity::Negative,
						_ => Polarity::Positive,
					})
					.collect_vec();

				let mut rd_ports = vec![];
				for i in 0..n_rd_ports {
					let addr_idx = index_of(&mapped_in_port, &format!("RD_ADDR_{i}")).unwrap();
					let data_idx = index_of(&mapped_out_port, &format!("RD_DATA_{i}")).unwrap();
					let clk_idx = index_of(&mapped_in_port, &format!("RD_CLK_{i}"));
					let en_idx = index_of(&mapped_in_port, &format!("RD_EN_{i}"));
					let arst_idx = index_of(&mapped_in_port, &format!("RD_ARST_{i}"));
					let srst_idx = index_of(&mapped_in_port, &format!("RD_SRST_{i}"));
					let rst_spec = if let Some(idx) = arst_idx {
						ResetSpec::Async(sig_in[idx])
					} else if let Some(idx) = srst_idx {
						ResetSpec::Sync(sig_in[idx])
					} else {
						ResetSpec::Disabled
					};
					let mut idx_offset = 1;
					if clk_idx.is_some() {
						assert!(addr_idx + idx_offset == clk_idx.unwrap());
						idx_offset += 1;
					}
					if en_idx.is_some() {
						assert!(addr_idx + idx_offset == en_idx.unwrap());
						idx_offset += 1;
					}
					if arst_idx.is_some() {
						assert!(addr_idx + idx_offset == arst_idx.unwrap());
					} else if srst_idx.is_some() {
						assert!(addr_idx + idx_offset == srst_idx.unwrap());
					}
					rd_ports.push(MemoryReadPort {
						addr: sig_in[addr_idx],
						data: sig_out[data_idx],
						clk: clk_idx.map(|idx| sig_in[idx]),
						en: en_idx.map(|idx| sig_in[idx]),
						rst: rst_spec,
						transparent: false,
						clk_polarity: rd_clk_polarity[i],
					});
				}

				{
					let wr_port = {
						let addr_idx = index_of(&mapped_in_port, "WR_ADDR").unwrap();
						let data_idx = index_of(&mapped_in_port, "WR_DATA").unwrap();
						let clk_idx = index_of(&mapped_in_port, "WR_CLK").unwrap();
						let en_idx = index_of(&mapped_in_port, "WR_EN");
						let mut idx_offset = 1;
						assert!(addr_idx + idx_offset == data_idx);
						idx_offset += 1;
						assert!(addr_idx + idx_offset == clk_idx);
						idx_offset += 1;
						if en_idx.is_some() {
							assert!(addr_idx + idx_offset == en_idx.unwrap());
						}
						MemoryWritePort {
							addr: sig_in[addr_idx],
							data: sig_in[data_idx],
							clk: sig_in[clk_idx],
							en: en_idx.map(|idx| sig_in[idx]),
							clk_polarity: Polarity::Positive,
						}
					};
					let arst_idx = index_of(&mapped_in_port, "ARST").unwrap();
					let byte_select_idx = index_of(&mapped_in_port, "BYTE_SELECT").unwrap();
					let program_file = cell.parameters["PROGRAM_FILE"].to_string();
					let mut program = util::load_hex_file(program_file);
					let size = cell.parameters["SIZE"].unwrap_bin_str();
					program.resize(size, 0);
					assert!(size > 0);
					let density = (1 << (64 - size.leading_zeros() - 1)).min(256);
					let (rd_ports, wr_port, arst_wire, byte_select_wire) = logical_design
						.add_programmable_ram(
							sig_in[arst_idx],
							sig_in[byte_select_idx],
							rd_ports,
							wr_port,
							density as usize,
							program,
						);
					let mut ret_in = vec![];
					let mut ret_out = vec![];
					for p in rd_ports {
						ret_in.push(p.addr_wire);
						ret_out.push(p.data);
						if let Some(clk_wire) = p.clk_wire {
							ret_in.push(clk_wire);
						}
						if let Some(en_wire) = p.en_wire {
							ret_in.push(en_wire);
						}
						if let Some(rst_wire) = p.rst_wire {
							ret_in.push(rst_wire);
						}
					}
					{
						let p = wr_port;
						ret_in.push(p.addr_wire);
						ret_in.push(p.data_wire);
						ret_in.push(p.clk_wire.unwrap());
						ret_in.push(p.en_wire.unwrap());
						ret_in.push(arst_wire);
						ret_in.push(byte_select_wire);
					}
					(ret_in, ret_out)
				}
			},
			ImplementableOp::MemoryPrim => {
				let cell = mapped_cell.unwrap();
				let size = cell.attributes["SIZE"].unwrap_bin_str() as u32;
				let mut n_rd_ports = 0;
				for i in 0..32 {
					let is_used_param = format!("PORT_R{i}_USED");
					if cell.parameters.contains_key(&is_used_param)
						&& cell.parameters[&is_used_param].unwrap_bin_str() == 1
					{
						n_rd_ports += 1;
					}
				}
				let mut write_ports = vec![];
				let mut sig_idx = n_rd_ports;
				let clk = sig_in[sig_idx];
				sig_idx += 1;
				for i in 0..32 {
					let is_used_param = format!("PORT_W{i}_USED");
					if cell.parameters.contains_key(&is_used_param)
						&& cell.parameters[&is_used_param].unwrap_bin_str() == 1
					{
						let addr = sig_in[sig_idx];
						sig_idx += 1;
						let data = sig_in[sig_idx];
						sig_idx += 1;
						let en = sig_in[sig_idx];
						sig_idx += 1;
						write_ports.push(MemoryWritePort {
							addr,
							data,
							en: Some(en),
							clk: Signal::None,
							clk_polarity: Polarity::Positive,
						});
					}
				}
				let (read_addr_wires, read_data_combs, clk_wire, write_ports) = logical_design
					.add_v2f_ram(&sig_in[0..n_rd_ports], &sig_out, clk, write_ports, size);
				let mut ret_in = read_addr_wires;
				ret_in.push(clk_wire);
				for p in write_ports {
					ret_in.push(p.addr_wire);
					ret_in.push(p.data_wire);
					ret_in.push(p.en_wire.unwrap());
				}

				let ret_out = read_data_combs;
				(ret_in, ret_out)
			},
			ImplementableOp::Rom => {
				let width = mapped_cell.unwrap().parameters["WIDTH"].unwrap_bin_str();
				let rom_values = mapped_cell.unwrap().parameters["INIT"]
					.chars()
					.rev()
					.chunks(width)
					.into_iter()
					.map(|rom_value| {
						rom_value
							.collect_vec()
							.iter()
							.rev()
							.collect::<String>()
							.unwrap_bin_str() as i32
					})
					.collect_vec();
				let (wire, comb) = logical_design.add_v2f_rom(sig_in[0], sig_out[0], rom_values);
				(vec![wire], vec![comb])
			},
			ImplementableOp::AndBitwise
			| ImplementableOp::OrBitwise
			| ImplementableOp::XorBitwise
			| ImplementableOp::Sshr
			| ImplementableOp::Mul
			| ImplementableOp::Div
			| ImplementableOp::Mod
			| ImplementableOp::Pow
			| ImplementableOp::Add
			| ImplementableOp::Sub
			| ImplementableOp::LessThan
			| ImplementableOp::GreaterThan
			| ImplementableOp::Equal
			| ImplementableOp::NotEqual
			| ImplementableOp::GreaterThanEqual
			| ImplementableOp::LessThanEqual
			| ImplementableOp::Neg
			| ImplementableOp::Not
			| ImplementableOp::V2FRollingAccumulate => panic!("This is not a multi-part op."),
		};
		(input_wires, outputs)
	}

	fn get_fanin_body_subgraphs(&self, nodeid: NodeId) -> (Vec<NodeId>, Vec<NodeId>, Vec<NodeId>) {
		let node = &self.nodes[nodeid];
		let mut body = Vec::with_capacity(node.fanin.len());
		let mut output = Vec::with_capacity(node.fanin.len());
		let mut input = Vec::with_capacity(node.fanin.len());
		for cell_input_id in &node.fanin {
			let cell_input = &self.nodes[*cell_input_id];
			let x_output_id = cell_input.fanin.first().copied().unwrap_or(NodeId::MAX);
			let x_body_id = if x_output_id != NodeId::MAX {
				let x_output = &self.nodes[x_output_id];
				x_output.fanin.first().copied().unwrap_or(NodeId::MAX)
			} else {
				NodeId::MAX
			};

			body.push(x_body_id);
			output.push(x_output_id);
			input.push(cell_input.id);
		}
		(body, output, input)
	}

	fn try_prune(&mut self, id: NodeId) -> usize {
		match self.nodes[id].node_type {
			NodeType::CellInput { .. } => {
				return self.try_prune(self.nodes[id].fanout[0]);
			},
			NodeType::CellOutput { .. } => {
				return self.try_prune(self.nodes[id].fanin[0]);
			},
			NodeType::CellBody { .. } => {},
			_ => return 0,
		}
		for foid in &self.nodes[id].fanout {
			if !self.nodes[*foid].fanout.is_empty() {
				return 0;
			}
		}
		for fiid in &self.nodes[id].fanin {
			if !self.nodes[*fiid].fanin.is_empty() {
				return 0;
			}
		}
		let mut n_pruned = 0;
		for idx in 0..self.nodes[id].fanout.len() {
			let foid = self.nodes[id].fanout[idx];
			self.nodes[foid].node_type = NodeType::Pruned;
			n_pruned += 1;
		}
		for idx in 0..self.nodes[id].fanin.len() {
			let fiid = self.nodes[id].fanin[idx];
			self.nodes[fiid].node_type = NodeType::Pruned;
			n_pruned += 1;
		}
		self.nodes[id].node_type = NodeType::Pruned;
		n_pruned += 1;
		n_pruned
	}

	fn disconnect_and_try_prune(&mut self, id: NodeId) -> usize {
		match self.nodes[id].node_type {
			NodeType::CellInput { .. } => {
				return self.disconnect_and_try_prune(self.nodes[id].fanout[0]);
			},
			NodeType::CellOutput { .. } => {
				return self.disconnect_and_try_prune(self.nodes[id].fanin[0]);
			},
			NodeType::CellBody { .. } => {},
			_ => return 0,
		}
		for idx in 0..self.nodes[id].fanout.len() {
			let foid = self.nodes[id].fanout[idx];
			loop {
				if let Some(x_input) = self.nodes[foid].fanout.last() {
					self.disconnect(foid, *x_input);
				} else {
					break;
				}
			}
		}
		for idx in 0..self.nodes[id].fanin.len() {
			let fiid = self.nodes[id].fanin[idx];
			loop {
				if let Some(x_output) = self.nodes[fiid].fanin.last() {
					self.disconnect(*x_output, fiid);
				} else {
					break;
				}
			}
		}
		self.try_prune(id)
	}

	/// Return the number of CellOutput nodes attached to the CellInput nodes on this CellBody
	fn count_connected_terminals(&self, id: NodeId) -> usize {
		assert!(matches!(self.node_type(id), NodeType::CellBody { .. }));
		self.nodes[id]
			.fanin
			.iter()
			.flat_map(|fiid| self.nodes[*fiid].fanin.iter())
			.count()
	}

	/** see [`count_connected_terminals`] */
	fn get_sigular_output_input_pair(&self, body_id: NodeId) -> (usize, usize) {
		assert_eq!(self.count_connected_terminals(body_id), 1);
		assert!(matches!(self.node_type(body_id), NodeType::CellBody { .. }));
		let body_node = &self.nodes[body_id];
		for cell_input_id in &body_node.fanin {
			let cell_input = &self.nodes[*cell_input_id];
			for cell_output_id in &cell_input.fanin {
				return (*cell_output_id, *cell_input_id);
			}
		}
		unreachable!("You fucked it, didnt call count_connected_terminals");
	}

	fn optimize_graph(&mut self, mapped_design: &MappedDesign) -> usize {
		let mut n_pruned = 0;
		use checked_design_optimizations as cdo;
		n_pruned += cdo::ConstantFold::apply(self, mapped_design);
		n_pruned += cdo::DeciderFold::apply(self, mapped_design);
		n_pruned += cdo::SopNot::apply(self, mapped_design);
		n_pruned += cdo::MuxToPmux::apply(self, mapped_design);
		n_pruned += cdo::MuxDuplication::apply(self, mapped_design);
		n_pruned += cdo::PMuxFold::apply(self, mapped_design);
		n_pruned
	}

	pub fn save_dot<S: AsRef<str>>(
		&self,
		mapped_design: &MappedDesign,
		filter: Option<&HashS<NodeId>>,
		filename: S,
	) {
		graph_viz::save_dot(self, mapped_design, filter, filename.as_ref()).unwrap()
	}

	fn identify_clocks(&mut self, mapped_design: &MappedDesign) {
		for id in 0..self.nodes.len() {
			let node = &self.nodes[id];
			match node.node_type {
				NodeType::PortOutput { .. } => {
					for sink in &node.fanout {
						let sink = &self.nodes[*sink];
						if let NodeType::CellInput { port, .. } = &sink.node_type {
							if let Some(cell) = mapped_design.get_cell_option(&sink.mapped_id) {
								let clk_pins = cell.get_clocks();
								if clk_pins.contains(port) {
									self.clocks.insert(node.id);
									self.clocks.insert(node.fanin[0]);
								}
							}
						}
					}
				},
				_ => {},
			}
		}
	}

	pub fn nodes_n_hops_back(&self, id: NodeId, max_hops: usize) -> HashS<NodeId> {
		let mut queue = LinkedList::new();
		let mut visited = hash_set();

		queue.push_back((id, 0));
		loop {
			let (id, hops) = if let Some((x, y)) = queue.pop_front() {
				(x, y)
			} else {
				break;
			};
			if visited.contains(&id) {
				continue;
			}
			if hops > max_hops {
				continue;
			}
			visited.insert(id);
			for fiid in &self.nodes[id].fanin {
				queue.push_back((*fiid, hops + 1));
			}
		}
		visited
	}

	fn report_timing_save_worst_path(&self, logd: &LogicalDesign, mapped_design: &MappedDesign) {
		self.check_connections();
		let topo = self.get_topo_order(mapped_design);
		let mut arrivals = vec![0; topo.len()];
		let time = &logd.timing_engine;
		let mut logic_map_inverse = vec![vec![]; logd.nodes.len()];
		let logic_map = &self.associated_logic.borrow();
		for id in &topo {
			if let Some(lid) = logic_map[*id] {
				logic_map_inverse[lid.0].push(*id);
			}
		}
		for id in &topo {
			let id = *id;
			let node = &self.nodes[id];
			match &node.node_type {
				NodeType::CellInput { .. } => {
					for fid in &self.nodes[id].fanin {
						arrivals[id] = arrivals[*fid].max(arrivals[id]);
					}
					if let Some(sink) = time.get_sink_delay(logic_map[id].unwrap()) {
						let body = self.nodes[id].fanout[0];
						arrivals[body] = arrivals[body].max(arrivals[id] + sink.0);
					} else {
						if let Some(arcs) = time.get_arcs(logic_map[id].unwrap()) {
							for edge in arcs {
								let (fo_lid, delay) = (edge.0, edge.1);
								for fid in &logic_map_inverse[fo_lid.0] {
									assert!(*fid != id);
									arrivals[*fid] = arrivals[*fid].max(arrivals[id] + delay.0);
								}
							}
						}
					}
				},
				NodeType::CellOutput { .. } => {
					if let Some(source) = time.get_source_delay(logic_map[id].unwrap()) {
						arrivals[id] = source.0;
					}
				},
				NodeType::PortInput { .. } => {
					for fid in &self.nodes[id].fanin {
						arrivals[id] = arrivals[id].max(arrivals[*fid]);
					}
				},
				NodeType::PortOutput { .. } => {
					arrivals[id] = 0;
				},
				NodeType::PortBody { .. } => {
					arrivals[id] = 0;
					for fid in &self.nodes[id].fanin {
						arrivals[id] = arrivals[*fid].max(arrivals[id]);
					}
				},
				NodeType::CellBody { .. } => {},
				NodeType::Pruned => {},
			}
		}
		let max_arrival = *arrivals.iter().max().unwrap();
		let mut requireds = vec![max_arrival; topo.len()];
		for id in topo.iter().rev() {
			let id = *id;
			let node = &self.nodes[id];
			match &node.node_type {
				NodeType::CellInput { .. } => {
					if let Some(sink) = time.get_sink_delay(logic_map[id].unwrap()) {
						let body = self.nodes[id].fanout[0];
						let body_req = requireds[body];
						requireds[id] = body_req - sink.0;
					} else {
						if let Some(arcs) = time.get_arcs(logic_map[id].unwrap()) {
							for edge in arcs {
								let (fo_lid, delay) = (edge.0, edge.1);
								for fid in &logic_map_inverse[fo_lid.0] {
									requireds[id] = requireds[id].min(requireds[*fid] - delay.0);
								}
							}
						}
					}
				},
				NodeType::CellOutput { .. } => {
					for fid in &self.nodes[id].fanout {
						requireds[id] = requireds[id].min(requireds[*fid]);
					}
				},
				NodeType::PortInput { .. } => {
					//requireds[id] = max_arrival - arrivals[id];
					requireds[node.fanout[0]] = requireds[id];
				},
				NodeType::PortOutput { .. } => {
					for fid in &self.nodes[id].fanout {
						requireds[id] = requireds[id].min(requireds[*fid]);
					}
					println!("{}, req {}", node.mapped_id, requireds[id]);
					requireds[node.fanin[0]] = requireds[id];
				},
				NodeType::PortBody { .. } => {},
				NodeType::CellBody { .. } => {},
				NodeType::Pruned => {},
			}
		}
		#[cfg(false)]
		{
			println!("Timings per node");
			for i in 0..self.nodes.len() {
				print!("    {i} : {} {}", arrivals[i], requireds[i]);
				if arrivals[i] > requireds[i] {
					print!(" BAD!");
				}
				println!();
			}
		}
		let slacks = izip!(&arrivals, &requireds)
			.map(|(arr, req)| req - arr)
			.collect_vec();

		let mut slacks_histogram = vec![0; max_arrival as usize + 1];
		for s in &slacks {
			if *s < 0 {
				continue;
			}
			slacks_histogram[*s as usize] += 1;
		}
		let mut arrivals_hisogram = vec![0; max_arrival as usize + 1];
		for a in &arrivals {
			arrivals_hisogram[*a as usize] += 1;
		}

		println!("Max arrival: {}", max_arrival);
		println!("Input port slack:");
		for i in 0..self.nodes.len() {
			let node = &self.nodes[i];
			if let NodeType::PortOutput { .. } = node.node_type {
				println!("    {}: {}", node.mapped_id, slacks[i]);
			}
		}
		println!("Output port arrival:");
		for i in 0..self.nodes.len() {
			let node = &self.nodes[i];
			if let NodeType::PortInput { .. } = node.node_type {
				println!("    {}: {}", node.mapped_id, arrivals[i]);
			}
		}
		println!("Slack histogram");
		for i in 0..slacks_histogram.len() {
			println!("    {}: {}", i, slacks_histogram[i]);
		}
		println!("Arrival histogram");
		for i in 0..arrivals_hisogram.len() {
			println!("    {}: {}", i, arrivals_hisogram[i]);
		}
		println!("Worst paths:");
		for id in &topo {
			let id = *id;
			let node = &self.nodes[id];
			if slacks[id] == 0 {
				if let Some(lid) = logic_map[id] {
					print!("    {}, {}: ", lid.0, id);
				} else {
					print!("    N/A: ");
				}

				print!("arr {}, ", arrivals[id],);

				match &node.node_type {
					NodeType::CellInput { port, .. } | NodeType::CellOutput { port, .. } => {
						print!("Cell {}, port {}", node.mapped_id, port);
					},
					NodeType::PortInput { .. } | NodeType::PortOutput { .. } => {
						print!("Port {}", node.mapped_id);
					},
					NodeType::PortBody { .. } => {},
					NodeType::CellBody { cell_type } => {
						let display_op = match cell_type {
							BodyType::ABY => mapped_design
								.get_cell(&node.mapped_id)
								.cell_type
								.simple_string(),
							BodyType::AY => mapped_design
								.get_cell(&node.mapped_id)
								.cell_type
								.simple_string(),
							BodyType::MultiPart { op } => op.simple_string(),
							BodyType::Constant { .. } => "$constant",
							BodyType::Nop => "$nop",
						};
						print!("CellBody {:?}, {:?}", node.mapped_id, display_op);
					},
					NodeType::Pruned => {},
				}
				println!()
			}
		}
		let mut filter = hash_set();
		for id in &topo {
			if slacks[*id] == 0 {
				filter.insert(*id);
			}
			let node = &self.nodes[*id];
			if node.node_type.is_cell_body() {
				if node.fanin.iter().any(|id| slacks[*id] == 0)
					|| node.fanout.iter().any(|id| slacks[*id] == 0)
				{
					filter.insert(*id);
					for fid in &node.fanin {
						filter.insert(*fid);
					}
					for fid in &node.fanout {
						filter.insert(*fid);
					}
				}
			}
		}
		self.save_dot(mapped_design, Some(&filter), "worst_path.dot");
	}

	fn check_connections(&self) {
		for id in 0..self.nodes.len() {
			let node = &self.nodes[id];
			for fid in &node.fanin {
				let node2 = &self.nodes[*fid];
				assert!(node2.fanout.contains(&id));
			}
			for fid in &node.fanout {
				let node2 = &self.nodes[*fid];
				assert!(node2.fanin.contains(&id));
			}
		}
		for id in 0..self.nodes.len() {
			match self.node_type(id) {
				NodeType::CellBody { .. } => {},
				NodeType::Pruned => {},
				_ => {
					let len = self.nodes[id].fanin.len();
					assert!(len <= 1);
				},
			}
		}
	}

	fn update_folded_data_node(&mut self, id: NodeId) {
		let node = &mut self.nodes[id];
		if !node.node_type.is_cell_body() {
			return;
		}
		match node.node_type.get_cell_type() {
			BodyType::MultiPart { op } => {
				let signals = node.fanin.iter().map(|id| self.signals[*id]).collect_vec();
				match op {
					ImplementableOp::LUT(_)
					| ImplementableOp::Sop(_)
					| ImplementableOp::SopNot(_) => {
						checked_design_optimizations::update_folded_data_isotropic_ports(
							&mut node.folded_expressions,
							signals,
						)
					},
					ImplementableOp::PMux(full_case, width) => {
						checked_design_optimizations::update_folded_data_pmux(
							node, signals, full_case, width,
						)
					},
					ImplementableOp::Mux => {
						checked_design_optimizations::update_folded_data_mux(node, signals);
					},
					_ => {},
				}
			},
			_ => {},
		}
	}

	fn update_folded_data(&mut self) {
		for id in 0..self.nodes.len() {
			self.update_folded_data_node(id);
		}
	}
}

mod graph_viz {
	use crate::checked_design::CheckedDesign;
	use crate::checked_design::NodeType;
	use crate::logical_design::Signal;
	use crate::mapped_design::MappedDesign;
	use crate::util::HashS;
	use graphviz_rust::attributes::EdgeAttributes;
	use graphviz_rust::attributes::NodeAttributes;
	use graphviz_rust::dot_generator::*;
	use graphviz_rust::dot_structures::*;
	use graphviz_rust::printer::DotPrinter;
	use graphviz_rust::printer::PrinterContext;

	pub fn save_dot(
		design: &CheckedDesign,
		mapped: &MappedDesign,
		filter: Option<&HashS<usize>>,
		filepath: &str,
	) -> std::io::Result<()> {
		let mut g = graph!(di id!("CheckedDesign"));
		g.add_stmt(stmt!(attr!("rankdir", "LR")));

		//let filter = {
		//	let (id, _) = design
		//		.nodes
		//		.iter()
		//		.enumerate()
		//		.find(|(_, n)| n.mapped_id == "instr_addr")
		//		.unwrap();
		//	design.nodes_n_hops_back(id, 20)
		//};
		//let filter = Some(&filter);
		let tmp = (0..design.nodes.len()).collect();
		let filter = filter.unwrap_or_else(|| &tmp);

		for node in design.nodes.iter().filter(|n| {
			!matches!(design.node_type(n.id), NodeType::Pruned) && filter.contains(&n.id)
		}) {
			let nid = format!("n{}", node.id);

			let mut node_label = {
				let idx = node.mapped_id.rfind("$").unwrap_or_default();
				format!("\"{}", &node.mapped_id[idx..],)
			};
			if let Some(mapped_cell) = mapped.get_cell_option(&node.mapped_id) {
				node_label += &format!("\n{}", mapped_cell.cell_type);
			}
			if let Some(Some(lid)) = design.associated_logic.borrow().get(node.id) {
				node_label += &format!("\n{}", lid);
			}
			match &node.node_type {
				NodeType::CellInput { port, .. } => node_label += &format!("\n{}", port),
				NodeType::CellOutput { port, .. } => node_label += &format!("\n{}", port),
				_ => {},
			}
			node_label += "\"";

			let shape = match node.node_type {
				NodeType::CellInput { .. } => "larrow",
				NodeType::CellOutput { .. } => "rarrow",
				NodeType::PortInput { .. } => "larrow",
				NodeType::PortOutput { .. } => "rarrow",
				NodeType::PortBody { .. } => "box",
				NodeType::CellBody { .. } => "box",
				NodeType::Pruned => "point",
			};

			let graph_node = node!(nid;
				attr!("shape", shape),
				NodeAttributes::label(node_label)
			);
			g.add_stmt(stmt!(graph_node));
		}

		for src_node in &design.nodes {
			if matches!(design.node_type(src_node.id), NodeType::Pruned)
				|| !filter.contains(&src_node.id)
			{
				continue;
			}
			for dst_id in &src_node.fanout {
				if matches!(design.node_type(*dst_id), NodeType::Pruned) || !filter.contains(dst_id)
				{
					continue;
				}
				let dst_node = &design.nodes[*dst_id];
				match &dst_node.node_type {
					NodeType::CellInput { port, .. }
						if port.to_lowercase().contains("clk")
							|| port.to_lowercase().contains("arst") =>
					{
						continue;
					},
					_ => {},
				}

				let src_id_str = format!("n{}", src_node.id);
				let dst_id_str = format!("n{}", dst_id);

				let mut attrs = Vec::new();

				let signal = design.signals.get(src_node.id);
				if !matches!(signal, None | Some(Signal::None)) {
					attrs.push(EdgeAttributes::label(format!("\"{}\"", signal.unwrap())));
				}

				if design.is_driver(*dst_id, src_node.id) {
					attrs.push(attr!("syle", "bold"));
					attrs.push(attr!("color", "blue"));
					g.add_stmt(stmt!(
						edge!(node_id!(src_id_str) => node_id!(dst_id_str), attrs)
					));
				} else {
					g.add_stmt(stmt!(edge!(node_id!(src_id_str) => node_id!(dst_id_str))));
				};
			}
		}

		let dot = g.print(&mut PrinterContext::default());
		std::fs::write(filepath, dot)
	}
}

use core::panic;
use std::{
	cell::RefCell,
	collections::{BTreeSet, HashSet, LinkedList},
	usize, vec,
};

type NodeId = usize;

use itertools::{chain, izip, Itertools};

use crate::mapped_design::MappedDesign;
use crate::{
	connected_design::{CoarseExpr, ConnectedDesign},
	logical_design::{
		self, ArithmeticOperator, DeciderOperator, LogicalDesign, MemoryReadPort, MemoryWritePort,
		ResetSpec, Signal, NET_RED_GREEN,
	},
	mapped_design::{BitSliceOps, Direction, FromBinStr, IntoBoolVec},
	signal_lookup_table,
	util::{hash_set, index_of},
};

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
	V2FRollingAccumulate,
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

	Swizzle, // Imaginary cell
}

impl ImplementableOp {
	pub(crate) fn get_body_type(&self) -> BodyType {
		match self {
			ImplementableOp::AndBitwise => BodyType::ABY,
			ImplementableOp::OrBitwise => BodyType::ABY,
			ImplementableOp::XorBitwise => BodyType::ABY,
			ImplementableOp::Shl => BodyType::MultiPart,
			ImplementableOp::Sshr => BodyType::ABY,
			ImplementableOp::Srl => BodyType::MultiPart,
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
			ImplementableOp::DFF => BodyType::MultiPart,
			ImplementableOp::Swizzle => BodyType::MultiPart,
			ImplementableOp::LUT(_) => BodyType::MultiPart,
			ImplementableOp::Memory => BodyType::MultiPart,
			ImplementableOp::PMux(_, _) => BodyType::MultiPart,
			ImplementableOp::Mux => BodyType::MultiPart,
			ImplementableOp::ReduceAnd => BodyType::MultiPart,
			ImplementableOp::ReduceOr => BodyType::MultiPart,
			ImplementableOp::Neg => BodyType::AY,
			ImplementableOp::SDFF => BodyType::MultiPart,
			ImplementableOp::SDFFE => BodyType::MultiPart,
			ImplementableOp::ADFFE => BodyType::MultiPart,
			ImplementableOp::ADFF => BodyType::MultiPart,
			ImplementableOp::DFFE => BodyType::MultiPart,
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
	coarse_exprs: Vec<Option<CoarseExpr>>,
	associated_logic: RefCell<Vec<Option<logical_design::NodeId>>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum NodeType {
	CellInput { port: String, connected_id: usize },
	CellOutput { port: String, connected_id: usize },
	PortInput { connected_id: usize },
	PortOutput { connected_id: usize },
	PortBody,
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
			NodeType::PortBody => "PortBody",
			NodeType::CellBody { .. } => "CellBody",
			NodeType::Pruned => "Pruned",
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum BodyType {
	ABY,
	AY,
	MultiPart, // This should be a superset of ABY and AY and Constant
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
	keep: bool,
	constants: Vec<Option<i32>>,
	folded_expressions: Vec<Option<(Signal, DeciderOperator, Signal)>>,
}

impl Node {
	#[allow(dead_code)]
	fn is_port_input(&self) -> bool {
		match self.node_type {
			NodeType::PortInput { .. } => true,
			_ => false,
		}
	}
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
		}
	}

	fn node_type(&self, nodeid: NodeId) -> &NodeType {
		&self.nodes[nodeid].node_type
	}

	fn is_driver(&self, nodeid: NodeId, to_check: NodeId) -> bool {
		self.node_type(to_check).is_output() && self.nodes[nodeid].fanin.contains(&to_check)
	}

	fn new_node(&mut self, mapped_id: &str, node_type: NodeType) -> NodeId {
		let connected_id = match node_type {
			NodeType::CellInput { connected_id, .. } => Some(connected_id),
			NodeType::CellOutput { connected_id, .. } => Some(connected_id),
			NodeType::PortInput { connected_id, .. } => Some(connected_id),
			NodeType::PortOutput { connected_id, .. } => Some(connected_id),
			NodeType::PortBody => None,
			NodeType::CellBody { .. } => None,
			NodeType::Pruned => unreachable!(),
		};
		self.nodes.push(Node {
			id: self.nodes.len(),
			node_type,
			mapped_id: mapped_id.to_owned(),
			fanin: vec![],
			fanout: vec![],
			constants: vec![],
			folded_expressions: vec![],
			keep: false,
		});
		if let Some(connected_id) = connected_id {
			if connected_id != usize::MAX {
				self.connected_id_map[connected_id] = self.nodes.len() - 1;
			}
		}
		self.coarse_exprs.push(None);

		self.nodes.len() - 1
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
			(NodeType::PortInput { .. }, NodeType::PortBody) => {},
			(NodeType::PortOutput { .. }, NodeType::CellInput { .. }) => {},
			(NodeType::PortOutput { .. }, NodeType::PortInput { .. }) => {},
			(NodeType::PortBody, NodeType::PortOutput { .. }) => {},
			(NodeType::CellBody { .. }, NodeType::CellOutput { .. }) => {},
			_ => assert!(false),
		};
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
			let id = self.new_node(name, NodeType::PortBody);
			self.nodes[id].keep = true;
		});
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
				NodeType::PortBody => {
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
				NodeType::CellBody { .. } => {
					let cell = mapped_design.get_cell(&mapped_id);

					for (connected_id, direction, terminal_name) in cell
						.get_terminal_names()
						.iter()
						.map(|terminal_name| {
							(
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
				if driver == sink {
					continue;
				}
			}
			if exprs.len() > 1 {
				// Insert swizzle that takes in the fanin and outputs a single
				let body = self.new_node(
					"$swizzle",
					NodeType::CellBody {
						cell_type: BodyType::MultiPart,
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
					continue;
				}
				let fiid = self.connected_id_map[*driver_ioid];
				assert_eq!(node.fanin.len(), 1);
				assert_eq!(fiid, node.fanin[0]);
				self.disconnect(fiid, nodeid);
				let body = self.new_node(
					"$swizzle",
					NodeType::CellBody {
						cell_type: BodyType::MultiPart,
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
			}
		}
	}

	pub fn enforce_network_requirements(&mut self) {
		let (_, _, _, localio_group_nodes) = self.get_groupings();
		let mut fallback_id = 0;
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
					.unwrap_or_else(|| {
						let mut counter = 0;
						while required_signals.contains(&fallback_id) {
							fallback_id += 1;
							counter += 1;
							fallback_id %= signal_lookup_table::n_ids();
							if counter > signal_lookup_table::n_ids() {
								panic!("Need to add a nop after at least one port.")
							}
						}

						fallback_id
					});
				if !required_signals.contains(&choice) {
					required_signals.insert(choice);
					signal_choices[*nodeid] = choice.try_into().expect("Impl error");
					continue;
				}
				// This port cant use this signal internally, so we must rename it with a nop.
				if matches!(self.node_type(*nodeid), NodeType::PortOutput { .. }) {
					let mut nop = usize::MAX;
					for foid in self.nodes[*nodeid].fanout.clone() {
						self.disconnect(*nodeid, foid);
						if nop == usize::MAX {
							nop = self.new_node(
								"$nop",
								NodeType::CellBody {
									cell_type: BodyType::Nop,
								},
							);
						}
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
						self.connect(*nodeid, a);
						self.connect(a, nop);
						self.connect(nop, y);
						self.connect(y, foid);
					}
				}
				if matches!(self.node_type(*nodeid), NodeType::PortInput { .. }) {
					let mut nop = usize::MAX;
					for fiid in self.nodes[*nodeid].fanin.clone() {
						self.disconnect(fiid, *nodeid);
						if nop == usize::MAX {
							nop = self.new_node(
								"$nop",
								NodeType::CellBody {
									cell_type: BodyType::Nop,
								},
							);
						}
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
						self.connect(fiid, a);
						self.connect(a, nop);
						self.connect(nop, y);
						self.connect(y, *nodeid);
					}
				}
			}
		}
	}

	fn calculate_and_validate_signal_choices(&self) -> Vec<Signal> {
		self.get_signal_choice_final()
	}

	fn elaborate_signal_choices(&self, signal_choices: &mut Vec<Signal>) {
		// Check that we now only have 0 or 1 option for a signal
		let topo_order = self.get_topo_order();
		//topo_order
		//	.iter()
		//	.map(|x| {
		//		(
		//			*x,
		//			self.node_type(*x).clone(),
		//			signal_choices[*x],
		//			self.nodes[*x].fanin.clone(),
		//			self.nodes[*x].fanout.clone(),
		//		)
		//	})
		//	.for_each(|tpl| println!("{:?}", tpl));
		// Final signal resolve
		for nodeid in topo_order {
			if signal_choices[nodeid].is_some() {
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
					if node
						.fanout
						.iter()
						.any(|foid| signal_choices[*foid].is_some())
					{
						signal_choices[nodeid] = signal_choices[node.fanin[0]];
						continue;
					}
				},
				NodeType::PortInput { .. }
				| NodeType::PortOutput { .. }
				| NodeType::PortBody
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
				self.set_signal(
					signal_choices,
					*id,
					sig.try_into()
						.expect("Too many signals on one wire network"),
				);
				sig += 1;
			}
		}
		//println!("\n\n");
		//let topo_order = self.get_topo_order();
		//topo_order
		//	.iter()
		//	.map(|x| {
		//		(
		//			*x,
		//			self.node_type(*x).clone(),
		//			signal_choices[*x],
		//			self.nodes[*x].fanin.clone(),
		//			self.nodes[*x].fanout.clone(),
		//		)
		//	})
		//	.for_each(|tpl| println!("{:?}", tpl));
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
				NodeType::PortBody => {
					assert!(signal_choices[node.id].is_some());
					assert!(matches!(signal_choices[node.id], Signal::Id(_)));
					for foid in &node.fanout {
						assert_eq!(signal_choices[node.id], signal_choices[*foid]);
					}
					for fiid in &node.fanin {
						assert_eq!(signal_choices[node.id], signal_choices[*fiid]);
					}
				},
				NodeType::CellBody { .. } => assert!(signal_choices[node.id].is_none()),
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
		self.connected_design.build_from(mapped_design);
		self.connected_id_map = vec![usize::MAX; self.connected_design.max_id() + 1];
		self.initialize_nodes(mapped_design);
		self.make_preliminary_connections();
		self.resolve_coarse_exprs();
		self.enforce_network_requirements();
		//self.insert_nop_to_sanitize_ports();
		//#[cfg(false)]
		loop {
			let n_pruned = self.optimize_graph(mapped_design);
			if n_pruned > 0 {
				println!("Pruned {n_pruned} nodes while optimizing.");
			} else {
				break;
			}
		}
		let mut signal_choices = self.calculate_and_validate_signal_choices();
		self.elaborate_signal_choices(&mut signal_choices);
		self.signals_correctness_check(&signal_choices);
		self.signals = signal_choices;
	}

	fn set_signal(&self, signals: &mut Vec<Signal>, nodeid: NodeId, signal: Signal) {
		let node = &self.nodes[nodeid];
		if signals[nodeid].is_some() {
			return;
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
					self.set_signal(signals, driver, signal);
				}
			},
			NodeType::CellOutput { .. } | NodeType::PortOutput { .. } => {
				signals[nodeid] = signal;
				for foid in &node.fanout {
					signals[*foid] = signal;
				}
			},
			NodeType::PortBody => {},
			NodeType::CellBody { .. } => {},
			NodeType::Pruned => {},
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
			NodeType::CellBody { .. } | NodeType::PortBody => {
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
				NodeType::PortBody | NodeType::CellBody { .. } => {
					panic!("Implementer is a fucking moron.")
				},
				NodeType::Pruned => {},
			}
		}
		retval
	}

	fn get_attached_nodes(&self, nodeid: NodeId) -> Vec<NodeId> {
		match self.node_type(nodeid) {
			NodeType::CellBody { .. } | NodeType::PortBody => {
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
				NodeType::CellBody { .. } | NodeType::PortBody => {
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

	#[allow(dead_code)]
	fn get_signal_choices(&self) -> Vec<Vec<i32>> {
		let mut signal_choices = vec![vec![]; self.nodes.len()];
		let mut fallback_id = 0;
		for node in &self.nodes {
			if node.node_type == NodeType::PortBody {
				let choice = signal_lookup_table::lookup_id(&node.mapped_id).unwrap_or_else(|| {
					let ret = fallback_id;
					fallback_id += 1;
					fallback_id %= signal_lookup_table::n_ids();
					ret
				});
				signal_choices[node.id] = vec![choice];
				if let Some(port_in_id) = node.fanin.first() {
					signal_choices[*port_in_id] = vec![choice];
				}
				if let Some(port_out_id) = node.fanout.first() {
					signal_choices[*port_out_id] = vec![choice];
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

	fn get_signal_choice_final(&self) -> Vec<Signal> {
		let mut signal_choices = vec![Signal::None; self.nodes.len()];
		let mut fallback_id = 0;
		for node in &self.nodes {
			if node.node_type == NodeType::PortBody {
				let choice = signal_lookup_table::lookup_id(&node.mapped_id).unwrap_or_else(|| {
					let ret = fallback_id;
					fallback_id += 1;
					fallback_id %= signal_lookup_table::n_ids();
					ret
				});
				signal_choices[node.id] = choice.try_into().expect("Impl error");
				if let Some(fiid) = node.fanin.first() {
					signal_choices[*fiid] = Signal::Id(choice);
				}
				if let Some(foid) = node.fanout.first() {
					signal_choices[*foid] = Signal::Id(choice);
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
		topological_order
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
				.unwrap_or(-1)
				+ 1
		}
		depth
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
				NodeType::PortBody => {
					if !node.fanout.is_empty() {
						let new_id =
							logical_design.add_constant(vec![self.signals[nodeid]], vec![0]);
						logic_map[nodeid] = Some(new_id);
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
						let (input, output) = self.apply_binary_op(
							logical_design,
							mapped_design.get_cell(&node.mapped_id).cell_type,
							sig_left,
							sig_right,
							sig_out,
						);
						logic_map[nodeid] = Some(input);
						logic_map[node.fanout[0]] = Some(output)
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
						logic_map[nodeid] = Some(logical_design.add_nop(sig_in, sig_out))
					},
					BodyType::AY => {
						let sig_in = self.signals[node.fanin[0]];
						let sig_out = self.signals[node.fanout[0]];
						let (input, output) = self.apply_unary_op(
							logical_design,
							mapped_design.get_cell(&node.mapped_id).cell_type,
							sig_in,
							sig_out,
						);
						logic_map[nodeid] = Some(input);
						logic_map[node.fanout[0]] = Some(output)
					},
					BodyType::MultiPart => {
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
							mapped_design
								.get_cell_option(&node.mapped_id)
								.map(|cell| cell.cell_type)
								.unwrap_or(ImplementableOp::Swizzle),
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
				NodeType::PortOutput { .. } => logical_design
					.append_description(logic_map[nodeid].unwrap(), node.mapped_id.as_str()),
				NodeType::PortBody => {},
				NodeType::CellBody { .. } => {},
			}
		}
		self.associated_logic.replace(logic_map);
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
				);
				(input_wires, vec![output_comb])
			},
			ImplementableOp::Memory => {
				let cell = mapped_cell.unwrap();
				let n_rd_ports = cell.parameters["RD_PORTS"].unwrap_bin_str();
				let n_wr_ports = cell.parameters["WR_PORTS"].unwrap_bin_str();

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
					for i in 0..n_rd_ports {
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
				let abs_folded_expr = if self.nodes[nodeid].folded_expressions.is_empty() {
					None
				} else {
					let exprs = &self.nodes[nodeid].folded_expressions;
					Some((
						&exprs[0..b_start],
						&exprs[b_start..s_start],
						&exprs[s_start..],
					))
				};
				let (a, b, s, y) = logical_design.add_pmux(
					if full_case { None } else { Some(sig_in[0]) },
					&sig_in[b_start..s_start],
					&sig_in[s_start..],
					sig_out[0],
					abs_folded_expr,
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
				let abs_folded_expr = if self.nodes[nodeid].folded_expressions.is_empty() {
					None
				} else {
					let exprs = &self.nodes[nodeid].folded_expressions;
					Some((&exprs[0], &exprs[1], &exprs[2]))
				};
				if let [a, b, s] = sig_in[0..3] {
					let y = sig_out[0];
					let (a, b, s, y) = logical_design.add_mux(a, b, s, y, abs_folded_expr);
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
				let (wire_data, wire_clk, wire_en, wire_arst, comb_q) = logical_design
					.add_adffe(sig_in[0], sig_in[1], sig_in[2], sig_in[3], sig_out[0]);
				(vec![wire_data, wire_clk, wire_en, wire_arst], vec![comb_q])
			},
			ImplementableOp::ADFF => {
				let (w1, w2, _w3, w4, c_q) = logical_design.add_adffe(
					sig_in[0],
					sig_in[1],
					Signal::Constant(1),
					sig_in[2],
					sig_out[0],
				);
				(vec![w1, w2, w4], vec![c_q])
			},
			ImplementableOp::DFFE => {
				let (w1, w2, w3, c_q) =
					logical_design.add_dffe(sig_in[0], sig_in[1], sig_in[2], sig_out[0]);
				(vec![w1, w2, w3], vec![c_q])
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
			| ImplementableOp::V2FRollingAccumulate => panic!("This is not a multi-part op."),
		};
		(input_wires, outputs)
	}

	fn apply_unary_op(
		&self,
		logical_design: &mut LogicalDesign,
		op: ImplementableOp,
		sig_in: Signal,
		sig_out: Signal,
	) -> (logical_design::NodeId, logical_design::NodeId) {
		match op {
			ImplementableOp::V2FRollingAccumulate => {
				let acc = logical_design.add_arithmetic(
					(sig_in, ArithmeticOperator::Add, Signal::Constant(0)),
					sig_in,
				);
				let pre_filter = logical_design.add_arithmetic(
					(sig_in, ArithmeticOperator::Mult, Signal::Constant(1)),
					sig_in,
				);
				let post_filter = logical_design.add_arithmetic(
					(sig_in, ArithmeticOperator::Mult, Signal::Constant(1)),
					sig_out,
				);
				logical_design.add_wire_red(vec![acc, pre_filter], vec![acc, post_filter]);
				(pre_filter, post_filter)
			},
			ImplementableOp::Neg => {
				let neg = logical_design.add_arithmetic(
					(Signal::Constant(0), ArithmeticOperator::Sub, sig_in),
					sig_out,
				);
				(neg, neg)
			},
			_ => {
				unreachable!()
			},
		}
	}

	fn apply_binary_op(
		&self,
		logical_design: &mut LogicalDesign,
		op: ImplementableOp,
		sig_left: Signal,
		sig_right: Signal,
		sig_out: Signal,
	) -> (logical_design::NodeId, logical_design::NodeId) {
		if let Some(op) = op.get_arithmetic_op() {
			let ld_node = logical_design.add_arithmetic((sig_left, op, sig_right), sig_out);
			return (ld_node, ld_node);
		}
		if let Some(op) = op.get_decider_op() {
			let id = logical_design.add_decider();
			logical_design.add_decider_input(
				id,
				(sig_left, op, sig_right),
				logical_design::DeciderRowConjDisj::FirstRow,
				(true, true),
				(true, true),
			);
			logical_design.add_decider_out_constant(id, sig_out, 1, NET_RED_GREEN);
			return (id, id);
		}
		unreachable!()
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
		if self.nodes[id].keep {
			return 0;
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
	fn get_sigular_output_input_pair(&self, id: NodeId) -> (usize, usize) {
		assert_eq!(self.count_connected_terminals(id), 1);
		assert!(matches!(self.node_type(id), NodeType::CellBody { .. }));
		let cell_input = self.nodes[id].fanin[0];
		let cell_output = self.nodes[cell_input].fanin[0];
		(cell_output, cell_input)
	}

	/** I wish there was a better way to implement optimizations, instead of having an ad-hoc approach for each
	 optimization there should a DB of sorts. An optimization class can be created with hooks into each needed pass.
	 Thats too much work right now, and really how many more optimizations will I need? Some can be applied in the
	 verilog techmaps. Will have to revisit this if I need to scale this.
	*/
	fn optimize_graph(&mut self, mapped_design: &MappedDesign) -> usize {
		let mut n_pruned = 0;
		// Merge constants into cells that can support it.
		for id in 0..self.nodes.len() {
			let node = &self.nodes[id];
			if !node.constants.is_empty() {
				continue;
			}
			if let NodeType::CellBody { .. } = &node.node_type {
			} else {
				continue;
			};
			let mapped = if let Some(m) = mapped_design.get_cell_option(&node.mapped_id) {
				m
			} else {
				continue;
			};
			let optimization_applies = mapped.cell_type.get_decider_op().is_some()
				|| mapped.cell_type.get_arithmetic_op().is_some()
				|| matches!(mapped.cell_type, ImplementableOp::PMux(_, _));
			if !optimization_applies {
				continue;
			}
			let (body, output, input) = self.get_fanin_body_subgraphs(id);
			// The actual constant merging.
			self.nodes[id].constants = vec![None; body.len()];
			for (idx, body_id) in body.iter().enumerate() {
				let fanin_body = &self.nodes[*body_id];
				if !matches!(fanin_body.node_type, NodeType::CellBody { .. }) {
					continue;
				}
				if let BodyType::Constant { value } = fanin_body.node_type.get_cell_type() {
					self.nodes[id].constants[idx] = Some(value);
					self.disconnect(output[idx], input[idx]);
				}
			}
			// If the input constant has no other fanout, then prune it.
			for idx in 0..body.len() {
				n_pruned += self.try_prune(body[idx]);
			}
		}
		// Merge decider operations into cells that support expression folding.
		for id in 0..self.nodes.len() {
			let node = &self.nodes[id];
			if !node.folded_expressions.is_empty() {
				continue;
			}
			if let NodeType::CellBody { .. } = &node.node_type {
			} else {
				continue;
			};
			let mapped = if let Some(m) = mapped_design.get_cell_option(&node.mapped_id) {
				m
			} else {
				continue;
			};
			let optimization_applies = matches!(
				mapped.cell_type,
				ImplementableOp::PMux(_, _) | ImplementableOp::LUT(_)
			);
			if !optimization_applies {
				continue;
			}
			let (body, output, input) = self.get_fanin_body_subgraphs(id);
			self.nodes[id].folded_expressions = vec![None; body.len()];
			for (idx, body_id) in body.iter().enumerate() {
				if *body_id == NodeId::MAX {
					continue;
				}
				let fanin_body = &self.nodes[*body_id];
				if !matches!(fanin_body.node_type, NodeType::CellBody { .. }) {
					continue;
				}
				let fanin_body_mapped =
					if let Some(m) = mapped_design.get_cell_option(&fanin_body.mapped_id) {
						m
					} else {
						continue;
					};
				// Finally, retain the body node for future reference when building the next
				if fanin_body_mapped.cell_type.get_decider_op().is_some()
					&& self.count_connected_terminals(*body_id) == 1
				{
					assert!(
						!fanin_body.constants.is_empty(),
						"Decider needs at least 1 constant to be folded in."
					);
					// Here I precompute what the expression for the folded expression should be. In the PMux/LUT
					// implementation I'll check this first and use it instead of s[i] == 1.
					let expr = (
						fanin_body.constants[0] // Recall, terminals are order sensitive, so 0 is left.
							.map(Signal::Constant)
							.unwrap_or(Signal::None),
						fanin_body_mapped.cell_type.get_decider_op().unwrap(),
						fanin_body.constants[1]
							.map(Signal::Constant)
							.unwrap_or(Signal::None),
					);
					assert_ne!(expr.0, expr.2);
					self.nodes[id].folded_expressions[idx] = Some(expr);
					// We need to wire around the expression being folded in.
					//                        1                 2              3                 4                 5
					// third_party_output (O) -> body_input (I) -> body_id (B) -> output[idx] (O) -> input[idx] (I) -> id
					//        |                   ^ the cell being pruned                               ^
					//        +-------------------------------------------------------------------------+
					//        6 This wire is being added by the connect call.
					let (third_party_output, body_input) =
						self.get_sigular_output_input_pair(*body_id);
					self.disconnect(output[idx], input[idx]); // 4
					self.connect(third_party_output, input[idx]); // 6
					if self.nodes[output[idx]].fanout.is_empty() {
						// Can only remove this iff the decider being wired around is no longer needed by anything.
						self.disconnect(third_party_output, body_input); // 1
					}
				}
			}
			// If the input constant has no other fanout, then prune it.
			for idx in 0..body.len() {
				if body[idx] == NodeId::MAX {
					continue;
				}
				n_pruned += self.try_prune(body[idx]);
			}
		}
		n_pruned
	}

	pub fn save_dot(&self, mapped_design: &MappedDesign) {
		graph_viz::save_dot(self, mapped_design, "checked_design.dot").unwrap()
	}
}

mod graph_viz {
	use crate::checked_design::CheckedDesign;
	use crate::checked_design::NodeType;
	use crate::logical_design::Signal;
	use crate::mapped_design::MappedDesign;
	use graphviz_rust::attributes::EdgeAttributes;
	use graphviz_rust::attributes::NodeAttributes;
	use graphviz_rust::dot_generator::*;
	use graphviz_rust::dot_structures::*;
	use graphviz_rust::printer::DotPrinter;
	use graphviz_rust::printer::PrinterContext;

	pub fn save_dot(
		design: &CheckedDesign,
		mapped: &MappedDesign,
		filepath: &str,
	) -> std::io::Result<()> {
		let mut g = graph!(di id!("CheckedDesign"));
		g.add_stmt(stmt!(attr!("rankdir", "LR")));

		for node in design
			.nodes
			.iter()
			.filter(|n| !matches!(design.node_type(n.id), NodeType::Pruned))
		{
			let nid = format!("n{}", node.id);

			let mut node_label = {
				let idx = node.mapped_id.rfind("$").unwrap_or_default();
				format!("\"{}", &node.mapped_id[idx..],)
			};
			if let Some(mapped_cell) = mapped.get_cell_option(&node.mapped_id) {
				node_label += &format!("\n{}", mapped_cell.cell_type);
			}
			if let Some(lid) = design.associated_logic.borrow()[node.id] {
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
				NodeType::PortBody => "box",
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
			if matches!(design.node_type(src_node.id), NodeType::Pruned) {
				continue;
			}
			for dst_id in &src_node.fanout {
				if matches!(design.node_type(*dst_id), NodeType::Pruned) {
					continue;
				}

				let src_id_str = format!("n{}", src_node.id);
				let dst_id_str = format!("n{}", dst_id);

				let mut attrs = Vec::new();

				let signal = design.signals[src_node.id];
				if !matches!(signal, Signal::None) {
					attrs.push(EdgeAttributes::label(format!("\"{}\"", signal)));
				}

				let edge = if design.is_driver(*dst_id, src_node.id) {
					attrs.push(attr!("syle", "bold"));
					attrs.push(attr!("color", "blue"));
					edge!(node_id!(src_id_str) => node_id!(dst_id_str), attrs)
				} else {
					edge!(node_id!(src_id_str) => node_id!(dst_id_str))
				};

				g.add_stmt(stmt!(edge));
			}
		}

		let dot = g.print(&mut PrinterContext::default());
		std::fs::write(filepath, dot)
	}
}

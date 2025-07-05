use std::{
	collections::{BTreeSet, HashSet, LinkedList},
	usize, vec,
};

type NodeId = usize;

use itertools::{chain, Itertools};

use crate::{
	connected_design::{CoarseExpr, ConnectedDesign},
	logical_design::{
		self, ArithmeticOperator, DeciderOperator, LogicalDesign, MemoryReadPort, MemoryWritePort,
		ResetSpec, Signal,
	},
	mapped_design::{BitSliceOps, Direction, FromBinStr, IntoBoolVec},
	signal_lookup_table,
	util::index_of,
};
use crate::{mapped_design::MappedDesign, signal_lookup_table::lookup_id};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ImplementableOp {
	AndBitwise,
	OrBitwise,
	XorBitwise,
	Shl,
	Shr,
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
	V2FRollingAccumulate,
	DFF,
	LUT(usize),
	PMux(bool, usize),
	Memory,

	Swizzle, // Imaginary cell
}

impl ImplementableOp {
	pub(crate) fn get_body_type(&self) -> BodyType {
		match self {
			ImplementableOp::AndBitwise => BodyType::ABY,
			ImplementableOp::OrBitwise => BodyType::ABY,
			ImplementableOp::XorBitwise => BodyType::ABY,
			ImplementableOp::Shl => BodyType::ABY,
			ImplementableOp::Shr => BodyType::ABY,
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
		}
	}

	pub fn get_arithmetic_op(&self) -> Option<ArithmeticOperator> {
		use ImplementableOp as Op;
		match self {
			Op::AndBitwise => Some(ArithmeticOperator::And),
			Op::OrBitwise => Some(ArithmeticOperator::Or),
			Op::XorBitwise => Some(ArithmeticOperator::Xor),
			Op::Shl => Some(ArithmeticOperator::Sll),
			Op::Shr => Some(ArithmeticOperator::Srl),
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
	signals: Vec<Option<i32>>,
	/// This is used to calculate the fanin & fanout of any terminal (port, cell IO) to any other terminal.
	connected_design: ConnectedDesign,
	/// Nodes in connected_design -> nodes in this struct.
	connected_id_map: Vec<NodeId>,
	coarse_exprs: Vec<Option<CoarseExpr>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum NodeType {
	CellInput { port: String, connected_id: usize },
	CellOutput { port: String, connected_id: usize },
	PortInput { connected_id: usize },
	PortOutput { connected_id: usize },
	PortBody,
	CellBody { cell_type: BodyType },
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
}

impl Node {
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
		};
		self.nodes.push(Node {
			id: self.nodes.len(),
			node_type,
			mapped_id: mapped_id.to_owned(),
			fanin: vec![],
			fanout: vec![],
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
			if lookup_id(name).is_some() {
				self.new_node(name, NodeType::PortBody);
			} else {
				panic!(
					"{:?} on top level design doesn't match a named game signal",
					name
				)
			}
		});
		mapped_design.for_all_cells(|_, name, cell| {
			self.new_node(
				name,
				NodeType::CellBody {
					cell_type: cell.cell_type.get_body_type(),
				},
			);
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
						.connections
						.keys()
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
		for x in &self.connected_design.expr {
			println!("{:?}", x);
		}
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
				println!("{:?}", exprs);
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
				if *shift == 0 && *bit_start == 0 && *bit_end == 32 {
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

	pub fn insert_nop_to_sanitize_ports(&mut self) {
		let topo_order = self.get_topo_order();
		// Insert Nop to partition signal networks
		let signal_choices = self.get_signal_choices();
		for nodeid in &topo_order {
			let node = &self.nodes[*nodeid];
			if node.is_port_input() {
				if signal_choices[node.id].len() < 2 {
					continue;
				}
				for fiid in node.fanin.clone() {
					self.disconnect(fiid, *nodeid);
					let a = self.new_node(
						"$nop",
						NodeType::CellInput {
							port: "A".to_string(),
							connected_id: usize::MAX,
						},
					);
					let nop = self.new_node(
						"$nop",
						NodeType::CellBody {
							cell_type: BodyType::Nop,
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
		let topo_order = self.get_topo_order();
		// Recalculate and insert Nop to partition signal networks
		let signal_choices = self.get_signal_choices();
		for nodeid in topo_order.iter().rev() {
			let node = &self.nodes[*nodeid];
			if let NodeType::CellBody { .. } = node.node_type {
				for cell_input in node.fanin.clone() {
					let mut signals = signal_choices[cell_input].clone();
					signals.sort();
					let mut found = false;
					if !signals.is_empty() {
						for i in 0..signals.len() - 1 {
							if signals[i] == signals[i + 1] {
								found = true;
								break;
							}
						}
					}
					if !found {
						continue;
					}

					let node = &self.nodes[cell_input];
					let fiid = node.fanin[0];
					self.disconnect(fiid, cell_input);
					let a = self.new_node(
						"$nop",
						NodeType::CellInput {
							port: "A".to_string(),
							connected_id: usize::MAX,
						},
					);
					let nop = self.new_node(
						"$nop",
						NodeType::CellBody {
							cell_type: BodyType::Nop,
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
					self.connect(y, cell_input);
				}
			}
		}
	}

	fn calculate_and_validate_signal_choices(&self) -> Vec<Option<i32>> {
		self.get_signal_choice_final()
	}

	fn elaborate_signal_choices(&self, signal_choices: &mut Vec<Option<i32>>) {
		// Check that we now only have 0 or 1 option for a signal
		let topo_order = self.get_topo_order();
		topo_order
			.iter()
			.map(|x| {
				(
					*x,
					self.node_type(*x).clone(),
					signal_choices[*x],
					self.nodes[*x].fanin.clone(),
					self.nodes[*x].fanout.clone(),
				)
			})
			.for_each(|tpl| println!("{:?}", tpl));
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
			}
			let local_io = self.get_local_cell_io_network(nodeid);
			let set_io = local_io
				.iter()
				.fold(HashSet::<i32>::new(), |mut set, ioid| {
					if let Some(sig) = signal_choices[*ioid] {
						set.insert(sig);
					};
					set
				});
			let mut sig = 0;
			for id in local_io.iter() {
				while set_io.contains(&sig) {
					sig += 1;
				}
				self.set_signal(signal_choices, *id, sig);
				sig += 1;
			}
		}
	}

	fn signals_correctness_check(&self, signal_choices: &[Option<i32>]) {
		// Final correctness check
		for nodeid in 0..self.nodes.len() {
			println!("{:?}", nodeid);
			let node = &self.nodes[nodeid];
			match node.node_type {
				NodeType::CellInput { .. } | NodeType::PortInput { .. } => {
					assert!(signal_choices[node.id].is_some());
					for fiid in &node.fanin {
						assert_eq!(signal_choices[node.id], signal_choices[*fiid]);
					}
				},
				NodeType::CellOutput { .. } | NodeType::PortOutput { .. } => {
					assert!(signal_choices[node.id].is_some());
					for foid in &node.fanout {
						assert_eq!(signal_choices[node.id], signal_choices[*foid]);
					}
				},
				NodeType::PortBody => {
					assert!(signal_choices[node.id].is_some());
					for foid in &node.fanout {
						assert_eq!(signal_choices[node.id], signal_choices[*foid]);
					}
					for fiid in &node.fanin {
						assert_eq!(signal_choices[node.id], signal_choices[*fiid]);
					}
				},
				NodeType::CellBody { .. } => assert!(signal_choices[node.id].is_none()),
			}
		}
	}

	pub fn build_from(&mut self, mapped_design: &MappedDesign) {
		self.connected_design.build_from(mapped_design);
		self.connected_id_map = vec![usize::MAX; self.connected_design.max_id() + 1];
		self.initialize_nodes(mapped_design);
		self.make_preliminary_connections();
		self.resolve_coarse_exprs();
		self.insert_nop_to_sanitize_ports();
		let mut signal_choices = self.calculate_and_validate_signal_choices();
		self.elaborate_signal_choices(&mut signal_choices);
		self.signals_correctness_check(&signal_choices);
		self.signals = signal_choices;
	}

	fn set_signal(&self, signals: &mut Vec<Option<i32>>, nodeid: NodeId, signal: i32) {
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
				signals[nodeid] = Some(signal);
				for foid in &node.fanout {
					signals[*foid] = Some(signal);
				}
			},
			NodeType::PortBody => {},
			NodeType::CellBody { .. } => {},
		}
	}

	fn get_other_input_nodes(&self, nodeid: NodeId) -> Vec<NodeId> {
		match self.node_type(nodeid) {
			NodeType::CellInput { .. } => assert!(true),
			_ => assert!(false),
		};
		self.nodes[self.nodes[nodeid].fanout[0]]
			.fanin
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
				NodeType::PortInput { .. } => {
					retval.push(curid);
				},
				NodeType::CellInput { .. } => {
					retval.push(curid);
					queue.extend(self.get_other_input_nodes(curid));
				},
				NodeType::CellOutput { .. } | NodeType::PortOutput { .. } => {
					retval.push(curid);
					for foid in &self.nodes[curid].fanout {
						queue.insert(*foid);
					}
				},
				NodeType::PortBody | NodeType::CellBody { .. } => {
					panic!("Implementer is a fucking moron.")
				},
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
			}
		}
		retval
	}

	fn get_signal_choices(&self) -> Vec<Vec<i32>> {
		let mut signal_choices = vec![vec![]; self.nodes.len()];
		for node in &self.nodes {
			if node.node_type == NodeType::PortBody {
				let choice = signal_lookup_table::lookup_id(&node.mapped_id).unwrap();
				signal_choices[node.id] = vec![choice];
				if let Some(fiid) = node.fanin.first() {
					signal_choices[*fiid] = vec![choice];
				}
				if let Some(foid) = node.fanout.first() {
					signal_choices[*foid] = vec![choice];
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

	fn get_signal_choice_final(&self) -> Vec<Option<i32>> {
		let mut signal_choices = vec![None; self.nodes.len()];
		for node in &self.nodes {
			if node.node_type == NodeType::PortBody {
				let choice = signal_lookup_table::lookup_id(&node.mapped_id).unwrap();
				signal_choices[node.id] = Some(choice);
				if let Some(fiid) = node.fanin.first() {
					signal_choices[*fiid] = Some(choice);
				}
				if let Some(foid) = node.fanout.first() {
					signal_choices[*foid] = Some(choice);
				}
			}
		}
		for nodeid in 0..self.nodes.len() {
			if signal_choices[nodeid].is_some() {
				continue;
			}
			let attached = self.get_attached_nodes(nodeid);
			let mut choice = None;
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
						let new_id = logical_design
							.add_constant(vec![Signal::Id(self.signals[nodeid].unwrap())], vec![1]);
						logic_map[nodeid] = Some(new_id);
						logical_design.mark_as_port(
							new_id,
							Direction::Input,
							node.mapped_id.clone(),
						);
					} else if !node.fanin.is_empty() {
						let new_id = logical_design.add_lamp((
							Signal::Id(self.signals[nodeid].unwrap()),
							logical_design::DeciderOperator::NotEqual,
							Signal::Constant(0),
						));
						logic_map[nodeid] = Some(new_id);
						logical_design.mark_as_port(
							new_id,
							Direction::Output,
							node.mapped_id.clone(),
						);
					}
				},
				NodeType::CellBody { cell_type } => match cell_type {
					BodyType::ABY => {
						let sig_left = self.signals[node.fanin[0]].unwrap();
						let sig_right = self.signals[node.fanin[1]].unwrap();
						let sig_out = self.signals[node.fanout[0]].unwrap();
						let (input, output) = self.apply_binary_op(
							logical_design,
							mapped_design.get_cell(&node.mapped_id).cell_type,
							Signal::Id(sig_left),
							Signal::Id(sig_right),
							Signal::Id(sig_out),
						);
						logic_map[nodeid] = Some(input);
						logic_map[node.fanout[0]] = Some(output)
					},
					BodyType::Constant { value } => {
						logic_map[nodeid] = Some(logical_design.add_constant(
							vec![Signal::Id(self.signals[node.fanout[0]].unwrap())],
							vec![*value],
						));
					},
					BodyType::Nop => {
						let sig_in = self.signals[node.fanin[0]].unwrap();
						let sig_out = self.signals[node.fanout[0]].unwrap();
						logic_map[nodeid] =
							Some(logical_design.add_nop(Signal::Id(sig_in), Signal::Id(sig_out)))
					},
					BodyType::AY => {
						let sig_in = self.signals[node.fanin[0]].unwrap();
						let sig_out = self.signals[node.fanout[0]].unwrap();
						let (input, output) = self.apply_unary_op(
							logical_design,
							mapped_design.get_cell(&node.mapped_id).cell_type,
							Signal::Id(sig_in),
							Signal::Id(sig_out),
						);
						logic_map[nodeid] = Some(input);
						logic_map[node.fanout[0]] = Some(output)
					},
					BodyType::MultiPart => {
						let sig_in = node
							.fanin
							.iter()
							.map(|fiid| Signal::Id(self.signals[*fiid].unwrap()))
							.collect_vec();
						let sig_out = node
							.fanout
							.iter()
							.map(|foid| Signal::Id(self.signals[*foid].unwrap()))
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
	}

	fn apply_multipart_op(
		&self,
		nodeid: NodeId,
		logical_design: &mut LogicalDesign,
		op: ImplementableOp,
		mapped_cell: Option<&crate::mapped_design::Cell>,
		sig_in: Vec<Signal>,
		sig_out: Vec<Signal>,
		mapped_in_port: Vec<&str>,
		mapped_out_port: Vec<&str>,
	) -> (Vec<logical_design::NodeId>, Vec<logical_design::NodeId>) {
		let (input_wires, outputs) = match op {
			ImplementableOp::DFF => {
				let (input_wire, clk_wire, output_comb) =
					logical_design.add_dff(sig_in[0], sig_in[1], sig_out[0]);
				(vec![input_wire, clk_wire], vec![output_comb])
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
						data: sig_in[data_idx],
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
						.chunks(width)
						.into_iter()
						.map(|rom_value| rom_value.collect::<String>().unwrap_bin_str() as i32)
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
				let b_start = full_case as usize;
				let s_start = b_start + s_width;
				let (a, b, s, y) = logical_design.add_pmux(
					if full_case { None } else { Some(sig_in[0]) },
					&sig_in[b_start..s_start],
					&sig_in[s_start..sig_in.len()],
					sig_out[0],
				);
				(chain!(a, b, s).collect_vec(), vec![y])
			},
			_ => unreachable!(),
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
				logical_design::DeciderRowConjDisj::Or,
				(true, true),
				(true, true),
			);
			return (id, id);
		}
		unreachable!()
	}
}

#[cfg(test)]
mod test {
	use std::{fs::File, io::BufReader};

	use crate::{phy::PhysicalDesign, serializable_design::SerializableDesign};

	use super::*;

	#[test]
	fn design_test1() {
		let file = File::open("./test_designs/output/test1.json").unwrap();
		let reader = BufReader::new(file);
		let mapped_design: MappedDesign = serde_json::from_reader(reader).unwrap();
		let mut checked_design = CheckedDesign::new();
		let mut logical_design = LogicalDesign::new();
		let mut physical_design = PhysicalDesign::new();
		let mut serializable_design = SerializableDesign::new();
		checked_design.build_from(&mapped_design);
		logical_design.build_from(&checked_design, &mapped_design);
		logical_design.for_all(|_x, y| println!("{:?}", y));
		physical_design.build_from(&logical_design);
		serializable_design.build_from(&physical_design, &logical_design);
		let blueprint_json = serde_json::to_string(&serializable_design).unwrap();
		println!("{}", blueprint_json);
	}

	#[test]
	fn design_test2() {
		let file = File::open("./test_designs/output/test2.json").unwrap();
		let reader = BufReader::new(file);
		let mapped_design: MappedDesign = serde_json::from_reader(reader).unwrap();
		let mut checked_design = CheckedDesign::new();
		let mut logical_design = LogicalDesign::new();
		let mut physical_design = PhysicalDesign::new();
		let mut serializable_design = SerializableDesign::new();
		checked_design.build_from(&mapped_design);
		logical_design.build_from(&checked_design, &mapped_design);
		logical_design.for_all(|_x, y| println!("{:?}", y));
		physical_design.build_from(&logical_design);
		serializable_design.build_from(&physical_design, &logical_design);
		let blueprint_json = serde_json::to_string(&serializable_design).unwrap();
		println!("{}", blueprint_json);
	}

	#[test]
	fn design_test3() {
		let file = File::open("./test_designs/output/test3.json").unwrap();
		let reader = BufReader::new(file);
		let mapped_design: MappedDesign = serde_json::from_reader(reader).unwrap();
		let mut checked_design = CheckedDesign::new();
		let mut logical_design = LogicalDesign::new();
		let mut physical_design = PhysicalDesign::new();
		let mut serializable_design = SerializableDesign::new();
		checked_design.build_from(&mapped_design);
		logical_design.build_from(&checked_design, &mapped_design);
		logical_design.for_all(|_x, y| println!("{:?}", y));
		physical_design.build_from(&logical_design);
		serializable_design.build_from(&physical_design, &logical_design);
		let blueprint_json = serde_json::to_string(&serializable_design).unwrap();
		println!("{}", blueprint_json);
	}

	#[test]
	fn design_complex_expr() {
		let file = File::open("./test_designs/output/complex_expr.json").unwrap();
		let reader = BufReader::new(file);
		let mapped_design: MappedDesign = serde_json::from_reader(reader).unwrap();
		let mut checked_design = CheckedDesign::new();
		let mut logical_design = LogicalDesign::new();
		let mut physical_design = PhysicalDesign::new();
		let mut serializable_design = SerializableDesign::new();
		checked_design.build_from(&mapped_design);
		logical_design.build_from(&checked_design, &mapped_design);
		logical_design.for_all(|_, y| println!("{:?}", y));
		physical_design.build_from(&logical_design);
		serializable_design.build_from(&physical_design, &logical_design);
		let blueprint_json = serde_json::to_string(&serializable_design).unwrap();
		println!("\n{}\n", blueprint_json);
	}

	#[test]
	fn design_balancer() {
		let file = File::open("./test_designs/output/test4.json").unwrap();
		let reader = BufReader::new(file);
		let mapped_design: MappedDesign = serde_json::from_reader(reader).unwrap();
		let mut checked_design = CheckedDesign::new();
		let mut logical_design = LogicalDesign::new();
		let mut physical_design = PhysicalDesign::new();
		let mut serializable_design = SerializableDesign::new();
		checked_design.build_from(&mapped_design);
		for n in &checked_design.nodes {
			println!("{:?}", n);
		}
		logical_design.build_from(&checked_design, &mapped_design);
		logical_design.for_all(|_, node| println!("{:?}", node));
		physical_design.build_from(&logical_design);
		serializable_design.build_from(&physical_design, &logical_design);
		let blueprint_json = serde_json::to_string(&serializable_design).unwrap();
		println!("{}", blueprint_json);
	}

	#[test]
	fn design_single_dff() {
		let file = File::open("./test_designs/output/test5.json").unwrap();
		let reader = BufReader::new(file);
		let mapped_design: MappedDesign = serde_json::from_reader(reader).unwrap();
		let mut checked_design = CheckedDesign::new();
		let mut logical_design = LogicalDesign::new();
		let mut physical_design = PhysicalDesign::new();
		let mut serializable_design = SerializableDesign::new();
		checked_design.build_from(&mapped_design);
		logical_design.build_from(&checked_design, &mapped_design);
		logical_design.for_all(|_, y| println!("{:?}", y));
		physical_design.build_from(&logical_design);
		serializable_design.build_from(&physical_design, &logical_design);
		let blueprint_json = serde_json::to_string(&serializable_design).unwrap();
		println!("{}", blueprint_json);
	}

	#[test]
	fn design_swizzle_bits() {
		let file = File::open("./test_designs/output/test6.json").unwrap();
		let reader = BufReader::new(file);
		let mapped_design: MappedDesign = serde_json::from_reader(reader).unwrap();
		let mut checked_design = CheckedDesign::new();
		let mut logical_design = LogicalDesign::new();
		let mut physical_design = PhysicalDesign::new();
		let mut serializable_design = SerializableDesign::new();
		checked_design.build_from(&mapped_design);
		logical_design.build_from(&checked_design, &mapped_design);
		logical_design.for_all(|_, y| println!("{:?}", y));
		physical_design.build_from(&logical_design);
		serializable_design.build_from(&physical_design, &logical_design);
		let blueprint_json = serde_json::to_string(&serializable_design).unwrap();
		println!("\n{}", blueprint_json);
	}

	#[test]
	fn design_simple_lut() {
		let file = File::open("./test_designs/output/test7.json").unwrap();
		let reader = BufReader::new(file);
		let mapped_design: MappedDesign = serde_json::from_reader(reader).unwrap();
		let mut checked_design = CheckedDesign::new();
		let mut logical_design = LogicalDesign::new();
		let mut physical_design = PhysicalDesign::new();
		let mut serializable_design = SerializableDesign::new();
		checked_design.build_from(&mapped_design);
		logical_design.build_from(&checked_design, &mapped_design);
		logical_design.for_all(|_, y| println!("{:?}", y));
		physical_design.build_from(&logical_design);
		serializable_design.build_from(&physical_design, &logical_design);
		let blueprint_json = serde_json::to_string(&serializable_design).unwrap();
		println!("\n{}", blueprint_json);
	}

	#[test]
	fn design_simple_lut_alterante() {
		let file = File::open("./test_designs/output/test9.json").unwrap();
		let reader = BufReader::new(file);
		let mapped_design: MappedDesign = serde_json::from_reader(reader).unwrap();
		let mut checked_design = CheckedDesign::new();
		let mut logical_design = LogicalDesign::new();
		let mut physical_design = PhysicalDesign::new();
		let mut serializable_design = SerializableDesign::new();
		checked_design.build_from(&mapped_design);
		logical_design.build_from(&checked_design, &mapped_design);
		logical_design.for_all(|_, y| println!("{:?}", y));
		physical_design.build_from(&logical_design);
		serializable_design.build_from(&physical_design, &logical_design);
		let blueprint_json = serde_json::to_string(&serializable_design).unwrap();
		println!("\n{}", blueprint_json);
	}
}

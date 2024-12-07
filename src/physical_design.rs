use std::{
	cmp::{min, Ordering},
	collections::{linked_list, HashMap},
	hash::Hash,
	vec,
};

use crate::logical_design::{self as ld, LogicalDesign, NodeId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct CombinatorId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct WireId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct TerminalId(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct PoleId(usize);

#[derive(Debug)]
struct Combinator {
	id: CombinatorId,
	logic: ld::NodeId,
	position: (f64, f64),
	placed: bool,
	orientation: u32,
}

#[derive(Debug)]
struct Wire {
	id: WireId,
	logic: ld::NodeId,
	node1_id: CombinatorId,
	node2_id: CombinatorId,
	terminal1_id: TerminalId,
	terminal2_id: TerminalId,
}

#[derive(Debug)]
enum WireHopType {
	Small,
	Medium,
	Big,
	Substation,
	Combinator,
	Lamp,
}

#[derive(Debug)]
struct Pole {
	id: PoleId,
	logic: ld::NodeId,
	position: (f64, f64),
}

#[derive(Debug)]
pub struct PhysicalDesign {
	combs: Vec<Combinator>,
	wires: Vec<Wire>,
	poles: Vec<Pole>,

	idx_combs: HashMap<ld::NodeId, CombinatorId>,
	idx_wires: HashMap<ld::NodeId, WireId>,
	idx_poles: HashMap<ld::NodeId, PoleId>,
	space: HashMap<(i32, i32), SpaceNode>,

	logical: ld::LogicalDesign,
}

#[derive(Debug)]
enum SpaceNode {
	Combinator {
		id: CombinatorId,
		ld_id: ld::NodeId,
	},
	Pole {
		id: PoleId,
		hop_type: WireHopType,
		ld_id: ld::NodeId,
	},
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
					placed: false,
					orientation: 4, // This shouldn't have to change
				});
				idx_combs.insert(ld_node.id, id);
			}
			ld::NodeFunction::WireSum => { /* Do nothing for now */ }
		});

		let ret = PhysicalDesign {
			combs,
			wires: vec![],
			poles: vec![],
			idx_combs,
			idx_wires: HashMap::new(),
			idx_poles: HashMap::new(),
			space: HashMap::new(),
			logical,
		};
		ret
	}

	pub fn mut_comb_and_logical(&mut self, id: CombinatorId) -> (&mut Combinator, &ld::Node) {
		let comb = &mut self.combs[id.0];
		let ld_node = self.logical.get_node(comb.logic);
		(comb, ld_node)
	}

	pub fn get_comb_and_logical(&mut self, id: CombinatorId) -> (&Combinator, &ld::Node) {
		let comb = &self.combs[id.0];
		let ld_node = self.logical.get_node(comb.logic);
		(comb, ld_node)
	}

	pub fn get_logical(&self, id: CombinatorId) -> &ld::Node {
		let comb = &self.combs[id.0];
		let ld_node = self.logical.get_node(comb.logic);
		ld_node
	}

	pub fn place_combs(&mut self) {
		let mut max_node = None;
		self.logical.for_all_roots(|ld_node| {
			let cur_rev_depth = self.logical.get_rev_depth(ld_node.id);
			max_node = match max_node {
				Some((_, rev_depth)) => {
					if cur_rev_depth > rev_depth {
						Some((ld_node.id, cur_rev_depth))
					} else {
						max_node
					}
				}
				None => Some((ld_node.id, cur_rev_depth)),
			};
		});
		let start_ld_id = match max_node {
			Some((ld_id, _)) => ld_id,
			None => return,
		};
		match self.logical.get_node(start_ld_id).function {
			ld::NodeFunction::WireSum => assert!(false, ""),
			_ => {}
		};

		let start_id = *self.idx_combs.get(&start_ld_id).unwrap();
		self.place_comb_physical((0.0, 0.0), start_id);
		self.recurse_place_comb(start_id)
	}

	pub fn place_comb_physical(
		&mut self,
		position: (f64, f64),
		id_to_place: CombinatorId,
	) -> Result<(), ()> {
		let pos_start = (position.0 as i32, position.1 as i32);
		let comb_to_place = &mut self.combs[id_to_place.0];
		let ld_node = self.logical.get_node(comb_to_place.logic);
		let hop_spec = ld_node.function.wire_hop_type().wire_hop_spec();
		for x in 0..hop_spec.dim.0 {
			for y in 0..hop_spec.dim.1 {
				let key = (pos_start.0 + x, pos_start.1 + y);
				if self.space.contains_key(&key) {
					return Result::Err(());
				}
			}
		}
		for x in 0..hop_spec.dim.0 {
			for y in 0..hop_spec.dim.1 {
				let key = (pos_start.0 + x, pos_start.1 + y);
				self.space.insert(
					key,
					SpaceNode::Combinator {
						id: id_to_place,
						ld_id: comb_to_place.logic,
					},
				);
			}
		}
		comb_to_place.position = (
			position.0 + hop_spec.dim.0 as f64 / 2.0,
			position.1 + hop_spec.dim.1 as f64 / 2.0,
		);
		comb_to_place.placed = true;
		Result::Ok(())
	}

	fn recurse_place_comb(&mut self, id: CombinatorId) {
		if self.combs[id.0].placed {
			return;
		}
		let sum = {
			let ld_comb = self.get_logical(id);
			ld_comb
				.fanin
				.iter()
				.chain(ld_comb.fanout.iter())
				.filter_map(|ld_id| {
					let comb = &self.combs[self.idx_combs.get(ld_id).unwrap().0];
					if comb.placed {
						Some((comb.position, 1.0))
					} else {
						None
					}
				})
				.reduce(|(pos, count), (pos2, count2)| {
					((pos.0 + pos2.0, pos.1 + pos2.1), count + count2)
				})
		};
		match sum {
			Some((pos, count)) => {
				let avg_pos = (pos.0 / count, (pos.1 / count / 2.0).round() * 2.0);
				for offset in get_proposed_placements() {
					let placement_pos = (avg_pos.0 + offset.0, avg_pos.1 + offset.1);
					match self.place_comb_physical(placement_pos, id) {
						Ok(_) => break,
						Err(_) => {}
					}
				}
				assert!(false, "Placement failed");
			}
			None => match self.place_comb_physical((0.0, 0.0), id) {
				Ok(_) => {}
				Err(_) => assert!(false),
			},
		}
		for fanout_wire_id in self.get_logical(id).fanout.clone() {
			let fo_node = self.logical.get_node(fanout_wire_id);
			match fo_node.function {
				ld::NodeFunction::WireSum => {
					for fanout_combinator in fo_node.fanout.clone() {
						self.recurse_place_comb(*self.idx_combs.get(&fanout_combinator).unwrap())
					}
				}
				_ => assert!(
					false,
					"A combinator is connected -- without a wire -- to another combinator."
				),
			}
		}
		for fanin_wire_id in self.get_logical(id).fanin.clone() {
			let fo_node = self.logical.get_node(fanin_wire_id);
			match fo_node.function {
				ld::NodeFunction::WireSum => {
					for fanin_combinator in fo_node.fanin.clone() {
						self.recurse_place_comb(*self.idx_combs.get(&fanin_combinator).unwrap())
					}
				}
				_ => assert!(
					false,
					"A combinator is connected -- without a wire -- to another combinator."
				),
			}
		}
	}

	pub fn connect_combs(&mut self) {
		let mut global_edges: Vec<(ld::NodeId, Vec<(ld::NodeId, ld::NodeId)>)> = vec![];
		self.logical.for_all(|_, ld_node| {
			match ld_node.function {
				ld::NodeFunction::WireSum => {
					let to_connect: Vec<ld::NodeId> = ld_node
						.fanin
						.iter()
						.chain(ld_node.fanout.iter())
						.cloned()
						.collect();
					let mut edges = vec![];
					for id_i in to_connect.iter() {
						for id_j in to_connect.iter() {
							if id_i == id_j {
								continue;
							}
							let comb_i = &self.combs[self.idx_combs.get(&id_i).unwrap().0];
							let comb_j = &self.combs[self.idx_combs.get(&id_j).unwrap().0];
							let ld_node_i = self.logical.get_node(comb_i.logic);
							let ld_node_j = self.logical.get_node(comb_j.logic);
							let hop_spec_i = ld_node_i.function.wire_hop_type().wire_hop_spec();
							let hop_spec_j = ld_node_j.function.wire_hop_type().wire_hop_spec();
							let min_distance = f64::min(hop_spec_i.reach, hop_spec_j.reach);
							if euclidean_distance_squared_f64_pair(comb_i.position, comb_j.position)
								< min_distance * min_distance
							{
								if id_i < id_j {
									edges.push((*id_i, *id_j));
								} else {
									edges.push((*id_j, *id_i));
								}
							}
						}
					}
					global_edges.push((ld_node.id, remove_non_unique(edges)));
				}
				_ => { /* Do nothing for non-wires */ }
			}
		});
		for wires in global_edges {
			for edge in wires.1 {
				self.connect_wire(
					wires.0,
					*self.idx_combs.get(&edge.0).unwrap(),
					*self.idx_combs.get(&edge.1).unwrap(),
				);
			}
		}
	}

	fn connect_wire(
		&mut self,
		ld_id_wire: ld::NodeId,
		id_comb_a: CombinatorId,
		id_comb_b: CombinatorId,
	) {
		let id_wire = self.wires.len();
		let comb_a = &self.combs[id_comb_a.0];
		let ld_comb_a = self.logical.get_node(comb_a.logic);
		let (term_a, term_b) = if ld_comb_a.fanout.contains(&ld_id_wire) {
			(TerminalId(2), TerminalId(0))
		} else {
			(TerminalId(0), TerminalId(2))
		};
		self.wires.push(Wire {
			id: WireId(id_wire),
			logic: ld_id_wire,
			node1_id: id_comb_a,
			node2_id: id_comb_b,
			terminal1_id: term_a,
			terminal2_id: term_b,
		})
	}
}

#[derive(Debug)]
struct WireHopSpec {
	dim: (i32, i32),
	reach: f64,
	area: i32,
}

impl WireHopSpec {
	fn new(dim: (i32, i32), reach: f64, area: i32) -> Self {
		WireHopSpec { dim, reach, area }
	}
}

impl WireHopType {
	fn wire_hop_spec(&self) -> WireHopSpec {
		match self {
			WireHopType::Small => WireHopSpec::new((1, 1), 7.5, 5),
			WireHopType::Medium => WireHopSpec::new((1, 1), 9.0, 11),
			WireHopType::Big => WireHopSpec::new((2, 2), 32.0, 4),
			WireHopType::Substation => WireHopSpec::new((2, 2), 18.0, 18),
			WireHopType::Combinator => WireHopSpec::new((1, 2), 9.0, 0),
			WireHopType::Lamp => WireHopSpec::new((1, 1), 9.0, 0),
		}
	}
}

impl ld::NodeFunction {
	fn wire_hop_type(&self) -> WireHopType {
		match self {
			ld::NodeFunction::Arithmetic { .. } => WireHopType::Combinator,
			ld::NodeFunction::Decider { .. } => WireHopType::Combinator,
			ld::NodeFunction::Constant { .. } => WireHopType::Lamp,
			ld::NodeFunction::Lamp { .. } => WireHopType::Lamp,
			ld::NodeFunction::WireSum => unreachable!(),
		}
	}
}

const fn euclidean_distance_squared(x: i32, y: i32) -> i32 {
	x * x + y * y
}

fn euclidean_distance_squared_f64_pair(x: (f64, f64), y: (f64, f64)) -> f64 {
	(y.0 - x.0) * (y.0 - x.0) + (y.1 - x.1) * (y.1 - x.1)
}

fn heuristic(x: i32, y: i32) -> f64 {
	let x_abs = x.abs() as f64;
	let y_abs = y.abs() as f64;
	y_abs / (x_abs + 1.0) + 2.0 * x_abs - 0.6 * y_abs
}

fn get_proposed_placements() -> Vec<(f64, f64)> {
	let max_distance = 7;
	let max_distance_squared = max_distance * max_distance;
	let mut points = Vec::new();

	// Generate points
	for x in -max_distance..=max_distance {
		for y in -max_distance..=max_distance {
			if y % 2 == 0 && euclidean_distance_squared(x, y) <= max_distance_squared {
				points.push((x, y));
			}
		}
	}

	// Sort points by the heuristic
	points.sort_by(|&(x1, y1), &(x2, y2)| {
		let h1 = heuristic(x1, y1);
		let h2 = heuristic(x2, y2);
		h1.partial_cmp(&h2).unwrap_or(Ordering::Equal)
	});

	points
		.into_iter()
		.map(|p| (p.0 as f64, p.1 as f64))
		.collect()
}

fn remove_non_unique<T: Eq + std::hash::Hash + Clone>(vec: Vec<T>) -> Vec<T> {
	let mut counts: HashMap<T, usize> = HashMap::new();
	for item in &vec {
		*counts.entry(item.clone()).or_insert(0) += 1;
	}
	vec.into_iter().filter(|item| counts[item] == 1).collect()
}

use std::{
	cmp::Ordering,
	collections::{HashMap, HashSet},
	vec,
};

use crate::logical_design::{self as ld, LogicalDesign};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CombinatorId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct WireId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct TerminalId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct PoleId(usize);

#[derive(Debug)]
pub struct Combinator {
	pub id: CombinatorId,
	pub logic: ld::NodeId,
	pub position: (f64, f64),
	pub placed: bool,
	pub orientation: u32,
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct Wire {
	pub id: WireId,
	pub logic: ld::NodeId,
	pub node1_id: CombinatorId,
	pub node2_id: CombinatorId,
	pub terminal1_id: TerminalId,
	pub terminal2_id: TerminalId,
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum WireHopType {
	Small,
	Medium,
	Big,
	Substation,
	Combinator,
	Lamp,
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct Pole {
	pub id: PoleId,
	pub logic: ld::NodeId,
	pub position: (f64, f64),
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct PhysicalDesign {
	combs: Vec<Combinator>,
	wires: Vec<Wire>,
	poles: Vec<Pole>,

	idx_combs: HashMap<ld::NodeId, CombinatorId>,
	idx_wires: HashMap<ld::NodeId, WireId>,
	idx_poles: HashMap<ld::NodeId, PoleId>,
	space: HashMap<(i32, i32), SpaceNode>,
}

#[allow(dead_code)]
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

#[allow(dead_code)]
impl PhysicalDesign {
	pub fn new() -> Self {
		PhysicalDesign {
			combs: vec![],
			wires: vec![],
			poles: vec![],
			idx_combs: HashMap::new(),
			idx_wires: HashMap::new(),
			idx_poles: HashMap::new(),
			space: HashMap::new(),
		}
	}

	pub fn build_from(&mut self, logical: &LogicalDesign) {
		self.extract_combs(logical);
		self.place_combs(logical);
		self.connect_combs(logical);
	}

	pub fn for_all_combinators<F>(&self, mut func: F)
	where
		F: FnMut(&Combinator),
	{
		for x in &self.combs {
			func(x)
		}
	}

	pub fn for_all_poles<F>(&self, mut func: F)
	where
		F: FnMut(&Pole),
	{
		for x in &self.poles {
			func(x)
		}
	}

	pub fn for_all_wires<F>(&self, mut func: F)
	where
		F: FnMut(&Wire),
	{
		for x in &self.wires {
			func(x)
		}
	}

	fn extract_combs(&mut self, logical: &LogicalDesign) {
		logical.for_all(|_, ld_node| match &ld_node.function {
			ld::NodeFunction::Arithmetic { .. }
			| ld::NodeFunction::Decider { .. }
			| ld::NodeFunction::Constant { .. }
			| ld::NodeFunction::Lamp { .. } => {
				let id = CombinatorId(self.combs.len());
				self.combs.push(Combinator {
					id,
					logic: ld_node.id,
					position: (0.0, 0.0), // Later we spread them out
					placed: false,
					orientation: 4, // This shouldn't have to change
				});
				self.idx_combs.insert(ld_node.id, id);
			}
			ld::NodeFunction::WireSum => { /* Do nothing for now */ }
		});
	}

	pub fn get_logical<'a>(&self, id: CombinatorId, logical: &'a LogicalDesign) -> &'a ld::Node {
		let comb = &self.combs[id.0];
		let ld_node = logical.get_node(comb.logic);
		ld_node
	}

	fn place_combs(&mut self, logical: &LogicalDesign) {
		let mut roots_and_depth = vec![];
		logical.for_all_roots(|ld_node| {
			let rev_depth = logical.get_rev_depth(ld_node.id);
			roots_and_depth.push((*self.idx_combs.get(&ld_node.id).unwrap(), rev_depth));
		});

		roots_and_depth
			.sort_by(|(_, rev_depth_a), (_, rev_depth_b)| rev_depth_a.cmp(rev_depth_b).reverse());

		for (root, _) in roots_and_depth {
			self.recurse_place_comb(root, logical)
		}
	}

	fn place_comb_physical(
		&mut self,
		position: (f64, f64),
		id_to_place: CombinatorId,
		logical: &LogicalDesign,
	) -> Result<(), ()> {
		let pos_start = (position.0 as i32, position.1 as i32);
		let comb_to_place = &mut self.combs[id_to_place.0];
		let ld_node = logical.get_node(comb_to_place.logic);
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

	fn recurse_place_comb(&mut self, id: CombinatorId, logical: &LogicalDesign) {
		if self.combs[id.0].placed {
			return;
		}
		let sum = {
			let ld_comb = self.get_logical(id, logical);
			let ld_in_wire_fanin_iter = ld_comb
				.fanin
				.get(0) // Option<NodeId>
				.map(|id| {
					logical.assert_is_wire_sum(*id);
					logical.get_node(*id).fanin.iter()
				})
				.into_iter()
				.flatten();
			let ld_out_wire_fanout_iter = ld_comb
				.fanout
				.get(0) // Option<NodeId>
				.map(|id| {
					logical.assert_is_wire_sum(*id);
					logical.get_node(*id).fanout.iter()
				})
				.into_iter()
				.flatten();
			ld_in_wire_fanin_iter
				.chain(ld_out_wire_fanout_iter)
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
				let avg_pos = (pos.0 / count, pos.1 / count);
				let mut good = false;
				for offset in get_proposed_placements() {
					let placement_pos = (
						((avg_pos.0 + offset.0).floor() / 2.0).floor() * 2.0,
						(avg_pos.1 + offset.1).floor(),
					);
					match self.place_comb_physical(placement_pos, id, logical) {
						Ok(_) => {
							good = true;
							break;
						}
						Err(_) => {}
					}
				}
				if !good {
					assert!(false, "Placement failed");
				}
			}
			None => {
				let mut good = false;
				let mut offset_factor = 1.0;
				while !good {
					for offset in get_proposed_placements() {
						match self.place_comb_physical(
							(offset.0 * offset_factor, offset.1 * offset_factor),
							id,
							logical,
						) {
							Ok(_) => {
								good = true;
								break;
							}
							Err(_) => {}
						};
					}
					if offset_factor > 100.0 {
						assert!(false, "Placement failed");
					}
					offset_factor += 1.0;
				}
			}
		}
		for fanout_wire_id in self.get_logical(id, logical).fanout.iter() {
			let fo_node = logical.get_node(*fanout_wire_id);
			match fo_node.function {
				ld::NodeFunction::WireSum => {
					for fanout_combinator in &fo_node.fanout {
						self.recurse_place_comb(
							*self.idx_combs.get(&fanout_combinator).unwrap(),
							logical,
						)
					}
				}
				_ => assert!(
					false,
					"A combinator is connected -- without a wire -- to another combinator."
				),
			}
		}
		for fanin_wire_id in self.get_logical(id, logical).fanin.iter() {
			let fo_node = logical.get_node(*fanin_wire_id);
			match fo_node.function {
				ld::NodeFunction::WireSum => {
					for fanin_combinator in &fo_node.fanin {
						self.recurse_place_comb(
							*self.idx_combs.get(&fanin_combinator).unwrap(),
							logical,
						)
					}
				}
				_ => assert!(
					false,
					"A combinator is connected -- without a wire -- to another combinator."
				),
			}
		}
	}

	fn connect_combs(&mut self, logical: &LogicalDesign) {
		let mut global_edges: Vec<(ld::NodeId, Vec<(ld::NodeId, ld::NodeId)>)> = vec![];
		logical.for_all(|_, ld_node| {
			match ld_node.function {
				ld::NodeFunction::WireSum => {
					let mut edges = vec![];
					for id_i in ld_node.fanout.iter() {
						for id_j in ld_node.fanin.iter() {
							let comb_i = &self.combs[self.idx_combs.get(&id_i).unwrap().0];
							let comb_j = &self.combs[self.idx_combs.get(&id_j).unwrap().0];
							let ld_node_i = logical.get_node(comb_i.logic);
							let ld_node_j = logical.get_node(comb_j.logic);
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
					logical,
				);
			}
		}
	}

	fn connect_wire(
		&mut self,
		ld_id_wire: ld::NodeId,
		id_comb_a: CombinatorId,
		id_comb_b: CombinatorId,
		logical: &LogicalDesign,
	) {
		let id_wire = self.wires.len();
		let comb_a = &self.combs[id_comb_a.0];
		let comb_b = &self.combs[id_comb_b.0];
		let ld_comb_a = logical.get_node(comb_a.logic);
		let ld_comb_b = logical.get_node(comb_b.logic);
		assert!(ld_comb_a.fanout.contains(&ld_id_wire));
		assert!(ld_comb_b.fanin.contains(&ld_id_wire));
		let term_a = match ld_comb_a.function {
			ld::NodeFunction::Arithmetic { .. } | ld::NodeFunction::Decider { .. } => TerminalId(3),
			ld::NodeFunction::Constant { .. } | ld::NodeFunction::Lamp { .. } => TerminalId(1),
			ld::NodeFunction::WireSum => unreachable!(),
		};
		let term_b = TerminalId(1);
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

#[allow(dead_code)]
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
			WireHopType::Combinator => WireHopSpec::new((2, 1), 10.0, 0),
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
	euclidean_distance_squared(x, y) as f64
}

fn get_proposed_placements() -> Vec<(f64, f64)> {
	let max_distance = 10i32;
	let max_distance_squared = max_distance * max_distance;
	let mut points = Vec::new();

	// Generate points
	for x in -max_distance..=max_distance {
		for y in -max_distance..=max_distance {
			if x.rem_euclid(2) == 0 && euclidean_distance_squared(x, y) <= max_distance_squared {
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
	let mut marks: HashSet<T> = HashSet::new();
	for item in &vec {
		marks.insert(item.clone());
	}
	marks.into_iter().collect()
}

#[cfg(test)]
mod test {
	use super::*;
	#[test]
	fn new() {
		let mut p = PhysicalDesign::new();
		let l = ld::get_simple_logical_design();
		p.build_from(&l);
	}
}

use std::{
	collections::{BinaryHeap, HashMap, HashSet},
	hash::Hash,
};

struct Heuristic<X>
where
	X: TopologyIndex,
{
	val: f32,
	id: X,
}

impl<X> Eq for Heuristic<X> where X: TopologyIndex {}

impl<X> PartialEq for Heuristic<X>
where
	X: TopologyIndex,
{
	fn eq(&self, other: &Self) -> bool {
		self.val.partial_cmp(&other.val) == Some(std::cmp::Ordering::Equal)
	}
}

impl<X> Ord for Heuristic<X>
where
	X: TopologyIndex,
{
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		std::cmp::Reverse(self.val)
			.partial_cmp(&std::cmp::Reverse(other.val))
			.unwrap()
	}
}

impl<X> PartialOrd for Heuristic<X>
where
	X: TopologyIndex,
{
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		Some(self.cmp(other))
	}
}

impl<X> Heuristic<X>
where
	X: TopologyIndex,
{
	fn h(val: f32, id: X) -> Heuristic<X> {
		Heuristic { val, id }
	}
}

pub trait Topology<'iter, N, X>
where
	X: TopologyIndex,
{
	type Iter: Iterator<Item = X>;
	fn neighbors(&'iter self, index: &X, goal: &X) -> Self::Iter;
	fn contains(&self, index: &X) -> bool;
	fn distance(&self, a: &X, b: &X) -> f32;
	fn heuristic(&self, a: &X, b: &X) -> f32;
}

pub trait TopologyIndex
where
	Self: Hash + Copy + Eq,
{
}

pub fn a_star<'a, T: Topology<'a, N, X>, N, X: TopologyIndex>(
	topology: &'a T,
	a: &X,
	b: &X,
	b_alt_condition: Option<f32>,
	relaxion_param: Option<f32>,
) -> Vec<X> {
	a_star_initial_set(topology, &[*a], &[*b], b_alt_condition, relaxion_param)
}

pub fn a_star_initial_set<'a, T: Topology<'a, N, X>, N, X: TopologyIndex>(
	topology: &'a T,
	a: &[X],
	b: &[X],
	b_alt_condition: Option<f32>,
	relaxation: Option<f32>,
) -> Vec<X> {
	if a.iter().any(|a| !topology.contains(a)) || !topology.contains(&b[0]) {
		return vec![];
	}
	let mut open_set = BinaryHeap::<Heuristic<X>>::new();
	open_set.reserve(2048);
	let mut open_set_check = HashSet::<X>::new();
	let mut came_from = HashMap::<X, X>::new();
	let mut g_score = HashMap::<X, f32>::new();
	for initial_a in a.iter() {
		g_score.insert(*initial_a, 0.0);
		open_set.push(Heuristic::h(
			topology.distance(initial_a, &b[0]),
			*initial_a,
		));
		open_set_check.insert(*initial_a);
	}
	while !open_set.is_empty() {
		let curr = open_set.pop().unwrap();
		open_set_check.remove(&curr.id);
		if b.contains(&curr.id)
			|| b_alt_condition.is_some_and(|thresh| topology.distance(&curr.id, &b[0]) < thresh)
		{
			let mut just_came_from = curr.id;
			let mut retval: Vec<X> = vec![];
			while !a.contains(&just_came_from) {
				retval.push(just_came_from);
				just_came_from = came_from[&just_came_from];
			}
			retval.push(just_came_from);
			return retval;
		}
		let iter = topology.neighbors(&curr.id, &b[0]);
		for n in iter {
			let new_d = topology.distance(&curr.id, &n);
			let tentative_g_score = *(g_score.entry(curr.id)).or_insert(f32::INFINITY) + new_d;
			let g_score_current = *(g_score.entry(n)).or_insert(f32::INFINITY);
			if tentative_g_score < g_score_current {
				let h_n = topology.heuristic(&n, &b[0]);
				came_from.insert(n, curr.id);
				g_score.insert(n, tentative_g_score);
				if !open_set_check.contains(&n) {
					open_set_check.insert(n);
					open_set.push(Heuristic::h(
						tentative_g_score * relaxation.unwrap_or(1.0) + h_n,
						n,
					));
				}
			}
		}
	}
	vec![]
}

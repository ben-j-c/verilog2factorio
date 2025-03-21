use super::placement::global::GlobalPlacement;
use super::route::Topology;
use super::*;

use core::{f64, panic};
use itertools::Itertools;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};

use std::{
	cmp::Ordering,
	collections::{HashMap, HashSet, LinkedList},
	hash::Hash,
	vec,
};

use crate::ndarr::Arr2;
use crate::phy::placement::*;
use crate::util::{self, hash_map, hash_set, HashM, HashS};
use crate::{
	logical_design::{self as ld, LogicalDesign, WireColour},
	svg::SVG,
};

use super::placement::Placement;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PhyId(usize);

impl Default for PhyId {
	fn default() -> Self {
		Self(usize::MAX)
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct WireId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TerminalId(pub u32);

impl TerminalId {
	fn input(color: ld::WireColour) -> TerminalId {
		match color {
			WireColour::Red => TerminalId(1),
			WireColour::Green => TerminalId(2),
		}
	}

	fn output_combinator(color: ld::WireColour) -> TerminalId {
		match color {
			WireColour::Red => TerminalId(3),
			WireColour::Green => TerminalId(4),
		}
	}

	fn output_constant(color: ld::WireColour) -> TerminalId {
		Self::input(color)
	}
}

#[derive(Debug)]
pub struct Route {
	input_sided: bool,
	far_side_id: PhyId,
	near_side_pole_id: PhyId,
	far_side_pole_id: PhyId,
}

#[derive(Debug)]
pub struct PhyNode {
	pub id: PhyId,
	pub logic: ld::NodeId,
	pub position: (f64, f64),
	pub placed: bool,
	pub orientation: u32,
	pub partition: i32,
	pub is_pole: bool,
	pub routes: Vec<Route>,
	pub foreign_routes: Vec<PhyId>,
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct Wire {
	pub id: WireId,
	pub node1_id: PhyId,
	pub node2_id: PhyId,
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
pub struct PhysicalDesign {
	nodes: Vec<PhyNode>,
	wires: Vec<Wire>,

	idx_combs: HashM<ld::NodeId, PhyId>,
	space: Vec<Arr2<PhyId>>,

	connected_wires: Vec<Vec<WireId>>,

	global_space: Arr2<PhyId>,

	side_length_single_partition: usize,
	side_length_partitions: usize,
	n_partitions: i32,
	n_edges_cut: i32,
	partition_assignments: Vec<(usize, usize)>,
	local_assignments: Vec<Vec<(usize, usize)>>,
	intra_partition_margin: usize,

	user_partition_margin: Option<usize>,
	user_partition_size: Option<usize>,
}

#[allow(dead_code)]
impl PhysicalDesign {
	pub fn new() -> Self {
		PhysicalDesign {
			nodes: vec![],
			wires: vec![],
			idx_combs: hash_map(),
			space: vec![],
			connected_wires: vec![],

			global_space: Arr2::new([2, 1]),

			side_length_single_partition: 100,
			side_length_partitions: 1,
			n_partitions: 1,
			n_edges_cut: 0,
			partition_assignments: vec![(0, 0)],
			local_assignments: vec![vec![]],
			intra_partition_margin: 0,

			user_partition_margin: None,
			user_partition_size: None,
		}
	}

	pub fn with_parameters(
		user_partition_margin: Option<usize>,
		user_partition_size: Option<usize>,
	) -> Self {
		let mut ret = Self::new();
		ret.user_partition_margin = user_partition_margin;
		ret.user_partition_size = user_partition_size;
		ret
	}

	pub fn build_from(&mut self, logical: &LogicalDesign) {
		let partition_size = self.user_partition_size.unwrap_or(1024);
		let scale_factor = 1.5;
		self.side_length_single_partition =
			(partition_size as f64 * scale_factor * 2.0).sqrt().ceil() as usize;
		self.extract_combs(logical);
		self.partition(logical, partition_size as i32);
		self.side_length_partitions = (self.n_partitions as f64).sqrt().ceil() as usize;

		let connectivity = self.get_connectivity_as_vec_usize(logical);
		let (
			partition_local_connectivity,
			partition_global_connectivity,
			local_to_global,
			global_to_local,
		) = self.split_connections_to_local_and_global(&connectivity);

		self.place_global(&partition_global_connectivity);
		self.place_local(logical, &partition_local_connectivity, &local_to_global);
		self.global_freeze_and_route(
			logical,
			&partition_global_connectivity,
			&local_to_global,
			&global_to_local,
		);
		self.connected_wires = vec![vec![]; self.nodes.len()];
		self.connect_combs(logical);
		self.validate_against(logical);
	}

	pub fn for_all_phy<F>(&self, mut func: F)
	where
		F: FnMut(&PhyNode),
	{
		for x in &self.nodes {
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

	pub(crate) fn extract_combs(&mut self, logical: &LogicalDesign) {
		logical.for_all(|_, ld_node| match &ld_node.function {
			ld::NodeFunction::Arithmetic { .. }
			| ld::NodeFunction::Decider { .. }
			| ld::NodeFunction::Constant { .. }
			| ld::NodeFunction::Lamp { .. } => {
				let id = PhyId(self.nodes.len());
				self.nodes.push(PhyNode {
					id,
					logic: ld_node.id,
					position: (0.0, 0.0),
					placed: false,
					orientation: 4,
					partition: 0,
					is_pole: false,
					routes: vec![],
					foreign_routes: vec![],
				});
				self.idx_combs.insert(ld_node.id, id);
				self.connected_wires.push(vec![]);
			}
			ld::NodeFunction::WireSum(_c) => { /* Do nothing for now */ }
		});
	}

	pub fn get_logical<'a>(&self, id: PhyId, logical: &'a LogicalDesign) -> &'a ld::Node {
		let comb = &self.nodes[id.0];
		let ld_node = logical.get_node(comb.logic);
		ld_node
	}

	fn get_initializations(local_connectivity: &Vec<Vec<usize>>) -> Vec<Vec<(usize, usize)>> {
		let side_length = (local_connectivity.len() as f64 * 2.0).sqrt().ceil() as usize;
		let mut retval = vec![];
		retval.push(vec![(0, 0); local_connectivity.len()]);
		{
			let initial0 = retval.last_mut().unwrap();
			let mut x = 0;
			let mut y: isize = 0;
			let mut ydir = 1;
			for c in Self::get_bfs_order(0, local_connectivity) {
				if y >= side_length as isize || y < 0 {
					x += 2;
					ydir = -ydir;
					y += ydir;
				}
				initial0[c] = (x, y as usize);
				y += ydir;
			}
		}
		retval.push(vec![(0, 0); local_connectivity.len()]);
		{
			let initial1 = retval.last_mut().unwrap();
			let mut x = 0;
			let mut y = 0;
			for c in Self::get_bfs_order(0, local_connectivity) {
				if y >= side_length {
					x += 2;
					y = 0;
				}
				initial1[c] = (x, y);
				y += 1;
			}
		}
		retval.push(vec![(0, 0); local_connectivity.len()]);
		{
			let initial2 = retval.last_mut().unwrap();
			let mut x = 0;
			let mut y: isize = 0;
			let mut ydir = 1;
			for c in 0..local_connectivity.len() {
				if y >= side_length as isize || y < 0 {
					x += 2;
					ydir = -ydir;
					y += ydir;
				}
				initial2[c] = (x, y as usize);
				y += ydir;
			}
		}
		retval.push(vec![(0, 0); local_connectivity.len()]);
		{
			let initial3 = retval.last_mut().unwrap();
			let mut x = 0;
			let mut y = 0;
			for c in 0..local_connectivity.len() {
				if y >= side_length {
					x += 2;
					y = 0;
				}
				initial3[c] = (x, y);
				y += 1;
			}
		}
		if local_connectivity.len() > 100 {
			retval.push(vec![(0, 0); local_connectivity.len()]);
			let initial = retval.last_mut().unwrap();
			let mut x = 0;
			let mut y = 0;
			let mut ydir = false;
			for c in Self::get_bfs_order(0, local_connectivity) {
				initial[c] = (x, y as usize);
				x += 2;
				if (x / 2) % 3 == 0 {
					x -= 4;
					if y == 0 && ydir || y == side_length - 1 && !ydir {
						ydir = !ydir;
					}
					if ydir {
						y -= 1;
					} else {
						y += 1;
					}
				}
			}
		}
		retval
	}

	fn place_local(
		&mut self,
		logical: &LogicalDesign,
		partition_local_connectivity: &Vec<Vec<Vec<usize>>>,
		local_to_global: &Vec<Vec<usize>>,
	) {
		for partition in 0..self.n_partitions {
			println!(
				"Partition local connectivity: {:?}",
				partition_local_connectivity[partition as usize]
			);
			let initializations =
				Self::get_initializations(&partition_local_connectivity[partition as usize]);
			self.space[partition as usize] = Arr2::new([
				self.side_length_single_partition,
				self.side_length_single_partition,
			]);
			let comb_positions = match Self::solve_as_mcmc_dense(
				&partition_local_connectivity[partition as usize],
				&initializations,
				None,
				self.side_length_single_partition,
			) {
				Ok(pos) => pos,
				Err(e) => {
					println!("WARN: MCMC failed to place with scale 1.5");
					println!("WARN: {}", e.0);
					let _ =
						self.place_combs_physical_dense(&e.1, logical, partition, local_to_global);
					self.save_svg(logical, format!("./svg/failed{}.svg", 1.5).as_str());
					panic!("failed to place");
				}
			};
			if let Err(_) = self.place_combs_physical_dense(
				&comb_positions,
				logical,
				partition,
				local_to_global,
			) {
				println!("Failed to place all cells. Here was the assignments:");
				for (idx, p) in comb_positions.iter().enumerate() {
					println!("{idx}: {:?}", p);
				}
				for (i, p_i) in comb_positions.iter().enumerate() {
					for (j, p_j) in comb_positions.iter().enumerate() {
						if j <= i {
							continue;
						}
						if p_i == p_j {
							println!("Found that {i} has the same assignment as {j}");
						}
					}
				}
				panic!("Have to bail due to failure in placement");
			}
			self.local_assignments[partition as usize] = comb_positions;
		}
	}

	fn place_combs_physical_dense(
		&mut self,
		comb_positions: &Vec<(usize, usize)>,
		logical: &LogicalDesign,
		partition: i32,
		local_to_global: &Vec<Vec<usize>>,
	) -> Result<(), String> {
		let mut failure = Ok(());
		for (idx, (x, y)) in comb_positions.iter().enumerate() {
			let p = (*x as f64, *y as f64);
			let res2 = self.place_comb_physical(
				p,
				PhyId(local_to_global[partition as usize][idx]),
				logical,
				Some(partition),
			);
			match res2.clone() {
				Ok(_) => {}
				Err(e) => {
					println!("Failed to place cell {idx} at position {:?}. {}", p, e);
				}
			}
			if failure.is_ok() {
				failure = res2;
			}
		}
		failure
	}

	pub fn solve_as_mcmc_dense(
		connections_per_node: &Vec<Vec<usize>>,
		initializations: &Vec<Vec<(usize, usize)>>,
		init_temp: Option<f64>,
		side_length: usize,
	) -> Result<Vec<(usize, usize)>, (String, Vec<(usize, usize)>)> {
		let num_cells = connections_per_node.len();

		let connections = adjacency_to_edges(connections_per_node, true);

		let mut rng = StdRng::seed_from_u64(0xCAFEBABE);
		let mut curr = Placement::from_initialization(vec![(0, 0); num_cells], side_length);

		let mut max_density = 15;

		if initializations.is_empty() {
			for cell in 0..num_cells {
				loop {
					let assignment = (
						rng.random_range(0..side_length) / 2 * 2,
						rng.random_range(0..side_length),
					);
					if curr.density(assignment) < max_density {
						curr.mov(cell, assignment);
						break;
					}
				}
			}
		} else {
			let mut min_score = (f64::INFINITY, false, i32::MAX);
			let mut min_idx = 0;
			for (idx, init) in initializations.iter().enumerate() {
				let mut initplc = curr.clone();
				for (idx, assignment) in init.iter().enumerate() {
					initplc.mov(idx, *assignment);
				}
				let cost = initplc.compute_cost(&connections, max_density);
				if min_score.1 && min_score.2 == 0 {
					if cost.1 && cost.2 == 0 && cost.0 < min_score.0 {
						min_score = cost;
						min_idx = idx;
					}
				} else if cost.0 < min_score.0 {
					min_score = cost;
					min_idx = idx;
				}
			}
			println!("Selecting initialization number {min_idx}");
			curr = Placement::from_initialization(initializations[min_idx].clone(), side_length);
		}

		let mut best = curr.clone();
		let mut best_cost = best.compute_cost(&connections, max_density);
		let mut assignments_cost = best_cost;

		let mut temp = init_temp.unwrap_or(20.0 + 5.0 * (num_cells as f64));
		let cooling_rate = 1.0 - 1E-7;
		let mut iterations = 20_000_000;

		let check_invariant_density = |plc: &Placement, max_density: i32| {
			for x in 0..plc.side_length {
				for y in 0..plc.side_length {
					let count = plc.density((x, y));
					if count > max_density {
						assert!(false);
					}
				}
			}
		};
		let check_invariant_unique_position = |plc: &Placement| {
			let mut seen = HashMap::new();
			for (idx, (x, y)) in plc.assignments.iter().enumerate() {
				if let Some(idx_prior) = seen.insert((x, y), idx) {
					assert!(false);
				}
				assert!(x % 2 == 0);
			}
		};
		let check_invariant_position = |plc: &Placement| {
			for x in 0..plc.side_length {
				for y in 0..plc.side_length {
					let count = plc.density((x, y));
					if count > 0 && x % 2 == 1 {
						assert!(false);
					}
				}
			}
			for (x, _) in plc.assignments.iter() {
				assert!(x % 2 == 0);
			}
		};
		let check_invariant_blocks_assignments_congruent = |plc: &Placement| {
			let mut block_counts_expected = vec![vec![0; side_length]; side_length];
			for (x, y) in plc.assignments.iter() {
				block_counts_expected[*x][*y] += 1;
			}
			for x in 0..plc.side_length {
				for y in 0..plc.side_length {
					let count = plc.density((x, y));
					assert_eq!(block_counts_expected[x][y], count);
				}
			}
		};
		let check_invariants = |plc: &Placement, max_density: i32| {
			check_invariant_density(plc, max_density);
			check_invariant_position(plc);
			check_invariant_blocks_assignments_congruent(plc);
		};

		let mut new_best = false;
		let mut stage_2 = false;
		let mut final_stage = false;
		let mut step = 0;
		let mut trend_of_bests = vec![];
		while step < iterations {
			if best_cost.1 && !final_stage {
				check_invariants(&curr, 1);
				final_stage = true;
				if (step as f64 / iterations as f64) < 0.05 {
					iterations = (step as f64 * 4.0) as i32;
				} else {
					iterations = (step as f64 * 1.3) as i32;
				}

				println!("Entering final compacting stage.");
				if best_cost.0 <= 0.0 {
					break;
				}
			}

			if !stage_2 && best_cost.2 * 100 < num_cells as i32 && max_density <= 1 {
				//stage_2 = true;
				//curr = best.clone();
				//assignments_cost = best_cost;
				//println!("Entering stage 2");
			}

			let mut new = curr.clone();

			type METHOD = (
				usize, //weight
				bool,  //final_stage
				bool,  // After best found
				fn(
					rng: &mut StdRng,
					curr: &Placement,
					new: &mut Placement,
					connections_per_node: &Vec<Vec<usize>>,
					side_length: usize,
					max_density: i32,
				),
				&'static str,
			);
			macro_rules! mthd {
				($a: expr, $b: expr, $c: expr, $d: expr) => {
					($a, $b, $c, $d, stringify!($d))
				};
			}
			const METHODS: &[METHOD] = &[
				mthd!(650, false, false, ripup_replace_method),
				mthd!(200, true, false, swap_local_method),
				//(15, false, false, swap_random_method,),
				mthd!(25, false, false, ripup_range_method),
				mthd!(200, false, false, crack_in_two_method),
				mthd!(50, true, false, slide_puzzle_method),
				mthd!(100, false, false, slide_puzzle_method_worst_cells),
				mthd!(100, false, false, overflowing_cells_swap_local_method),
				mthd!(10, false, true, simulated_spring_method),
				mthd!(40, false, true, slide_puzzle_method_on_violations),
				mthd!(10, false, true, swap_random_energy_method),
				mthd!(100, true, true, swap_local_energy_method),
			];
			const METHODS_2: &[METHOD] = &[
				mthd!(100, false, false, ripup_replace_method),
				mthd!(100, true, false, swap_local_method),
				mthd!(100, false, false, swap_random_method),
				mthd!(100, false, false, ripup_range_method),
				mthd!(100, false, false, crack_in_two_method),
				mthd!(100, true, false, slide_puzzle_method),
				mthd!(100, false, false, slide_puzzle_method_worst_cells),
				mthd!(100, false, false, overflowing_cells_swap_local_method),
				mthd!(100, false, true, simulated_spring_method),
				mthd!(1000, false, true, slide_puzzle_method_on_violations),
				mthd!(10, true, true, swap_local_energy_method),
			];

			// Select weighted method
			{
				let total_weight: usize = if stage_2 && !final_stage {
					METHODS_2
				} else {
					METHODS
				}
				.iter()
				.filter(|(_, can_run_if_final, after_best_found, _, _)| {
					if final_stage {
						*can_run_if_final
					} else if new_best {
						*after_best_found
					} else {
						true
					}
				})
				.map(|(weight, _, _, _, _)| *weight)
				.sum();

				let pick = rng.random_range(0..total_weight);
				let mut cumulative = 0;
				for (weight, can_run_if_final, _, func, method_name) in METHODS {
					if final_stage && !can_run_if_final {
						continue;
					}
					cumulative += weight;
					if pick < cumulative {
						func(
							&mut rng,
							&curr,
							&mut new,
							&connections_per_node,
							side_length,
							max_density,
						);
						check_invariant_blocks_assignments_congruent(&new);
						// This invariant should always be true, if not then a step has placed a node into an odd x block and is incorrect.
						check_invariant_position(&new);
						break;
					}
				}
			}
			new_best = false;

			let new_cost = new.compute_cost(&connections, max_density);
			let delta = new_cost.0 - best_cost.0;

			if new_cost.2 <= 0 && max_density > 1 {
				max_density -= 1;
				println!("Reducing max density to {max_density}");
			}

			if step % 3000 == 0 {
				println!(
					"Current cost {} ({}), temp {}, step {step}/{iterations}",
					assignments_cost.0, assignments_cost.2, temp
				);
				let mut graph = SVG::new();
				graph.make_chart(
					trend_of_bests
						.iter()
						.map(|b: &(f64, bool, i32)| b.2)
						.collect_vec(),
					"Sat count over time",
					"Number",
					"Count",
				);
				graph.save("svg/sat_count.svg").unwrap();
				let mut graph = SVG::new();
				graph.make_chart(
					trend_of_bests
						.iter()
						.map(|b: &(f64, bool, i32)| b.0 as i32)
						.collect_vec(),
					"Sat count over time",
					"Number",
					"Count",
				);
				graph.save("svg/cost.svg").unwrap();
			}

			if !final_stage && (delta < 0.0 || new_cost.1)
				|| final_stage && (delta < 0.0 && new_cost.1)
			{
				//best_cost = new.compute_cost(&connections, max_density);
				new.draw_placement(&connections, max_density, "svg/best.svg");
				best = new.clone();
				curr = new;
				best_cost = new_cost;
				//assignments_cost = new_cost;
				assignments_cost = best_cost;
				println!("Best cost {} ({}), temp {}", best_cost.0, best_cost.2, temp);
				trend_of_bests.push(best_cost);
				if !final_stage && iterations - step < iterations / 10 {
					println!("Boosting iterations {}", best_cost.0);
					iterations = iterations * 25 / 20;
					temp *= 1.12;
				}

				if final_stage {
					check_invariant_unique_position(&best);
				}
			} else {
				let p = f64::exp(-delta / temp);
				if p > 1.0 || rng.random_bool(p) {
					curr = new;
					assignments_cost = new_cost;
				}
			}

			if assignments_cost.0 / best_cost.0 > 2.0 {
				curr = best.clone();
				assignments_cost = best_cost;
			}

			temp *= cooling_rate;
			if temp < 1e-3 {
				temp = 1e-3;
			}
			step += 1;
		}

		//check_invariants(&block_counts, max_density);

		if !best_cost.1 {
			return Err((
				format!(
					"Could not satisfy distance requirements, lowest cost: {}",
					best_cost.0
				),
				best.assignments,
			));
		}

		Ok(best.assignments)
	}

	pub fn solve_as_mcmc_global(
		connections_per_node: &Vec<Vec<usize>>,
		initializations: &Vec<Vec<(usize, usize)>>,
		init_temp: Option<f64>,
		side_length: usize,
		cell_size: usize,
	) -> Result<Vec<(usize, usize)>, (String, Vec<(usize, usize)>)> {
		let num_cells = connections_per_node.len();

		let connections = adjacency_to_edges(connections_per_node, true);

		let mut rng = StdRng::seed_from_u64(0xCAFEBABE);
		let mut curr =
			GlobalPlacement::from_initialization(vec![(0, 0); num_cells], side_length, cell_size);

		let mut max_density = 15;

		if initializations.is_empty() {
			for cell in 0..num_cells {
				loop {
					let assignment = (
						rng.random_range(0..side_length) / 2 * 2,
						rng.random_range(0..side_length),
					);
					if curr.density(assignment) < max_density {
						curr.mov(cell, assignment);
						break;
					}
				}
			}
		} else {
			let mut min_score = (f64::INFINITY, false, i32::MAX);
			let mut min_idx = 0;
			for (idx, init) in initializations.iter().enumerate() {
				let mut initplc = curr.clone();
				for (idx, assignment) in init.iter().enumerate() {
					initplc.mov(idx, *assignment);
				}
				let cost = initplc.compute_cost(&connections, max_density);
				if min_score.1 && min_score.2 == 0 {
					if cost.1 && cost.2 == 0 && cost.0 < min_score.0 {
						min_score = cost;
						min_idx = idx;
					}
				} else if cost.0 < min_score.0 {
					min_score = cost;
					min_idx = idx;
				}
			}
			println!("Selecting initialization number {min_idx}");
			curr = GlobalPlacement::from_initialization(
				initializations[min_idx].clone(),
				side_length,
				cell_size,
			);
		}

		let mut best = curr.clone();
		let mut best_cost = best.compute_cost(&connections, max_density);
		let mut assignments_cost = best_cost;

		let mut temp = init_temp.unwrap_or(20.0 + 5.0 * (num_cells as f64));
		let cooling_rate = 1.0 - 1E-7;
		let mut iterations = 20_000_000;

		let check_invariant_density = |plc: &GlobalPlacement, max_density: i32| {
			for x in 0..plc.side_length {
				for y in 0..plc.side_length {
					let count = plc.density((x, y));
					if count > max_density {
						assert!(false);
					}
				}
			}
		};
		let check_invariant_unique_position = |plc: &GlobalPlacement| {
			let mut seen = HashMap::new();
			for (idx, (x, y)) in plc.assignments.iter().enumerate() {
				if let Some(idx_prior) = seen.insert((x, y), idx) {
					assert!(false);
				}
				assert!(x % 2 == 0);
			}
		};
		let check_invariant_position = |plc: &GlobalPlacement| {
			for x in 0..plc.side_length {
				for y in 0..plc.side_length {
					let count = plc.density((x, y));
					if count > 0 && x % 2 == 1 {
						assert!(false);
					}
				}
			}
			for (x, _) in plc.assignments.iter() {
				assert!(x % 2 == 0);
			}
		};
		let check_invariant_blocks_assignments_congruent = |plc: &GlobalPlacement| {
			let mut block_counts_expected = vec![vec![0; side_length]; side_length];
			for (x, y) in plc.assignments.iter() {
				block_counts_expected[*x][*y] += 1;
			}
			for x in 0..plc.side_length {
				for y in 0..plc.side_length {
					let count = plc.density((x, y));
					assert_eq!(block_counts_expected[x][y], count);
				}
			}
		};
		let check_invariants = |plc: &GlobalPlacement, max_density: i32| {
			check_invariant_density(plc, max_density);
			check_invariant_position(plc);
			check_invariant_blocks_assignments_congruent(plc);
		};

		let mut new_best = false;
		let mut stage_2 = false;
		let mut final_stage = false;
		let mut step = 0;
		let mut trend_of_bests = vec![];
		while step < iterations {
			if best_cost.1 && !final_stage {
				check_invariants(&curr, 1);
				final_stage = true;
				if (step as f64 / iterations as f64) < 0.05 {
					iterations = (step as f64 * 4.0) as i32;
				} else {
					iterations = (step as f64 * 1.3) as i32;
				}

				println!("Entering final compacting stage.");
				if best_cost.0 <= 0.0 {
					break;
				}
			}

			if !stage_2 && best_cost.2 * 100 < num_cells as i32 && max_density <= 1 {
				//stage_2 = true;
				//curr = best.clone();
				//assignments_cost = best_cost;
				//println!("Entering stage 2");
			}

			let mut new = curr.clone();

			type METHOD = (
				usize, //weight
				bool,  //final_stage
				bool,  // After best found
				fn(
					rng: &mut StdRng,
					curr: &GlobalPlacement,
					new: &mut GlobalPlacement,
					connections_per_node: &Vec<Vec<usize>>,
					side_length: usize,
					max_density: i32,
				),
				&'static str,
			);
			macro_rules! mthd {
				($a: expr, $b: expr, $c: expr, $d: expr) => {
					($a, $b, $c, $d, stringify!($d))
				};
			}
			use global as glb;
			const METHODS: &[METHOD] = &[
				mthd!(650, false, false, glb::ripup_replace_method),
				mthd!(200, true, false, glb::swap_local_method),
				//(15, false, false, glb::swap_random_method,),
				mthd!(25, false, false, glb::ripup_range_method),
				mthd!(200, false, false, glb::crack_in_two_method),
				mthd!(50, true, false, glb::slide_puzzle_method),
				mthd!(100, false, false, glb::slide_puzzle_method_worst_cells),
				mthd!(100, false, false, glb::overflowing_cells_swap_local_method),
				mthd!(10, false, true, glb::simulated_spring_method),
				//mthd!(40, false, true, global::slide_puzzle_method_on_violations),
				mthd!(10, false, true, glb::swap_random_energy_method),
				mthd!(100, true, true, glb::swap_local_energy_method),
			];
			const METHODS_2: &[METHOD] = &[
				mthd!(100, false, false, glb::ripup_replace_method),
				mthd!(100, true, false, glb::swap_local_method),
				mthd!(100, false, false, glb::swap_random_method),
				mthd!(100, false, false, glb::ripup_range_method),
				mthd!(100, false, false, glb::crack_in_two_method),
				mthd!(100, true, false, glb::slide_puzzle_method),
				mthd!(100, false, false, glb::slide_puzzle_method_worst_cells),
				mthd!(100, false, false, glb::overflowing_cells_swap_local_method),
				mthd!(100, false, true, glb::simulated_spring_method),
				//mthd!(1000, false, true, glb::slide_puzzle_method_on_violations),
				mthd!(10, true, true, glb::swap_local_energy_method),
			];

			// Select weighted method
			{
				let total_weight: usize = if stage_2 && !final_stage {
					METHODS_2
				} else {
					METHODS
				}
				.iter()
				.filter(|(_, can_run_if_final, after_best_found, _, _)| {
					if final_stage {
						*can_run_if_final
					} else if new_best {
						*after_best_found
					} else {
						true
					}
				})
				.map(|(weight, _, _, _, _)| *weight)
				.sum();

				let pick = rng.random_range(0..total_weight);
				let mut cumulative = 0;
				for (weight, can_run_if_final, _, func, method_name) in METHODS {
					if final_stage && !can_run_if_final {
						continue;
					}
					cumulative += weight;
					if pick < cumulative {
						func(
							&mut rng,
							&curr,
							&mut new,
							&connections_per_node,
							side_length,
							max_density,
						);
						check_invariant_blocks_assignments_congruent(&new);
						// This invariant should always be true, if not then a step has placed a node into an odd x block and is incorrect.
						check_invariant_position(&new);
						break;
					}
				}
			}
			new_best = false;

			let new_cost = new.compute_cost(&connections, max_density);
			let delta = new_cost.0 - best_cost.0;

			if new_cost.2 <= 0 && max_density > 1 {
				max_density -= 1;
				println!("Reducing max density to {max_density}");
			}

			if step % 3000 == 0 {
				println!(
					"Current cost {} ({}), temp {}, step {step}/{iterations}",
					assignments_cost.0, assignments_cost.2, temp
				);
				let mut graph = SVG::new();
				graph.make_chart(
					trend_of_bests
						.iter()
						.map(|b: &(f64, bool, i32)| b.2)
						.collect_vec(),
					"Sat count over time",
					"Number",
					"Count",
				);
				graph.save("svg/sat_count.svg").unwrap();
				let mut graph = SVG::new();
				graph.make_chart(
					trend_of_bests
						.iter()
						.map(|b: &(f64, bool, i32)| b.0 as i32)
						.collect_vec(),
					"Sat count over time",
					"Number",
					"Count",
				);
				graph.save("svg/cost.svg").unwrap();
			}

			if !final_stage && (delta < 0.0 || new_cost.1)
				|| final_stage && (delta < 0.0 && new_cost.1)
			{
				new.draw_placement(&connections, max_density)
					.save("svg/global_placement.svg")
					.unwrap();
				best = new.clone();
				curr = new;
				best_cost = new_cost;
				assignments_cost = best_cost;
				println!("Best cost {} ({}), temp {}", best_cost.0, best_cost.2, temp);
				trend_of_bests.push(best_cost);
				if !final_stage && iterations - step < iterations / 10 {
					println!("Boosting iterations {}", best_cost.0);
					iterations = iterations * 25 / 20;
					temp *= 1.12;
				}

				if final_stage {
					check_invariant_unique_position(&best);
				}
			} else {
				let p = f64::exp(-delta / temp);
				if p > 1.0 || rng.random_bool(p) {
					curr = new;
					assignments_cost = new_cost;
				}
			}

			if assignments_cost.0 / best_cost.0 > 2.0 {
				curr = best.clone();
				assignments_cost = best_cost;
			}

			temp *= cooling_rate;
			if temp < 1e-3 {
				temp = 1e-3;
			}
			step += 1;
		}

		//check_invariants(&block_counts, max_density);

		if !best_cost.1 {
			return Err((
				format!(
					"Could not satisfy distance requirements, lowest cost: {}",
					best_cost.0
				),
				best.assignments,
			));
		}

		Ok(best.assignments)
	}

	pub fn max_allowable_distance(
		&self,
		logical: &LogicalDesign,
		id_comb_a: PhyId,
		id_comb_b: PhyId,
	) -> f64 {
		let hop_spec_i = self
			.get_logical(id_comb_a, logical)
			.function
			.wire_hop_type()
			.wire_hop_spec();
		let hop_spec_j = self
			.get_logical(id_comb_b, logical)
			.function
			.wire_hop_type()
			.wire_hop_spec();
		f64::min(hop_spec_i.reach, hop_spec_j.reach)
	}

	fn reset_place_route(&mut self) {
		for p in 0..self.n_partitions {
			self.space[p as usize] =
				Arr2::new([self.side_length_partitions, self.side_length_partitions]);
		}
		for c in &mut self.nodes {
			c.placed = false;
		}
		self.wires.clear();
	}

	fn place_comb_physical(
		&mut self,
		position: (f64, f64),
		id_to_place: PhyId,
		logical: &LogicalDesign,
		partition_opt: Option<i32>,
	) -> Result<(), String> {
		let pos_start = (position.0 as usize, position.1 as usize);
		let comb_to_place = &mut self.nodes[id_to_place.0];
		let ld_node = logical.get_node(comb_to_place.logic);
		let hop_spec = ld_node.function.wire_hop_type().wire_hop_spec();
		for x in 0..hop_spec.dim.0 as usize {
			for y in 0..hop_spec.dim.1 as usize {
				let key = (pos_start.0 + x, pos_start.1 + y);
				if let Some(partition) = partition_opt {
					if !self.space[partition as usize].index_good(key) {
						return Result::Err(format!("Sub-cell ({x}, {y}) is OOB."));
					}
					if self.space[partition as usize][key] != PhyId::default() {
						return Result::Err(format!(
							"Sub-cell ({x}, {y}) overlaps with an existing cell."
						));
					}
				} else {
					if !self.global_space.index_good(key) {
						return Result::Err(format!("Sub-cell ({x}, {y}) is OOB."));
					}
					if self.global_space[key] != PhyId::default() {
						return Result::Err(format!(
							"Sub-cell ({x}, {y}) overlaps with an existing cell."
						));
					}
				}
			}
		}
		for x in 0..hop_spec.dim.0 as usize {
			for y in 0..hop_spec.dim.1 as usize {
				let key = (pos_start.0 + x, pos_start.1 + y);
				if let Some(partition) = partition_opt {
					self.space[partition as usize][key] = id_to_place;
				} else {
					self.global_space[key] = id_to_place;
				}
			}
		}
		comb_to_place.position = (
			position.0 + hop_spec.dim.0 as f64 / 2.0,
			position.1 + hop_spec.dim.1 as f64 / 2.0,
		);
		comb_to_place.placed = true;
		Result::Ok(())
	}

	fn connect_combs(&mut self, logical: &LogicalDesign) {
		let mut global_edges: Vec<(ld::NodeId, Vec<(bool, ld::NodeId, bool, ld::NodeId)>)> = vec![];
		logical.for_all(|_, ld_node| {
			match &ld_node.function {
				ld::NodeFunction::WireSum(c) => {
					let mut edges = vec![];
					for id_i in ld_node.iter_fanout(*c) {
						for id_j in ld_node.iter_fanin(*c) {
							let comb_i = &self.nodes[self.idx_combs.get(id_i).unwrap().0];
							let comb_j = &self.nodes[self.idx_combs.get(id_j).unwrap().0];
							let ld_node_i = logical.get_node(comb_i.logic);
							let ld_node_j = logical.get_node(comb_j.logic);
							let hop_spec_i = ld_node_i.function.wire_hop_type().wire_hop_spec();
							let hop_spec_j = ld_node_j.function.wire_hop_type().wire_hop_spec();
							let min_distance = f64::min(hop_spec_i.reach, hop_spec_j.reach);
							if euclidean_distance_squared_f64_pair(comb_i.position, comb_j.position)
								< min_distance * min_distance
							{
								edges.push((false, *id_j, true, *id_i));
							}
						}
					}
					if ld_node.fanout_empty() {
						for id_i in ld_node.iter_fanin(*c) {
							for id_j in ld_node.iter_fanin(*c) {
								if id_i == id_j {
									continue;
								}
								let comb_i = &self.nodes[self.idx_combs.get(id_i).unwrap().0];
								let comb_j = &self.nodes[self.idx_combs.get(id_j).unwrap().0];
								let ld_node_i = logical.get_node(comb_i.logic);
								let ld_node_j = logical.get_node(comb_j.logic);
								let hop_spec_i = ld_node_i.function.wire_hop_type().wire_hop_spec();
								let hop_spec_j = ld_node_j.function.wire_hop_type().wire_hop_spec();
								let min_distance = f64::min(hop_spec_i.reach, hop_spec_j.reach);
								if euclidean_distance_squared_f64_pair(
									comb_i.position,
									comb_j.position,
								) < min_distance * min_distance
								{
									edges.push((false, *id_j, false, *id_i));
								}
							}
						}
					}
					if ld_node.fanin_empty() {
						for id_i in ld_node.iter_fanout(*c) {
							for id_j in ld_node.iter_fanout(*c) {
								if id_i == id_j {
									continue;
								}
								let comb_i = &self.nodes[self.idx_combs.get(id_i).unwrap().0];
								let comb_j = &self.nodes[self.idx_combs.get(id_j).unwrap().0];
								let ld_node_i = logical.get_node(comb_i.logic);
								let ld_node_j = logical.get_node(comb_j.logic);
								let hop_spec_i = ld_node_i.function.wire_hop_type().wire_hop_spec();
								let hop_spec_j = ld_node_j.function.wire_hop_type().wire_hop_spec();
								let min_distance = f64::min(hop_spec_i.reach, hop_spec_j.reach);
								if euclidean_distance_squared_f64_pair(
									comb_i.position,
									comb_j.position,
								) < min_distance * min_distance
								{
									edges.push((true, *id_j, true, *id_i));
								}
							}
						}
					}
					global_edges.push((ld_node.id, remove_non_unique(edges)));
				}
				_ => { /* Do nothing for non-wires */ }
			}
		});

		// Connect through routes.
		let mut route_edges = vec![];
		for (id, node) in self.nodes.iter().enumerate() {
			if node.is_pole {
				break;
			}
			for route in &node.routes {
				route_edges.push((
					route.input_sided,
					PhyId(id),
					route.near_side_pole_id,
					WireColour::Red,
				));
				route_edges.push((
					route.input_sided,
					PhyId(id),
					route.near_side_pole_id,
					WireColour::Green,
				));
			}
			// Naming is from perspective of the foreign node.
			let far_side_ld_id = node.logic;
			let far_side_id = PhyId(id);
			for near_side_id in &node.foreign_routes {
				let near_ld_id = self.nodes[near_side_id.0].logic;
				for near_route in &self.nodes[near_side_id.0].routes {
					if near_route.far_side_id != far_side_id {
						continue;
					}
					for (colour, far_is_input_sided) in [WireColour::Red, WireColour::Green]
						.iter()
						.cartesian_product([false, true])
					{
						if !logical.have_shared_wire_detailed(
							far_side_ld_id,
							near_ld_id,
							*colour,
							far_is_input_sided,
							near_route.input_sided,
						) {
							continue;
						}
						route_edges.push((
							far_is_input_sided,
							near_route.far_side_id,
							near_route.far_side_pole_id,
							*colour,
						));
					}
				}
			}
		}
		for wire in route_edges {
			self.connect_wire_to_pole(wire.0, wire.1, wire.2, logical, wire.3);
		}
		for wires in global_edges {
			for edge in wires.1 {
				self.connect_wire(
					wires.0,
					edge.0,
					*self.idx_combs.get(&edge.1).unwrap(),
					edge.2,
					*self.idx_combs.get(&edge.3).unwrap(),
					logical,
				);
			}
		}
		#[cfg(debug_assertions)]
		println!("Wires {:?}", self.connected_wires);
	}

	fn connect_wire_to_pole(
		&mut self,
		fanin_a: bool,
		id_comb_a: PhyId,
		id_pole: PhyId,
		logical: &LogicalDesign,
		colour: WireColour,
	) {
		let id_wire = self.wires.len();
		let comb_a = &self.nodes[id_comb_a.0];
		let ld_comb_a = logical.get_node(comb_a.logic);
		let term_a = match &ld_comb_a.function {
			ld::NodeFunction::Arithmetic { .. } | ld::NodeFunction::Decider { .. } => {
				if fanin_a {
					TerminalId::input(colour)
				} else {
					TerminalId::output_combinator(colour)
				}
			}
			ld::NodeFunction::Constant { .. } | ld::NodeFunction::Lamp { .. } => {
				TerminalId::output_constant(colour)
			}
			ld::NodeFunction::WireSum(_c) => unreachable!(),
		};
		self.connected_wires[id_comb_a.0].push(WireId(id_wire));
		self.connected_wires[id_pole.0].push(WireId(id_wire));
		self.wires.push(Wire {
			id: WireId(id_wire),
			node1_id: id_comb_a,
			node2_id: id_pole,
			terminal1_id: term_a,
			terminal2_id: TerminalId::input(colour), // TODO: Check in game what poles actually do!
		});
	}

	fn connect_wire(
		&mut self,
		ld_id_wire: ld::NodeId,
		fanin_a: bool,
		id_comb_a: PhyId,
		fanin_b: bool,
		id_comb_b: PhyId,
		logical: &LogicalDesign,
	) {
		let id_wire = self.wires.len();
		let comb_a = &self.nodes[id_comb_a.0];
		let comb_b = &self.nodes[id_comb_b.0];
		let ld_comb_a = logical.get_node(comb_a.logic);
		let ld_comb_b = logical.get_node(comb_b.logic);
		let color = if let ld::NodeFunction::WireSum(color) = logical.get_node(ld_id_wire).function
		{
			match color {
				WireColour::Red => assert!(
					ld_comb_a.fanout_red.contains(&ld_id_wire)
						&& ld_comb_b.fanin_red.contains(&ld_id_wire)
						|| ld_comb_a.fanin_red.contains(&ld_id_wire)
							&& ld_comb_b.fanin_red.contains(&ld_id_wire)
						|| ld_comb_a.fanout_red.contains(&ld_id_wire)
							&& ld_comb_b.fanout_red.contains(&ld_id_wire)
				),
				WireColour::Green => assert!(
					ld_comb_a.fanout_green.contains(&ld_id_wire)
						&& ld_comb_b.fanin_green.contains(&ld_id_wire)
						|| ld_comb_a.fanin_green.contains(&ld_id_wire)
							&& ld_comb_b.fanin_green.contains(&ld_id_wire)
						|| ld_comb_a.fanout_green.contains(&ld_id_wire)
							&& ld_comb_b.fanout_green.contains(&ld_id_wire)
				),
			}
			color
		} else {
			panic!("Tried to connect two combinators over a non-wire node. All combinators should have a wire between eachother.");
		};
		let term_a = match &ld_comb_a.function {
			ld::NodeFunction::Arithmetic { .. } | ld::NodeFunction::Decider { .. } => {
				if fanin_a {
					TerminalId::input(color)
				} else {
					TerminalId::output_combinator(color)
				}
			}
			ld::NodeFunction::Constant { .. } | ld::NodeFunction::Lamp { .. } => {
				TerminalId::output_constant(color)
			}
			ld::NodeFunction::WireSum(_c) => unreachable!(),
		};
		let term_b = match &ld_comb_b.function {
			ld::NodeFunction::Arithmetic { .. } | ld::NodeFunction::Decider { .. } => {
				if fanin_b {
					TerminalId::input(color)
				} else {
					TerminalId::output_combinator(color)
				}
			}
			ld::NodeFunction::Constant { .. } | ld::NodeFunction::Lamp { .. } => {
				TerminalId::input(color)
			}
			ld::NodeFunction::WireSum(_c) => unreachable!(),
		};
		self.connected_wires[id_comb_a.0].push(WireId(id_wire));
		self.connected_wires[id_comb_b.0].push(WireId(id_wire));
		self.wires.push(Wire {
			id: WireId(id_wire),
			node1_id: id_comb_a,
			node2_id: id_comb_b,
			terminal1_id: term_a,
			terminal2_id: term_b,
		});
	}

	fn save_svg(&self, ld: &LogicalDesign, filename: &str) {
		const SCALE: f64 = 20.0;
		const GREY: (u8, u8, u8) = (230, 230, 230);
		let mut svg = SVG::new();
		svg.add_rect(
			0,
			0,
			SCALE as i32 * self.global_space.dims().0 as i32,
			SCALE as i32 * self.global_space.dims().1 as i32,
			(255, 255, 255),
			None,
			None,
		);
		let mut rects = vec![];
		for c in &self.nodes {
			let node = ld.get_node(c.logic);
			let mut x = (c.position.0 * (SCALE + 2.0)) as i32;
			let y = (c.position.1 * (SCALE + 2.0)) as i32;
			if c.is_pole {
				// Otter brown
				rects.push(svg.add_circle(x, y, SCALE as i32 / 4, (101, 67, 33), None, None));
				continue;
			}
			let (w, h, label, hover) = match &node.function {
				ld::NodeFunction::Arithmetic { op, .. } => {
					x -= SCALE as i32 / 2;
					(
						SCALE * 2.0,
						SCALE,
						Some(op.resolve_string()),
						node.description.clone(),
					)
				}
				ld::NodeFunction::Decider { expressions, .. } => {
					x -= SCALE as i32 / 2;
					if let Some(e) = expressions.first() {
						(
							SCALE * 2.0,
							SCALE,
							Some(e.1.resolve_string()),
							node.description.clone(),
						)
					} else {
						(SCALE * 2.0, SCALE, Some("D"), node.description.clone())
					}
				}
				ld::NodeFunction::Constant { .. } => {
					(SCALE, SCALE, Some("C"), node.description.clone())
				}
				ld::NodeFunction::Lamp { .. } => {
					(SCALE, SCALE, Some("L"), node.description.clone())
				}
				_ => {
					unreachable!()
				}
			};
			rects.push(svg.add_rect(
				x,
				y,
				w as i32,
				h as i32,
				GREY,
				label.map(|x| x.to_owned()),
				hover,
			));
		}
		for wire in &self.wires {
			svg.add_wire(
				wire.node1_id.0,
				wire.node2_id.0,
				wire.terminal1_id.0 - 1,
				wire.terminal2_id.0 - 1,
			);
		}
		svg.save(filename).unwrap()
	}

	fn validate_against(&self, ld: &LogicalDesign) {
		return;
		unreachable!();
		for c in &self.nodes {
			let ld_comb = ld.get_node(c.logic);
			if ld_comb.is_constant() {
				let n1 = ld.get_fanout_network(c.logic, WireColour::Red);
				let n2 = ld.get_fanout_network(c.logic, WireColour::Green);
				let n3 = self
					.get_network(c.id, TerminalId::output_constant(WireColour::Red))
					.iter()
					.map(|(coid, _)| self.nodes[coid.0].logic)
					.collect();
				let n4 = self
					.get_network(c.id, TerminalId::output_constant(WireColour::Green))
					.iter()
					.map(|(coid, _)| self.nodes[coid.0].logic)
					.collect();
				println!("\n\n{:?}", self.connected_wires);
				assert_eq!(n1, n3);
				assert_eq!(n2, n4);
			} else {
				let n1 = ld.get_fanin_network(c.logic, WireColour::Red);
				let n2 = ld.get_fanin_network(c.logic, WireColour::Green);
				let n3 = self
					.get_network(c.id, TerminalId::input(WireColour::Red))
					.iter()
					.map(|(coid, _)| self.nodes[coid.0].logic)
					.collect();
				let n4 = self
					.get_network(c.id, TerminalId::input(WireColour::Green))
					.iter()
					.map(|(coid, _)| self.nodes[coid.0].logic)
					.collect();
				assert_eq!(n1, n3);
				assert_eq!(n2, n4);
			}
			if ld_comb.is_arithmetic() || ld_comb.is_decider() {
				let n1 = ld.get_fanout_network(c.logic, WireColour::Red);
				let n2 = ld.get_fanout_network(c.logic, WireColour::Green);
				let n3 = self
					.get_network(c.id, TerminalId::output_combinator(WireColour::Red))
					.iter()
					.map(|(coid, _)| self.nodes[coid.0].logic)
					.collect();
				let n4 = self
					.get_network(c.id, TerminalId::output_combinator(WireColour::Green))
					.iter()
					.map(|(coid, _)| self.nodes[coid.0].logic)
					.collect();
				assert_eq!(n1, n3);
				assert_eq!(n2, n4);
			}
		}
	}

	pub(crate) fn get_network(
		&self,
		coid: PhyId,
		terminal: TerminalId,
	) -> HashSet<(PhyId, TerminalId)> {
		let mut queue = LinkedList::new();
		let mut seen: HashSet<WireId> = HashSet::new();
		let mut retval = HashSet::new();
		let add_wires_on_terminal =
			|coid: PhyId, terminal: TerminalId, queue: &mut LinkedList<WireId>| {
				for wiid in &self.connected_wires[coid.0] {
					let wire = &self.wires[wiid.0];
					if wire.node1_id == coid && wire.terminal1_id == terminal
						|| wire.node2_id == coid && wire.terminal2_id == terminal
					{
						queue.push_back(*wiid);
					}
				}
			};
		add_wires_on_terminal(coid, terminal, &mut queue);
		while !queue.is_empty() {
			let wiid = queue.pop_front().unwrap();
			if seen.contains(&wiid) {
				continue;
			}
			seen.insert(wiid);
			let wire = &self.wires[wiid.0];
			add_wires_on_terminal(wire.node1_id, wire.terminal1_id, &mut queue);
			add_wires_on_terminal(wire.node2_id, wire.terminal2_id, &mut queue);
			retval.insert((wire.node1_id, wire.terminal1_id));
			retval.insert((wire.node2_id, wire.terminal2_id));
		}
		retval
	}

	fn get_bfs_order(coid: usize, connections: &Vec<Vec<usize>>) -> Vec<usize> {
		let mut queue = LinkedList::new();
		let mut seen_comb: HashS<usize> = hash_set();
		let mut retval = Vec::with_capacity(connections.len());
		queue.push_back(coid);
		retval.push(coid);
		while !queue.is_empty() {
			let coid = queue.pop_front().unwrap();
			if seen_comb.contains(&coid) {
				continue;
			}
			seen_comb.insert(coid);
			retval.push(coid);
			for coid_faninfanout in &connections[coid] {
				queue.push_back(*coid_faninfanout);
			}
		}
		retval
	}

	// Return two maps. First one local_to_global[partition][local_id] -> global_id.
	// Second one global_to_local[partition][global_id] -> local_id.
	// local_id : "the id in the partition"
	// global_id : "the id in the self.combs array"
	fn get_partition_id_mapping(&self) -> (Vec<Vec<usize>>, Vec<HashM<usize, usize>>) {
		let mut local_to_global = vec![vec![]; self.n_partitions as usize];
		let mut global_to_local = vec![util::hash_map(); self.n_partitions as usize];
		for cell in &self.nodes {
			let local_id = local_to_global[cell.partition as usize].len();
			local_to_global[cell.partition as usize].push(cell.id.0);
			global_to_local[cell.partition as usize].insert(cell.id.0, local_id);
		}
		(local_to_global, global_to_local)
	}

	pub fn get_connectivity_as_vec(&self, ld: &LogicalDesign) -> Vec<Vec<PhyId>> {
		let mut connections = vec![vec![]; self.nodes.len()];
		for cell in &self.nodes {
			for ldid in ld.get_connected_combs(cell.logic) {
				connections[cell.id.0].push(*self.idx_combs.get(&ldid).unwrap());
			}
		}
		connections
	}

	pub fn get_connectivity_as_vec_usize(&self, ld: &LogicalDesign) -> Vec<Vec<usize>> {
		let mut connections = vec![vec![]; self.nodes.len()];
		for cell in &self.nodes {
			for ldid in ld.get_connected_combs(cell.logic) {
				connections[cell.id.0].push(self.idx_combs.get(&ldid).unwrap().0);
			}
		}
		connections
	}

	pub fn split_connections_to_local_and_global(
		&self,
		global_connections: &Vec<Vec<usize>>,
	) -> (
		Vec<Vec<Vec<usize>>>,
		Vec<Vec<Vec<(i32, usize, usize)>>>,
		Vec<Vec<usize>>,
		Vec<HashM<usize, usize>>,
	) {
		let (local_to_global, global_to_local) = self.get_partition_id_mapping();
		let mut partitions_local_connections = vec![];
		for part in 0..self.n_partitions as usize {
			partitions_local_connections.push(vec![vec![]; local_to_global[part].len()]);
			let part_local_connections = partitions_local_connections.last_mut().unwrap();
			for global_id in &local_to_global[part] {
				let local_id = *global_to_local[part].get(global_id).unwrap();
				for other_global_id in &global_connections[*global_id] {
					let other_part = self.nodes[*other_global_id].partition;
					if part == other_part as usize {
						let other_local_id = *global_to_local[part].get(global_id).unwrap();
						part_local_connections[local_id].push(other_local_id);
					}
				}
			}
		}
		let mut partitions_global_connections = vec![];
		for part in 0..self.n_partitions as usize {
			partitions_global_connections.push(vec![vec![]; local_to_global[part].len()]);
			let global_connections_part = partitions_global_connections.last_mut().unwrap();
			for global_id in &local_to_global[part] {
				let local_id = *global_to_local[part].get(global_id).unwrap();
				for other_global_id in &global_connections[*global_id] {
					let other_part = self.nodes[*other_global_id].partition;
					if part != other_part as usize {
						let other_local_id = *global_to_local[other_part as usize]
							.get(other_global_id)
							.unwrap();
						global_connections_part[local_id].push((
							other_part,
							other_local_id,
							*other_global_id,
						));
					}
				}
			}
		}
		(
			partitions_local_connections,
			partitions_global_connections,
			local_to_global,
			global_to_local,
		)
	}

	pub fn get_connectivity_as_matrix(
		&self,
		ld: &LogicalDesign,
		triangular: bool,
	) -> Vec<Vec<bool>> {
		let mut connections = vec![vec![false; self.nodes.len()]; self.nodes.len()];
		let conn_list = self.get_connectivity_as_vec(ld);
		for (id_i, connected_i) in conn_list.iter().enumerate() {
			for id_j in connected_i {
				if triangular && id_j.0 < id_i {
					continue;
				}
				connections[id_i][id_j.0] = true;
			}
		}
		connections
	}

	pub fn get_connectivity_as_edges(
		&self,
		ld: &LogicalDesign,
		triangular: bool,
	) -> Vec<(usize, usize)> {
		let mut ret = vec![];
		let conn_list = self.get_connectivity_as_vec(ld);
		for (id_i, connected_j) in conn_list.iter().enumerate() {
			for id_j in connected_j {
				if triangular && id_j.0 < id_i {
					continue;
				}
				ret.push((id_i, id_j.0));
			}
		}
		ret
	}

	pub fn partition(&mut self, ld: &LogicalDesign, target_size: i32) {
		let connectivity = self.get_connectivity_as_vec_usize(ld);
		if target_size * 3 / 2 > connectivity.len() as i32 {
			self.space = vec![Arr2::new([0, 0]); self.n_partitions as usize];
			println!("Design small enough to skip partitioning.");
			return;
		}
		let n_parts = connectivity.len() as i32 / target_size;
		let (partition, n_cuts) = partition::metis(&connectivity, n_parts);
		partition::report_partition_quality(&partition, n_cuts, &connectivity, n_parts);
		self.n_edges_cut = n_cuts;
		self.n_partitions = n_parts;
		for (id, p) in partition.iter().enumerate() {
			assert!(*p >= 0, "");
			self.nodes[id].partition = *p;
		}
		self.local_assignments = vec![vec![]; n_parts as usize];
		let side_length = (n_parts as f64).sqrt().ceil() as usize;
		self.partition_assignments = vec![];

		for x in 0..side_length {
			for y in 0..side_length {
				self.partition_assignments.push((x, y));
			}
		}
		self.partition_assignments = self
			.partition_assignments
			.iter()
			.take(n_parts as usize)
			.cloned()
			.collect_vec();
		self.space = vec![Arr2::new([0, 0]); self.n_partitions as usize];
	}

	pub fn place_global(
		&mut self,
		partitions_global_connectivity: &Vec<Vec<Vec<(i32, usize, usize)>>>,
	) {
		assert_eq!(
			self.n_partitions as usize,
			partitions_global_connectivity.len()
		);
		if self.n_partitions < 2 {
			return;
		}
		// We need to have a way to simply connect those global cells. Each partition has an index, interior vec is the edges with weight.
		let mut connectivity = vec![vec![]; self.n_partitions as usize];
		// Sum up the number of edges between partitions, use as weight in algorithm.
		for (pid, partition) in partitions_global_connectivity.iter().enumerate() {
			let mut sum_edges = vec![0_usize; self.n_partitions as usize];
			for single_node_edges in partition {
				for edge in single_node_edges {
					sum_edges[edge.0 as usize] += 1;
				}
			}
			// Add an index to retain which partition it goes to, filter weight 0 edges.
			connectivity[pid] = sum_edges
				.into_iter()
				.enumerate()
				.filter(|(_, sum)| *sum > 0)
				.collect_vec()
		}
	}

	fn global_freeze(
		&mut self,
		logical: &LogicalDesign,
		margin: usize,
		partition_dims: (usize, usize),
		local_to_global: &Vec<Vec<usize>>,
	) {
		self.global_space = Arr2::new([
			self.side_length_partitions * (partition_dims.0 + margin) + margin,
			self.side_length_partitions * (partition_dims.1 + margin) + margin,
		]);
		for (part, (part_x, part_y)) in self.partition_assignments.clone().iter().enumerate() {
			let mut failure = Ok(());
			let comb_positions = self.local_assignments[part].clone();
			for (idx, (x, y)) in comb_positions.iter().enumerate() {
				let pos = (
					(part_x * (partition_dims.0 + margin) + x + margin) as f64,
					(part_y * (partition_dims.1 + margin) + y + margin) as f64,
				);
				let global_id = local_to_global[part][idx];
				let res2 = self.place_comb_physical(pos, PhyId(global_id), logical, None);
				match res2.clone() {
					Ok(_) => {}
					Err(e) => {
						println!("Failed to place cell {idx} at position {:?}. {}", pos, e);
					}
				}
				if failure.is_ok() {
					failure = res2;
				}
			}
		}
	}

	fn print_ascii_global_space(&self) {
		println!("global_space:");
		print!("   ");
		for i in 0..self.global_space.dims().0 {
			print!(" {} ", i % 10);
		}
		println!();
		for y in 0..self.global_space.dims().1 {
			print!("{y:3} ");
			for x in 0..self.global_space.dims().0 {
				if self.global_space[(x, y)] == PhyId::default() {
					print!(" _ ");
					continue;
				}
				let node = &self.nodes[self.global_space[(x, y)].0];
				if node.is_pole {
					print!(" P ");
				} else {
					print!(" {} ", self.global_space[(x, y)].0)
				}
			}
			println!();
		}
	}

	fn global_freeze_and_route(
		&mut self,
		logical: &LogicalDesign,
		partition_global_connectivity: &Vec<Vec<Vec<(i32, usize, usize)>>>,
		local_to_global: &Vec<Vec<usize>>,
		global_to_local: &Vec<HashM<usize, usize>>,
	) {
		let partition_dims = calculate_minimum_partition_dim(&self.local_assignments);
		println!(
			"Starting global freeze and route with partition dims ({}, {}).",
			partition_dims.0, partition_dims.1
		);
		let edges_to_route =
			adjacency_to_edges_global_edges(partition_global_connectivity, local_to_global);
		let margin_range = self.user_partition_margin.map(|m| m..=m).unwrap_or(0..=10);
		for margin in margin_range {
			self.intra_partition_margin = margin;
			self.reset_place_route();
			self.global_freeze(logical, margin, partition_dims, local_to_global);
			for (i, edge) in edges_to_route.iter().enumerate() {
				let (input_sided, output_sided) = self.get_needed_routes(*edge, logical);
				if input_sided {
					match self.route_single_edge(*edge) {
						Ok(path) => {
							println!(
								"Comitting route (input sided) {:?} -> {:?}: {:?}",
								edge.0, edge.1, path
							);
							self.commit_route(path, PhyId(edge.0), PhyId(edge.1), true);
						}
						Err(s) => {
							panic!("Failed durring routing on edge {i}: {s}");
						}
					}
				}
				if output_sided {
					match self.route_single_edge(*edge) {
						Ok(path) => {
							println!(
								"Comitting route (output sided) {:?} -> {:?}: {:?}",
								edge.0, edge.1, path
							);
							self.commit_route(path, PhyId(edge.0), PhyId(edge.1), false);
						}
						Err(s) => {
							panic!("Failed durring routing on edge {i}: {s}");
						}
					}
				}
			}
			#[cfg(debug_assertions)]
			self.print_ascii_global_space();
		}
	}

	fn get_needed_routes(&self, edge: (usize, usize), logical: &LogicalDesign) -> (bool, bool) {
		let logic_id0 = self.nodes[edge.0].logic;
		let logic_id1 = self.nodes[edge.1].logic;
		logical.have_shared_wire_discriminate_side(logic_id0, logic_id1)
	}

	fn commit_route(&mut self, mut path: Vec<SpaceIndex>, src: PhyId, dst: PhyId, is_input: bool) {
		let mut last_id = {
			let loc = path.pop().unwrap();
			let id = PhyId(self.nodes.len());
			let logic = self.nodes[src.0].logic;
			self.nodes.push(PhyNode {
				id,
				logic,
				position: (loc.0 as f64, loc.1 as f64),
				placed: true,
				orientation: 4,
				partition: -1,
				is_pole: true,
				routes: vec![],
				foreign_routes: vec![],
			});
			self.global_space[(loc.0, loc.1)] = id;
			id
		};
		let near_side_pole_id = last_id;
		let mut far_side_pole_id = last_id;
		while !path.is_empty() {
			let loc = path.pop().unwrap();
			let id = PhyId(self.nodes.len());
			self.nodes.push(PhyNode {
				id,
				logic: self.nodes[src.0].logic,
				position: (loc.0 as f64, loc.1 as f64),
				placed: true,
				orientation: 4,
				partition: -1,
				is_pole: true,
				routes: vec![],
				foreign_routes: vec![],
			});
			let wire_id = self.wires.len();
			self.wires.push(Wire {
				id: WireId(wire_id),
				node1_id: last_id,
				node2_id: id,
				terminal1_id: TerminalId::input(WireColour::Red),
				terminal2_id: TerminalId::input(WireColour::Red),
			});
			let wire_id = self.wires.len();
			self.wires.push(Wire {
				id: WireId(wire_id),
				node1_id: last_id,
				node2_id: id,
				terminal1_id: TerminalId::input(WireColour::Green),
				terminal2_id: TerminalId::input(WireColour::Green),
			});
			self.global_space[(loc.0, loc.1)] = id;
			last_id = id;
			far_side_pole_id = last_id;
		}

		self.nodes[src.0].routes.push(Route {
			input_sided: is_input,
			far_side_id: dst,
			near_side_pole_id,
			far_side_pole_id,
		});
		self.nodes[dst.0].foreign_routes.push(src);
	}

	fn route_single_edge(&mut self, edge: (usize, usize)) -> Result<Vec<SpaceIndex>, String> {
		let comb1 = &self.nodes[edge.0];
		let comb2 = &self.nodes[edge.1];
		let neig1 = self.get_neighbors(
			&SpaceIndex(comb1.position.0 as usize, comb1.position.1 as usize),
			&SpaceIndex(0, 0),
		);

		let neig2 = self.get_neighbors(
			&SpaceIndex(comb2.position.0 as usize, comb2.position.1 as usize),
			&SpaceIndex(0, 0),
		);
		let neig2 = [SpaceIndex(
			comb2.position.0 as usize,
			comb2.position.1 as usize,
		)]
		.into_iter()
		.chain(neig2);

		let a = neig1
			.into_iter()
			.filter(|pos| distance_diff(&(pos.0 as f64, pos.1 as f64), &comb1.position) <= 8.0)
			.collect_vec();
		let b = neig2
			.filter(|pos| distance_diff(&(pos.0 as f64, pos.1 as f64), &comb2.position) <= 8.0)
			.collect_vec();
		let ret = route::a_star_initial_set(self, &a, &b, None);
		if ret.is_empty() {
			return Result::Err(format!("Could not route {} -> {}", edge.0, edge.1));
		}
		Result::Ok(ret)
	}

	fn get_neighbors(&self, index: &SpaceIndex, _goal: &SpaceIndex) -> SpaceIter {
		let min_x = index.0 - 9;
		let min_y = index.1 - 9;
		let max_x = index.0 + 9;
		let max_y = index.1 + 9;
		let mut ret = vec![];
		for x in min_x..=max_x {
			for y in min_y..=max_y {
				let dx = x.abs_diff(index.0) as f32;
				let dy = y.abs_diff(index.1) as f32;
				let distance = dx * dx + dy * dy;
				if distance <= 81.0
					&& self.global_space.index_good((x, y))
					&& self.global_space[(x, y)] == PhyId::default()
				{
					ret.push(SpaceIndex(x, y));
				}
			}
		}
		SpaceIter { points: ret }
	}
}

pub(crate) fn calculate_minimum_partition_dim(
	local_assignments: &Vec<Vec<(usize, usize)>>,
) -> (usize, usize) {
	let mut max_x = 0;
	let mut max_y = 0;
	for p in local_assignments {
		for (x, y) in p {
			max_x = max_x.max(*x);
			max_y = max_y.max(*y);
		}
	}
	(max_x + 2, max_y + 1)
}

pub(crate) fn adjacency_to_edges(
	conn_list: &Vec<Vec<usize>>,
	triangular: bool,
) -> Vec<(usize, usize)> {
	let mut ret = vec![];
	for (id_i, connected_j) in conn_list.iter().enumerate() {
		for id_j in connected_j {
			if triangular && *id_j < id_i {
				continue;
			}
			ret.push((id_i, *id_j));
		}
	}
	ret
}

pub(crate) fn adjacency_to_edges_global_edges(
	partition_global_connectivity: &Vec<Vec<Vec<(i32, usize, usize)>>>,
	local_to_global: &Vec<Vec<usize>>,
) -> Vec<(usize, usize)> {
	let mut ret = vec![];
	for (part, edges) in partition_global_connectivity.iter().enumerate() {
		for (local_id, adjacency) in edges.iter().enumerate() {
			let global_id = local_to_global[part][local_id];
			for neighbor in adjacency {
				if global_id < neighbor.2 {
					ret.push((global_id, neighbor.2));
				}
			}
		}
	}
	ret
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
struct SpaceIndex(usize, usize);

impl route::TopologyIndex for SpaceIndex {}

struct SpaceIter {
	points: Vec<SpaceIndex>,
}

impl Iterator for SpaceIter {
	type Item = SpaceIndex;

	fn next(&mut self) -> Option<Self::Item> {
		self.points.pop()
	}
}

impl<'iter> Topology<'iter, PhyId, SpaceIndex> for PhysicalDesign {
	type Iter = SpaceIter;

	// Horribly inefficient. but w/e, we will see how slow.
	fn neighbors(&'iter self, index: &SpaceIndex, goal: &SpaceIndex) -> Self::Iter {
		self.get_neighbors(index, goal)
	}

	fn contains(&self, _index: &SpaceIndex) -> bool {
		true
	}

	fn distance(&self, a: &SpaceIndex, b: &SpaceIndex) -> f32 {
		let dx = b.0.abs_diff(a.0) as f32;
		let dy = b.1.abs_diff(a.1) as f32;
		(dx * dx + dy * dy).sqrt()
	}

	fn heuristic(&self, a: &SpaceIndex, b: &SpaceIndex) -> f32 {
		self.distance(a, b)
	}
}

fn distance_diff(a: &(f64, f64), b: &(f64, f64)) -> f64 {
	let dx = a.0 - b.0;
	let dy = a.1 - b.1;
	(dx * dx + dy * dy).sqrt()
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
			ld::NodeFunction::WireSum(_c) => unreachable!(),
		}
	}
}

fn euclidean_distance_squared_f64_pair(x: (f64, f64), y: (f64, f64)) -> f64 {
	(y.0 - x.0) * (y.0 - x.0) + (y.1 - x.1) * (y.1 - x.1)
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

	use std::{fs::File, io::Write};

	use crate::{
		logical_design::{get_large_logical_design, get_large_logical_design_2d},
		serializable_design,
	};

	use super::*;
	#[test]
	fn new() {
		let mut p = PhysicalDesign::new();
		let l = ld::get_simple_logical_design();
		p.build_from(&l);
	}

	#[test]
	fn n_combs() {
		let mut p = PhysicalDesign::new();
		let l = get_large_logical_design(200);
		p.build_from(&l);
		p.save_svg(&l, "svg/n_combs_connectivity_averaging.svg");
	}

	#[test]
	fn simple_connectivity_averaging() {
		let mut p = PhysicalDesign::new();
		let l = ld::get_simple_logical_design();
		println!("{l}");
		p.build_from(&l);
	}

	#[test]
	fn complex_40_mcmc_dense() {
		let mut p = PhysicalDesign::new();
		let l = ld::get_complex_40_logical_design();
		p.build_from(&l);
		p.save_svg(&l, "svg/complex40_mcmc_coarse15.svg");
	}

	#[test]
	fn memory_n_mcmc_dense() {
		let mut p = PhysicalDesign::new();
		let l = ld::get_large_memory_test_design(2_000);
		p.build_from(&l);
		p.save_svg(&l, "svg/memory_n_mcmc_dense.svg");
	}

	#[test]
	fn dense_memory_n_mcmc_dense() {
		let mut p = PhysicalDesign::new();
		let l = ld::get_large_dense_memory_test_design(1_024);
		p.build_from(&l);
		p.save_svg(&l, "svg/dense_memory_n_mcmc_dense.svg");

		let mut s = serializable_design::SerializableDesign::new();
		s.build_from(&p, &l);
		let blueprint_json: String = serde_json::to_string(&s).unwrap();
		let mut output = File::create("./output/dense_memory_n_mcmc_dense.json").unwrap();
		output.write_all(blueprint_json.as_bytes()).unwrap();
	}

	#[test]
	fn sweep_synthetic_n_mcmc_dense() {
		for x in (20..=200).step_by(20) {
			println!("==============={x}===============");
			let mut p = PhysicalDesign::new();
			let l = get_large_logical_design(x);
			p.build_from(&l);
		}
	}

	#[test]
	fn synthetic_n_mcmc_dense() {
		let mut p = PhysicalDesign::new();
		let l = get_large_logical_design(500);
		p.build_from(&l);
		p.save_svg(&l, "svg/synthetic_n_mcmc_dense.svg");
	}

	#[test]
	fn synthetic_2d_n_mcmc_dense() {
		let mut p = PhysicalDesign::new();
		let l = get_large_logical_design_2d(50);
		p.build_from(&l);
		p.save_svg(&l, "svg/synthetic_2d_n_mcmc_dense.svg");
	}

	#[test]
	fn comb_pair() {
		let mut p = PhysicalDesign::with_parameters(Some(10), Some(1));
		let mut l = LogicalDesign::new();
		let c1 = l.add_nop_simple();
		let c2 = l.add_nop_simple();
		l.add_wire_red_simple(c1, c2);
		l.set_description_node(c1, "c1".to_owned());
		l.set_description_node(c2, "c2".to_owned());
		p.build_from(&l);
		p.save_svg(&l, "svg/comb_pair.svg");
	}
}

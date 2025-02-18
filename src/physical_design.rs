use core::{f64, panic};
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};
use std::os::unix::raw::blksize_t;
use std::sync::{Arc, RwLock};
use std::{
	cmp::Ordering,
	collections::{HashMap, HashSet, LinkedList},
	hash::Hash,
	vec,
};

use crate::{
	logical_design::{self as ld, LogicalDesign, WireColour},
	svg::SVG,
};

#[derive(Debug, Clone, Copy, Default, clap::ValueEnum)]
pub enum PlacementStrategy {
	ConnectivityAveraging,
	#[default]
	MCMCSADense,
	MCMCSADenseParallel,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CombinatorId(usize);

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

	connected_wires: Vec<Vec<WireId>>,
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
			connected_wires: vec![],
		}
	}

	pub fn build_from(&mut self, logical: &LogicalDesign, placement_strategy: PlacementStrategy) {
		self.extract_combs(logical);
		self.place_combs(logical, placement_strategy);
		self.connect_combs(logical);
		self.validate_against(logical);
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
					position: (0.0, 0.0),
					placed: false,
					orientation: 4,
				});
				self.idx_combs.insert(ld_node.id, id);
				self.connected_wires.push(vec![]);
			}
			ld::NodeFunction::WireSum(_c) => { /* Do nothing for now */ }
		});
	}

	pub fn get_logical<'a>(&self, id: CombinatorId, logical: &'a LogicalDesign) -> &'a ld::Node {
		let comb = &self.combs[id.0];
		let ld_node = logical.get_node(comb.logic);
		ld_node
	}

	fn solve_as_connectivity_averaging(
		&mut self,
		logical: &LogicalDesign,
	) -> Result<Vec<(usize, usize)>, ()> {
		logical.for_all(|_, node| {
			match node.function {
				ld::NodeFunction::Arithmetic { .. }
				| ld::NodeFunction::Decider { .. }
				| ld::NodeFunction::Constant { .. }
				| ld::NodeFunction::Lamp { .. } => {
					self.recurse_place_comb(*self.idx_combs.get(&node.id).unwrap(), logical)
				}
				ld::NodeFunction::WireSum(..) => {}
			};
		});
		let min_x = self
			.combs
			.iter()
			.map(|c| c.position.0)
			.min_by(|a, b| a.total_cmp(b))
			.unwrap()
			.floor();
		let min_y = self
			.combs
			.iter()
			.map(|c| c.position.1)
			.min_by(|a, b| a.total_cmp(b))
			.unwrap()
			.floor();
		for c in &mut self.combs {
			c.position.0 -= min_x;
			c.position.1 -= min_y;
		}
		let mut ret = vec![(0, 0); self.combs.len()];
		for (idx, c) in self.combs.iter().enumerate() {
			ret[idx] = (c.position.0.round() as usize + 1, c.position.1 as usize);
		}
		self.save_svg(logical, "./svg/internal_cavg.svg");
		self.reset_place_route();
		Ok(ret)
	}

	fn get_initializations(&self, logical: &LogicalDesign) -> Vec<Vec<(usize, usize)>> {
		let side_length = (self.combs.len() as f64 * 2.0).sqrt().ceil() as usize;
		let mut retval = vec![];
		retval.push(vec![(0, 0); self.combs.len()]);
		{
			let initial0 = retval.last_mut().unwrap();
			let mut x = 0;
			let mut y: isize = 0;
			let mut ydir = 1;
			for c in &self.get_bfs_order(CombinatorId(0), logical) {
				if y >= side_length as isize || y < 0 {
					x += 2;
					ydir = -ydir;
					y += ydir;
				}
				initial0[c.0] = (x, y as usize);
				y += ydir;
			}
		}
		retval.push(vec![(0, 0); self.combs.len()]);
		{
			let initial1 = retval.last_mut().unwrap();
			let mut x = 0;
			let mut y = 0;
			for c in &self.get_bfs_order(CombinatorId(0), logical) {
				if y >= side_length {
					x += 2;
					y = 0;
				}
				initial1[c.0] = (x, y);
				y += 1;
			}
		}
		retval.push(vec![(0, 0); self.combs.len()]);
		{
			let initial2 = retval.last_mut().unwrap();
			let mut x = 0;
			let mut y: isize = 0;
			let mut ydir = 1;
			for c in 0..self.combs.len() {
				if y >= side_length as isize || y < 0 {
					x += 2;
					ydir = -ydir;
					y += ydir;
				}
				initial2[c] = (x, y as usize);
				y += ydir;
			}
		}
		retval.push(vec![(0, 0); self.combs.len()]);
		{
			let initial3 = retval.last_mut().unwrap();
			let mut x = 0;
			let mut y = 0;
			for c in 0..self.combs.len() {
				if y >= side_length {
					x += 2;
					y = 0;
				}
				initial3[c] = (x, y);
				y += 1;
			}
		}
		if self.combs.len() > 100 {
			retval.push(vec![(0, 0); self.combs.len()]);
			let initial = retval.last_mut().unwrap();
			let mut x = 0;
			let mut y = 0;
			let mut ydir = false;
			for c in &self.get_bfs_order(CombinatorId(0), logical) {
				initial[c.0] = (x, y as usize);
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

	fn place_combs(&mut self, logical: &LogicalDesign, placement_strategy: PlacementStrategy) {
		match placement_strategy {
			PlacementStrategy::ConnectivityAveraging => {
				let pos = self.solve_as_connectivity_averaging(logical).unwrap();
				self.place_combs_physical_dense(&pos, logical).unwrap();
			}
			PlacementStrategy::MCMCSADense => {
				let initializations = self.get_initializations(logical);
				self.reset_place_route();
				let comb_positions =
					match self.solve_as_mcmc_dense(logical, 1.5, &initializations, None) {
						Ok(pos) => pos,
						Err(e) => {
							println!("WARN: MCMC failed to place with scale 1.3");
							println!("WARN: {}", e.0);
							self.place_combs_physical_dense(&e.1, logical).is_ok();
							self.connect_combs(logical);
							self.save_svg(logical, format!("./svg/failed{}.svg", 1.3).as_str());
							panic!("failed to place");
						}
					};
				self.place_combs_physical_dense(&comb_positions, logical)
					.unwrap();
			}
			PlacementStrategy::MCMCSADenseParallel => {
				let initializations = self.get_initializations(logical);
				self.reset_place_route();
				let comb_positions =
					match self.solve_as_mcmc_dense_parallel(logical, 1.5, &initializations, None) {
						Ok(pos) => pos,
						Err(e) => {
							println!("WARN: MCMC failed to place with scale 1.3");
							println!("WARN: {}", e.0);
							match self.place_combs_physical_dense(&e.1, logical) {
								Ok(_) => {}
								Err(_) => {}
							}
							self.connect_combs(logical);
							self.save_svg(logical, format!("./svg/failed{}.svg", 1.3).as_str());
							panic!("failed to place");
						}
					};
				self.place_combs_physical_dense(&comb_positions, logical)
					.unwrap();
				return;
			}
		}
	}

	fn place_combs_physical_dense(
		&mut self,
		comb_positions: &Vec<(usize, usize)>,
		logical: &LogicalDesign,
	) -> Result<(), ()> {
		for (idx, (x, y)) in comb_positions.iter().enumerate() {
			let p = (*x as f64, *y as f64);
			self.place_comb_physical(p, CombinatorId(idx), logical)?;
		}
		Ok(())
	}

	fn place_combs_physical_coarse15(
		&mut self,
		comb_positions: &Vec<(usize, usize)>,
		logical: &LogicalDesign,
	) {
		let mut coarse_cells: HashMap<(usize, usize), usize> = HashMap::new();
		for (idx, (x, y)) in comb_positions.iter().enumerate() {
			let allocated_resources = match coarse_cells.get(&(*x, *y)) {
				Some(&allocated) => {
					coarse_cells.insert((*x, *y), allocated + 1);
					allocated
				}
				None => {
					coarse_cells.insert((*x, *y), 1);
					0
				}
			};
			let p = (
				(*x * 3 + (allocated_resources % 3)) as f64 * 2.0,
				(*y * 5 + allocated_resources / 3) as f64,
			);
			self.place_comb_physical(p, CombinatorId(idx), logical)
				.unwrap();
		}
	}

	pub fn solve_as_mcmc_dense(
		&self,
		ld: &LogicalDesign,
		scale_factor: f64,
		initializations: &Vec<Vec<(usize, usize)>>,
		init_temp: Option<f64>,
	) -> Result<Vec<(usize, usize)>, (String, Vec<(usize, usize)>)> {
		let side_length = (self.combs.len() as f64 * scale_factor * 2.0).sqrt().ceil() as usize;
		let num_cells = self.combs.len();

		let connections = self.get_connectivity_as_edges(ld, true);

		let compute_cost = |assign: &Vec<(usize, usize)>,
		                    block_counts: &Vec<Vec<i32>>,
		                    max_density: i32|
		 -> (f64, bool, i32) {
			let mut cost = 0.0;
			let mut sat_count = 0;
			let mut sat = true;
			for &(i, j) in &connections {
				let (x_i, y_i) = assign[i];
				let (x_j, y_j) = assign[j];
				let dx = (x_i as isize - x_j as isize).abs() as f64;
				let dy = (y_i as isize - y_j as isize).abs() as f64;
				let r2distance = dx.powi(2) + dy.powi(2);
				if r2distance > (64.0 + 81.0) / 2.0 {
					cost += r2distance.sqrt();
					sat_count += 1;
					sat = false;
				} else {
					cost += r2distance.sqrt() / 10.0;
				}
			}
			for block_y in block_counts.iter() {
				for count in block_y.iter() {
					if *count > 1 {
						sat = false;
						cost += *count as f64 * 10.0;
						sat_count += *count - max_density;
					}
				}
			}
			(cost, sat, sat_count)
		};

		let mut rng = StdRng::seed_from_u64(0xCAFEBABE);
		let mut assignments = vec![(0, 0); num_cells];
		let mut block_counts = vec![vec![0; side_length]; side_length];

		let mut max_density = 15;

		if initializations.is_empty() {
			for cell in 0..num_cells {
				loop {
					let (cx, cy) = (
						rng.random_range(0..side_length) / 2 * 2,
						rng.random_range(0..side_length),
					);
					if block_counts[cx][cy] < max_density {
						assignments[cell] = (cx, cy);
						block_counts[cx][cy] += 1;
						break;
					}
				}
			}
		} else {
			let mut min_score = (f64::INFINITY, false, i32::MAX);
			let mut min_idx = 0;
			for (idx, init) in initializations.iter().enumerate() {
				block_counts = vec![vec![0; side_length]; side_length];
				for (cx, cy) in init {
					block_counts[*cx][*cy] += 1;
				}
				let cost = compute_cost(init, &block_counts, max_density);
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
			assignments = initializations[min_idx].clone();
			block_counts = vec![vec![0; side_length]; side_length];
			for (cx, cy) in &assignments {
				block_counts[*cx][*cy] += 1;
			}
		}

		let mut best_assign = assignments.clone();
		let mut best_block_counts = block_counts.clone();
		let mut best_cost = compute_cost(&assignments, &block_counts, max_density);
		let mut assignments_cost = best_cost;

		let mut temp = init_temp.unwrap_or(20.0 + 10.0 * (num_cells as f64));
		let cooling_rate = 1.0 - 1E-7;
		let mut iterations = 20_000_000;

		let check_invariants = |block_counts: &Vec<Vec<i32>>, max_density: i32| {
			for (x, block_y) in block_counts.iter().enumerate() {
				for count in block_y.iter() {
					if *count > max_density {
						assert!(false);
					}
					if *count > 0 && x % 2 == 1 {
						assert!(false);
					}
				}
			}
		};

		let mut final_stage = false;
		let mut step = 0;
		while step < iterations {
			if best_cost.1 && !final_stage {
				check_invariants(&block_counts, 1);
				final_stage = true;
				iterations = (step as f64 * 1.2) as i32;
				println!("Entering final compacting stage.");
				if best_cost.0 <= 0.0 {
					break;
				}
			}

			let mut new_block_counts = block_counts.clone();
			let mut new_assignments = assignments.clone();

			const METHODS: &[(
				usize,
				bool,
				fn(
					rng: &mut StdRng,
					assignments: &Vec<(usize, usize)>,
					block_counts: &Vec<Vec<i32>>,
					new_assignments: &mut Vec<(usize, usize)>,
					new_block_counts: &mut Vec<Vec<i32>>,
					side_length: usize,
					max_density: i32,
				),
			)] = &[
				(650, true, ripup_replace_method),
				(200, true, swap_local_method),
				//(15, false, swap_random_method),
				(25, false, ripup_range_method),
				(350, false, crack_in_two_method),
				(50, false, slide_puzzle_method),
				(100, false, slide_puzzle_method_worst_cells),
				(100, false, overflowing_cells_swap_local_method),
			];

			// Select weighted method
			{
				let total_weight: usize = METHODS
					.iter()
					.filter(|(_, can_run_if_final, _)| !final_stage || *can_run_if_final)
					.map(|(weight, _, _)| *weight)
					.sum();

				let pick = rng.random_range(0..total_weight);
				let mut cumulative = 0;
				for (weight, can_run_if_final, func) in METHODS {
					if final_stage && !can_run_if_final {
						continue;
					}
					cumulative += weight;
					if pick < cumulative {
						func(
							&mut rng,
							&assignments,
							&block_counts,
							&mut new_assignments,
							&mut new_block_counts,
							side_length,
							max_density,
						);
						break;
					}
				}
			}

			let new_cost = compute_cost(&new_assignments, &new_block_counts, max_density);
			let delta = new_cost.0 - best_cost.0;

			if new_cost.2 <= 0 && max_density > 1 {
				max_density -= 1;
				println!("Reducing max density to {max_density}");
			}

			if step % 1000000 == 0 {
				println!(
					"Current cost {} ({}), temp {}",
					assignments_cost.0, assignments_cost.2, temp
				);
			}

			if !final_stage && (delta < 0.0 || new_cost.1)
				|| final_stage && (delta < 0.0 && new_cost.1)
			{
				best_cost = new_cost;
				best_assign = new_assignments.clone();
				assignments = new_assignments;
				block_counts = new_block_counts;
				assignments_cost = best_cost;
				best_block_counts = block_counts.clone();
				println!("Best cost {} ({}), temp {}", best_cost.0, best_cost.2, temp);
				if !final_stage && iterations - step < iterations / 10 {
					println!("Boosting iterations {}", best_cost.0);
					iterations = iterations * 25 / 20;
					temp *= 1.12;
				}
			} else {
				let p = f64::exp(-delta / temp);
				if rng.random_bool(p) {
					assignments = new_assignments;
					block_counts = new_block_counts;
					assignments_cost = new_cost;
				}
			}

			if assignments_cost.0 / best_cost.0 > 2.0 {
				assignments = best_assign.clone();
				block_counts = best_block_counts.clone();
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
				best_assign,
			));
		}

		Ok(best_assign)
	}

	pub fn solve_as_mcmc_dense_parallel(
		&self,
		ld: &LogicalDesign,
		scale_factor: f64,
		initializations: &Vec<Vec<(usize, usize)>>,
		init_temp: Option<f64>,
	) -> Result<Vec<(usize, usize)>, (String, Vec<(usize, usize)>)> {
		let side_length = (self.combs.len() as f64 * scale_factor * 2.0).sqrt().ceil() as usize;
		let num_cells = self.combs.len();

		let connections = self.get_connectivity_as_edges(ld, true);

		let compute_cost = |assign: &Vec<(usize, usize)>,
		                    block_counts: &Vec<Vec<i32>>,
		                    max_density: i32|
		 -> (f64, bool, i32) {
			let mut cost = 0.0;
			let mut sat_count = 0;
			let mut sat = true;
			for &(i, j) in &connections {
				let (x_i, y_i) = assign[i];
				let (x_j, y_j) = assign[j];
				let dx = (x_i as isize - x_j as isize).abs() as f64;
				let dy = (y_i as isize - y_j as isize).abs() as f64;
				let r2distance = dx.powi(2) + dy.powi(2);
				if r2distance > (64.0 + 81.0) / 2.0 {
					cost += r2distance.sqrt();
					sat_count += 1;
					sat = false;
				} else {
					cost += r2distance.sqrt() / 10.0;
				}
			}
			for (_x, block_y) in block_counts.iter().enumerate() {
				for (_y, count) in block_y.iter().enumerate() {
					if *count > 1 {
						sat = false;
						cost += *count as f64 * 10.0;
						sat_count += *count - max_density;
					}
				}
			}
			(cost, sat, sat_count)
		};

		let mut rng = StdRng::seed_from_u64(0xCAFEBABE);
		let mut assignments = vec![(0, 0); num_cells];
		let mut block_counts = vec![vec![0; side_length]; side_length];

		let max_density_init = 15;

		if initializations.is_empty() {
			for cell in 0..num_cells {
				loop {
					let (cx, cy) = (
						rng.random_range(0..side_length) / 2 * 2,
						rng.random_range(0..side_length),
					);
					if block_counts[cx][cy] < max_density_init {
						assignments[cell] = (cx, cy);
						block_counts[cx][cy] += 1;
						break;
					}
				}
			}
		} else {
			let mut min_score = (f64::INFINITY, false, i32::MAX);
			let mut min_idx = 0;
			for (idx, init) in initializations.iter().enumerate() {
				block_counts = vec![vec![0; side_length]; side_length];
				for (cx, cy) in init {
					block_counts[*cx][*cy] += 1;
				}
				let cost = compute_cost(init, &block_counts, max_density_init);
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
			assignments = initializations[min_idx].clone();
			block_counts = vec![vec![0; side_length]; side_length];
			for (cx, cy) in &assignments {
				block_counts[*cx][*cy] += 1;
			}
		}

		struct Best {
			assignments: Vec<(usize, usize)>,
			block_counts: Vec<Vec<i32>>,
			cost: (f64, bool, i32),
			max_density: i32,
		}

		let best = Arc::from(RwLock::new(Best {
			assignments: assignments.clone(),
			block_counts: block_counts.clone(),
			cost: compute_cost(&assignments, &block_counts, max_density_init),
			max_density: max_density_init,
		}));

		let cooling_rate = 1.0 - 1E-7;

		let check_invariants = |block_counts: &Vec<Vec<i32>>, max_density: i32| {
			for (x, block_y) in block_counts.iter().enumerate() {
				for (_y, count) in block_y.iter().enumerate() {
					if *count > max_density {
						assert!(false);
					}
					if *count > 0 && x % 2 == 1 {
						assert!(false);
					}
				}
			}
		};

		let n_threads = 2; //std::thread::available_parallelism().unwrap().get();
		let pool = rayon::ThreadPoolBuilder::new()
			.num_threads(n_threads)
			.build()
			.unwrap();

		pool.scope(|s| {
			for i in 0..n_threads {
				let best = Arc::clone(&best);
				s.spawn(move |_| {
					let mut rng = StdRng::seed_from_u64(i as u64);
					let (mut assignments, mut block_counts, mut assignments_cost) = {
						let tmpbest = best.read().unwrap();
						(
							tmpbest.assignments.clone(),
							tmpbest.block_counts.clone(),
							tmpbest.cost.clone(),
						)
					};

					let mut temp = init_temp.unwrap_or(20.0 + 10.0 * (num_cells as f64));
					let mut iterations = 20_000_000;
					let mut final_stage = false;
					let mut step = 0;
					while step < iterations {
						{
							let tmpbest = best.read().unwrap();
							if tmpbest.cost.1 && !final_stage {
								check_invariants(&block_counts, 1);
								final_stage = true;
								iterations = (step as f64 * 1.2) as i32;
								println!("Entering final compacting stage.");
								if tmpbest.cost.0 <= 0.0 {
									break;
								}
							}
						}

						let max_density = best.read().unwrap().max_density;
						let mut new_block_counts = block_counts.clone();
						let mut new_assignments = assignments.clone();

						const METHODS: &[(
							usize,
							bool,
							fn(
								rng: &mut StdRng,
								assignments: &Vec<(usize, usize)>,
								block_counts: &Vec<Vec<i32>>,
								new_assignments: &mut Vec<(usize, usize)>,
								new_block_counts: &mut Vec<Vec<i32>>,
								side_length: usize,
								max_density: i32,
							),
						)] = &[
							(650, true, ripup_replace_method),
							(200, true, swap_local_method),
							//(15, false, swap_random_method),
							(25, false, ripup_range_method),
							(350, false, crack_in_two_method),
							(50, false, slide_puzzle_method),
							(100, false, slide_puzzle_method_worst_cells),
							(100, false, overflowing_cells_swap_local_method),
						];

						// Select weighted method
						{
							let total_weight: usize = METHODS
								.iter()
								.filter(|(_, can_run_if_final, _)| {
									!final_stage || *can_run_if_final
								})
								.map(|(weight, _, _)| *weight)
								.sum();

							let pick = rng.random_range(0..total_weight);
							let mut cumulative = 0;
							for (weight, can_run_if_final, func) in METHODS {
								if final_stage && !can_run_if_final {
									continue;
								}
								cumulative += weight;
								if pick < cumulative {
									func(
										&mut rng,
										&assignments,
										&block_counts,
										&mut new_assignments,
										&mut new_block_counts,
										side_length,
										max_density,
									);
									break;
								}
							}
						}

						let new_cost =
							compute_cost(&new_assignments, &new_block_counts, max_density);

						{
							let best_cost = best.read().unwrap().cost.clone();
							let delta = new_cost.0 - best_cost.0;

							if new_cost.2 <= best_cost.2
								&& (!final_stage && (delta < 0.0 || new_cost.1)
									|| final_stage && (delta < 0.0 && new_cost.1))
							{
								let mut tmpbest = best.write().unwrap();
								if new_cost.2 <= 0 && max_density > 1 {
									tmpbest.max_density -= 1;
									println!("Reducing max density to {}", tmpbest.max_density);
								}
								let delta = new_cost.0 - tmpbest.cost.0;
								if !final_stage && (delta < 0.0 || new_cost.1)
									|| final_stage && (delta < 0.0 && new_cost.1)
								{
									tmpbest.cost = new_cost;
									tmpbest.assignments = new_assignments.clone();
									assignments = new_assignments;
									block_counts = new_block_counts;
									assignments_cost = tmpbest.cost;
									tmpbest.block_counts = block_counts.clone();
									println!(
										"Best cost {} ({}), max_density {}, temp {}",
										tmpbest.cost.0, tmpbest.cost.2, max_density, temp
									);
									if !final_stage && iterations - step < iterations * 1 / 10 {
										println!("Boosting iterations {}", tmpbest.cost.0);
										iterations = iterations * 25 / 20;
										temp *= 1.12;
									}
								}
							} else {
								let p = f64::exp(-delta / temp);
								if p >= 1.0 || rng.random_bool(p) {
									assignments = new_assignments;
									block_counts = new_block_counts;
									assignments_cost = new_cost;
								}
							}
						}

						let tmpbest = best.read().unwrap();
						if assignments_cost.0 / tmpbest.cost.0 > 2.0 {
							assignments = tmpbest.assignments.clone();
							block_counts = tmpbest.block_counts.clone();
							assignments_cost = tmpbest.cost;
						}

						temp *= cooling_rate;
						if temp < 1e-3 {
							temp = 1e-3;
						}
						step += 1;
					}
				});
			}
		});

		//check_invariants(&block_counts, max_density);

		let best = best.read().unwrap();
		if !best.cost.1 {
			return Err((
				format!(
					"Could not satisfy distance requirements, lowest cost: {}",
					best.cost.0
				),
				best.assignments.clone(),
			));
		}

		Ok(best.assignments.clone())
	}

	pub fn max_allowable_distance(
		&self,
		logical: &LogicalDesign,
		id_comb_a: CombinatorId,
		id_comb_b: CombinatorId,
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
		self.space.clear();
		for x in &mut self.connected_wires {
			x.clear();
		}
		for c in &mut self.combs {
			c.placed = false;
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
			let ld_in_wire_fanin_red_iter = ld_comb
				.fanin_red
				.first() // Option<NodeId>
				.map(|id| {
					logical.assert_is_wire_sum(*id);
					logical.get_node(*id).iter_fanin(WireColour::Red)
				})
				.into_iter()
				.flatten();
			let ld_in_wire_fanin_green_iter = ld_comb
				.fanin_green
				.first() // Option<NodeId>
				.map(|id| {
					logical.assert_is_wire_sum(*id);
					logical.get_node(*id).iter_fanin(WireColour::Green)
				})
				.into_iter()
				.flatten();
			let ld_out_wire_fanout_red_iter = ld_comb
				.fanout_red
				.first() // Option<NodeId>
				.map(|id| {
					logical.assert_is_wire_sum(*id);
					logical.get_node(*id).iter_fanout(WireColour::Red)
				})
				.into_iter()
				.flatten();
			let ld_out_wire_fanout_green_iter = ld_comb
				.fanout_green
				.first() // Option<NodeId>
				.map(|id| {
					logical.assert_is_wire_sum(*id);
					logical.get_node(*id).iter_fanout(WireColour::Green)
				})
				.into_iter()
				.flatten();
			ld_in_wire_fanin_red_iter
				.chain(ld_in_wire_fanin_green_iter)
				.chain(ld_out_wire_fanout_red_iter)
				.chain(ld_out_wire_fanout_green_iter)
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
					if self.place_comb_physical(placement_pos, id, logical).is_ok() {
						good = true;
						break;
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
						if self
							.place_comb_physical(
								(offset.0 * offset_factor, offset.1 * offset_factor),
								id,
								logical,
							)
							.is_ok()
						{
							good = true;
							break;
						};
					}
					if offset_factor > 100.0 {
						assert!(false, "Placement failed");
					}
					offset_factor += 1.0;
				}
			}
		}
		for fanout_wire_id in self
			.get_logical(id, logical)
			.iter_fanout(WireColour::Red)
			.chain(self.get_logical(id, logical).iter_fanout(WireColour::Green))
		{
			let fo_node = logical.get_node(*fanout_wire_id);
			match fo_node.function {
				ld::NodeFunction::WireSum(c) => {
					for fanout_combinator in fo_node.iter_fanout(c) {
						self.recurse_place_comb(
							*self.idx_combs.get(fanout_combinator).unwrap(),
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
		for fanin_wire_id in self.get_logical(id, logical).fanin_red.iter() {
			let fo_node = logical.get_node(*fanin_wire_id);
			match fo_node.function {
				ld::NodeFunction::WireSum(c) => {
					for fanin_combinator in fo_node.iter_fanin(c) {
						self.recurse_place_comb(
							*self.idx_combs.get(fanin_combinator).unwrap(),
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
		let mut global_edges: Vec<(ld::NodeId, Vec<(bool, ld::NodeId, bool, ld::NodeId)>)> = vec![];
		logical.for_all(|_, ld_node| {
			match &ld_node.function {
				ld::NodeFunction::WireSum(c) => {
					let mut edges = vec![];
					for id_i in ld_node.iter_fanout(*c) {
						for id_j in ld_node.iter_fanin(*c) {
							let comb_i = &self.combs[self.idx_combs.get(id_i).unwrap().0];
							let comb_j = &self.combs[self.idx_combs.get(id_j).unwrap().0];
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
								let comb_i = &self.combs[self.idx_combs.get(id_i).unwrap().0];
								let comb_j = &self.combs[self.idx_combs.get(id_j).unwrap().0];
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
								let comb_i = &self.combs[self.idx_combs.get(id_i).unwrap().0];
								let comb_j = &self.combs[self.idx_combs.get(id_j).unwrap().0];
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
	}

	fn connect_wire(
		&mut self,
		ld_id_wire: ld::NodeId,
		fanin_a: bool,
		id_comb_a: CombinatorId,
		fanin_b: bool,
		id_comb_b: CombinatorId,
		logical: &LogicalDesign,
	) {
		let id_wire = self.wires.len();
		let comb_a = &self.combs[id_comb_a.0];
		let comb_b = &self.combs[id_comb_b.0];
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
			logic: ld_id_wire,
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
		let mut rects = vec![];
		for c in &self.combs {
			let node = ld.get_node(c.logic);
			let mut x = (c.position.0 * (SCALE + 2.0)) as i32;
			let y = (c.position.1 * (SCALE + 2.0)) as i32;
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
		todo!();
		for c in &self.combs {
			let ld_comb = ld.get_node(c.logic);
			if ld_comb.is_constant() {
				let n1 = ld.get_fanout_network(c.logic, WireColour::Red);
				let n2 = ld.get_fanout_network(c.logic, WireColour::Green);
				let n3 = self
					.get_network(c.id, TerminalId::output_constant(WireColour::Red))
					.iter()
					.map(|(coid, _)| self.combs[coid.0].logic)
					.collect();
				let n4 = self
					.get_network(c.id, TerminalId::output_constant(WireColour::Green))
					.iter()
					.map(|(coid, _)| self.combs[coid.0].logic)
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
					.map(|(coid, _)| self.combs[coid.0].logic)
					.collect();
				let n4 = self
					.get_network(c.id, TerminalId::input(WireColour::Green))
					.iter()
					.map(|(coid, _)| self.combs[coid.0].logic)
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
					.map(|(coid, _)| self.combs[coid.0].logic)
					.collect();
				let n4 = self
					.get_network(c.id, TerminalId::output_combinator(WireColour::Green))
					.iter()
					.map(|(coid, _)| self.combs[coid.0].logic)
					.collect();
				assert_eq!(n1, n3);
				assert_eq!(n2, n4);
			}
		}
	}

	pub(crate) fn get_network(
		&self,
		coid: CombinatorId,
		terminal: TerminalId,
	) -> HashSet<(CombinatorId, TerminalId)> {
		let mut queue = LinkedList::new();
		let mut seen: HashSet<WireId> = HashSet::new();
		let mut retval = HashSet::new();
		let add_wires_on_terminal =
			|coid: CombinatorId, terminal: TerminalId, queue: &mut LinkedList<WireId>| {
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

	fn get_bfs_order(&self, coid: CombinatorId, ld: &LogicalDesign) -> Vec<CombinatorId> {
		let connections = self.get_connectivity_as_vec(ld);
		let mut queue = LinkedList::new();
		let mut seen_comb: HashSet<CombinatorId> = HashSet::new();
		let mut retval = vec![];
		queue.push_back(coid);
		retval.push(coid);
		while !queue.is_empty() {
			let coid = queue.pop_front().unwrap();
			if seen_comb.contains(&coid) {
				continue;
			}
			seen_comb.insert(coid);
			retval.push(coid);
			for coid_faninfanout in &connections[coid.0] {
				queue.push_back(*coid_faninfanout);
			}
		}
		retval
	}

	pub fn get_connectivity_as_vec(&self, ld: &LogicalDesign) -> Vec<Vec<CombinatorId>> {
		let mut connections = vec![vec![]; self.combs.len()];
		for cell in &self.combs {
			for ldid in ld.get_connected_combs(cell.logic) {
				connections[cell.id.0].push(*self.idx_combs.get(&ldid).unwrap());
			}
		}
		connections
	}

	pub fn get_connectivity_as_matrix(
		&self,
		ld: &LogicalDesign,
		triangular: bool,
	) -> Vec<Vec<bool>> {
		let mut connections = vec![vec![false; self.combs.len()]; self.combs.len()];
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

fn ripup_replace_method(
	rng: &mut StdRng,
	assignments: &Vec<(usize, usize)>,
	_block_counts: &Vec<Vec<i32>>,
	new_assignments: &mut Vec<(usize, usize)>,
	new_block_counts: &mut Vec<Vec<i32>>,
	side_length: usize,
	max_density: i32,
) {
	let num_cells = assignments.len();
	let ripup = exponential_distr_sample(rng.random(), 0.02, num_cells);
	let mut ripup_cells = vec![];
	while ripup_cells.len() != ripup {
		let cell = rng.random_range(0..num_cells);
		if ripup_cells.contains(&cell) {
			continue;
		}
		ripup_cells.push(cell);
	}

	for cell in &ripup_cells {
		let ass = assignments[*cell];
		new_block_counts[ass.0][ass.1] -= 1;
	}

	for cell in &ripup_cells {
		loop {
			let (cx, cy) = (
				rng.random_range(0..side_length) / 2 * 2,
				rng.random_range(0..side_length),
			);
			if new_block_counts[cx][cy] < max_density {
				new_assignments[*cell] = (cx, cy);
				new_block_counts[cx][cy] += 1;
				break;
			}
		}
	}
}

fn swap_local_method(
	rng: &mut StdRng,
	assignments: &Vec<(usize, usize)>,
	_block_counts: &Vec<Vec<i32>>,
	new_assignments: &mut Vec<(usize, usize)>,
	new_block_counts: &mut Vec<Vec<i32>>,
	side_length: usize,
	_max_density: i32,
) {
	let num_cells = assignments.len();
	let n_swap = exponential_distr_sample(rng.random(), 0.02, num_cells);
	let mut swap_cells = vec![];
	while swap_cells.len() != n_swap {
		let cell = rng.random_range(0..num_cells);
		if swap_cells.contains(&cell) {
			continue;
		}
		swap_cells.push(cell);
	}

	for cell in &swap_cells {
		let (cx, cy) = new_assignments[*cell];
		loop {
			let pick_x: i32 = rng.random_range(0..=1) * 2;
			let pick_y: i32 = rng.random_range(-1..=1);
			let want_x = cx as isize + pick_x as isize;
			let want_y = cy as isize + pick_y as isize;
			let want_assignment = (want_x, want_y);
			if (cx as isize, cy as isize) == want_assignment
				|| want_assignment.0 < 0
				|| want_assignment.0 >= side_length as isize
				|| want_assignment.1 < 0
				|| want_assignment.1 >= side_length as isize
			{
				continue;
			}
			let want_assignment = (want_x as usize, want_y as usize);
			let mut did_swap = false;
			for (cell2, cxy2) in new_assignments.iter().enumerate() {
				if *cxy2 != want_assignment {
					continue;
				}
				did_swap = true;
				new_assignments.swap(*cell, cell2);
				break;
			}
			if !did_swap {
				new_assignments[*cell] = want_assignment;
				new_block_counts[want_assignment.0][want_assignment.1] += 1;
				new_block_counts[cx][cy] -= 1;
			}
			break;
		}
	}
}

fn swap_random_method(
	rng: &mut StdRng,
	assignments: &Vec<(usize, usize)>,
	_block_counts: &Vec<Vec<i32>>,
	new_assignments: &mut Vec<(usize, usize)>,
	_new_block_counts: &mut Vec<Vec<i32>>,
	_side_length: usize,
	_max_density: i32,
) {
	let num_cells = assignments.len();
	let swap_count = exponential_distr_sample(rng.random(), 0.02, num_cells) / 2 * 2;
	let mut picked = vec![];

	while picked.len() < swap_count {
		let c = rng.random_range(0..num_cells);
		if !picked.contains(&c) {
			picked.push(c);
		}
	}

	for i in (0..swap_count).step_by(2) {
		let c1 = picked[i];
		let c2 = picked[i + 1];
		let (x1, y1) = new_assignments[c1];
		let (x2, y2) = new_assignments[c2];
		new_assignments[c1] = (x2, y2);
		new_assignments[c2] = (x1, y1);
	}
}

fn ripup_range_method(
	rng: &mut StdRng,
	assignments: &Vec<(usize, usize)>,
	_block_counts: &Vec<Vec<i32>>,
	new_assignments: &mut Vec<(usize, usize)>,
	new_block_counts: &mut Vec<Vec<i32>>,
	side_length: usize,
	max_density: i32,
) {
	let num_cells = assignments.len();
	let region_w = rng.random_range(1..=side_length / 4);
	let region_h = rng.random_range(1..=side_length / 4);

	let sx = rng.random_range(0..(side_length - region_w).max(1));
	let sy = rng.random_range(0..(side_length - region_h).max(1));

	let mut ripup_cells = vec![];
	for cell in 0..num_cells {
		let (cx, cy) = new_assignments[cell];
		if cx >= sx && cx < sx + region_w && cy >= sy && cy < sy + region_h {
			ripup_cells.push(cell);
			new_block_counts[cx][cy] -= 1;
		}
	}

	for cell in &ripup_cells {
		loop {
			let (cx, cy) = (
				rng.random_range(0..side_length) / 2 * 2,
				rng.random_range(0..side_length),
			);
			if new_block_counts[cx][cy] < max_density {
				new_assignments[*cell] = (cx, cy);
				new_block_counts[cx][cy] += 1;
				break;
			}
		}
	}
}

fn crack_in_two_method(
	rng: &mut StdRng,
	assignments: &Vec<(usize, usize)>,
	_block_counts: &Vec<Vec<i32>>,
	new_assignments: &mut Vec<(usize, usize)>,
	new_block_counts: &mut Vec<Vec<i32>>,
	side_length: usize,
	max_density: i32,
) {
	let num_cells = assignments.len();
	if rng.random_bool(0.5) {
		if rng.random_bool(0.5) {
			let line_y = rng.random_range(3..side_length - 3);
			let mut ripup_cells = vec![];
			for cell in 0..num_cells {
				let (cx, cy) = new_assignments[cell];
				if cy == 0 {
					continue;
				}
				if cy < line_y {
					ripup_cells.push(cell);
					new_block_counts[cx][cy] -= 1;
				}
			}
			ripup_cells.sort_by(|a, b| new_assignments[*a].1.cmp(&new_assignments[*b].1));
			for cell in ripup_cells {
				let (cx, cy) = new_assignments[cell];
				if new_block_counts[cx][cy - 1] < max_density {
					new_block_counts[cx][cy - 1] += 1;
					new_assignments[cell] = (cx, cy - 1);
				} else {
					new_block_counts[cx][cy] += 1;
				}
			}
		} else {
			let line_y = rng.random_range(3..side_length - 3);
			let mut ripup_cells = vec![];
			for cell in 0..num_cells {
				let (cx, cy) = new_assignments[cell];
				if cy == side_length - 1 {
					continue;
				}
				if cy > line_y {
					ripup_cells.push(cell);
					new_block_counts[cx][cy] -= 1;
				}
			}
			ripup_cells.sort_by(|a, b| new_assignments[*b].1.cmp(&new_assignments[*a].1));
			for cell in ripup_cells {
				let (cx, cy) = new_assignments[cell];
				if new_block_counts[cx][cy + 1] < max_density {
					new_block_counts[cx][cy + 1] += 1;
					new_assignments[cell] = (cx, cy + 1);
				} else {
					new_block_counts[cx][cy] += 1;
				}
			}
		}
	} else if rng.random_bool(0.5) {
		let line_x = rng.random_range(4..side_length - 4) / 2 * 2;
		let mut ripup_cells = Vec::new();
		for cell in 0..num_cells {
			let (cx, cy) = new_assignments[cell];
			if cx == 0 {
				continue;
			}
			if cx > line_x {
				ripup_cells.push(cell);
				new_block_counts[cx][cy] -= 1;
			}
		}
		ripup_cells.sort_by(|a, b| new_assignments[*b].0.cmp(&new_assignments[*a].0));

		for cell in ripup_cells {
			let (cx, cy) = new_assignments[cell];
			if new_block_counts[cx - 2][cy] < max_density {
				new_block_counts[cx - 2][cy] += 1;
				new_assignments[cell] = (cx - 2, cy);
			} else {
				new_block_counts[cx][cy] += 1;
			}
		}
	} else {
		let line_x = rng.random_range(4..(side_length - 4)) / 2 * 2;
		let mut ripup_cells = Vec::new();
		for cell in 0..num_cells {
			let (cx, cy) = new_assignments[cell];
			if cx == side_length - 1 {
				continue;
			}
			if cx < line_x {
				ripup_cells.push(cell);
				new_block_counts[cx][cy] -= 1;
			}
		}
		ripup_cells.sort_by(|a, b| new_assignments[*a].0.cmp(&new_assignments[*b].0));

		for cell in ripup_cells {
			let (cx, cy) = new_assignments[cell];
			if new_block_counts[cx + 2][cy] < max_density {
				new_block_counts[cx + 2][cy] += 1;
				new_assignments[cell] = (cx + 2, cy);
			} else {
				new_block_counts[cx][cy] += 1;
			}
		}
	}
}

fn slide_puzzle_method(
	rng: &mut StdRng,
	assignments: &Vec<(usize, usize)>,
	_block_counts: &Vec<Vec<i32>>,
	new_assignments: &mut Vec<(usize, usize)>,
	new_block_counts: &mut Vec<Vec<i32>>,
	side_length: usize,
	max_density: i32,
) {
	let num_cells = assignments.len();
	let start = rng.random_range(1..side_length - 3);
	let end = rng.random_range(3..side_length - 3);
	let (start, end) = (start.min(end), start.max(end));
	if rng.random_bool(0.5) {
		if rng.random_bool(0.5) {
			let line_y = rng.random_range(3..side_length - 3);
			let mut ripup_cells = vec![];
			for cell in 0..num_cells {
				let (cx, cy) = new_assignments[cell];
				if cy == 0 {
					continue;
				}
				if cy < line_y && cx >= start && cx < end {
					ripup_cells.push(cell);
					new_block_counts[cx][cy] -= 1;
				}
			}
			ripup_cells.sort_by(|a, b| new_assignments[*a].1.cmp(&new_assignments[*b].1));
			for cell in ripup_cells {
				let (cx, cy) = new_assignments[cell];
				if new_block_counts[cx][cy - 1] < max_density {
					new_block_counts[cx][cy - 1] += 1;
					new_assignments[cell] = (cx, cy - 1);
				} else {
					new_block_counts[cx][cy] += 1;
				}
			}
		} else {
			let line_y = rng.random_range(3..side_length - 3);
			let mut ripup_cells = vec![];
			for cell in 0..num_cells {
				let (cx, cy) = new_assignments[cell];
				if cy == side_length - 1 {
					continue;
				}
				if cy > line_y && cx >= start && cx < end {
					ripup_cells.push(cell);
					new_block_counts[cx][cy] -= 1;
				}
			}
			ripup_cells.sort_by(|a, b| new_assignments[*b].1.cmp(&new_assignments[*a].1));
			for cell in ripup_cells {
				let (cx, cy) = new_assignments[cell];
				if new_block_counts[cx][cy + 1] < max_density {
					new_block_counts[cx][cy + 1] += 1;
					new_assignments[cell] = (cx, cy + 1);
				} else {
					new_block_counts[cx][cy] += 1;
				}
			}
		}
	} else if rng.random_bool(0.5) {
		let line_x = rng.random_range(4..side_length - 4) / 2 * 2;
		let mut ripup_cells = Vec::new();
		for cell in 0..num_cells {
			let (cx, cy) = new_assignments[cell];
			if cx == 0 {
				continue;
			}
			if cx > line_x && cy >= start && cy < end {
				ripup_cells.push(cell);
				new_block_counts[cx][cy] -= 1;
			}
		}
		ripup_cells.sort_by(|a, b| new_assignments[*b].0.cmp(&new_assignments[*a].0));

		for cell in ripup_cells {
			let (cx, cy) = new_assignments[cell];
			if new_block_counts[cx - 2][cy] < max_density {
				new_block_counts[cx - 2][cy] += 1;
				new_assignments[cell] = (cx - 2, cy);
			} else {
				new_block_counts[cx][cy] += 1;
			}
		}
	} else {
		let line_x = rng.random_range(4..(side_length - 4)) / 2 * 2;
		let mut ripup_cells = Vec::new();
		for cell in 0..num_cells {
			let (cx, cy) = new_assignments[cell];
			if cx == side_length - 1 {
				continue;
			}
			if cx < line_x && cy >= start && cy < end {
				ripup_cells.push(cell);
				new_block_counts[cx][cy] -= 1;
			}
		}
		ripup_cells.sort_by(|a, b| new_assignments[*a].0.cmp(&new_assignments[*b].0));

		for cell in ripup_cells {
			let (cx, cy) = new_assignments[cell];
			if new_block_counts[cx + 2][cy] < max_density {
				new_block_counts[cx + 2][cy] += 1;
				new_assignments[cell] = (cx + 2, cy);
			} else {
				new_block_counts[cx][cy] += 1;
			}
		}
	}
}

fn slide_puzzle_method_worst_cells(
	rng: &mut StdRng,
	assignments: &Vec<(usize, usize)>,
	_block_counts: &Vec<Vec<i32>>,
	new_assignments: &mut Vec<(usize, usize)>,
	new_block_counts: &mut Vec<Vec<i32>>,
	side_length: usize,
	max_density: i32,
) {
	let num_cells = assignments.len();
	let mut interesting_cells = vec![];
	let mut interesting_cells_assignment = vec![];
	for (idx, (cx, cy)) in new_assignments.iter().enumerate() {
		if new_block_counts[*cx][*cy] > max_density
			&& interesting_cells_assignment.last() != Some(&(cx, cy))
		{
			interesting_cells.push(idx);
			interesting_cells_assignment.push((cx, cy));
		}
	}
	for picked_cell in interesting_cells {
		if new_assignments[picked_cell].1 != 0
			&& new_assignments[picked_cell].1 != side_length - 1
			&& rng.random_bool(0.5)
		{
			let line_y = new_assignments[picked_cell].1;
			if rng.random_bool(0.5) {
				let mut ripup_cells = vec![];
				for cell in 0..num_cells {
					let (cx, cy) = new_assignments[cell];
					if cy == 0 {
						continue;
					}
					if cy < line_y && cx == assignments[picked_cell].0 {
						ripup_cells.push(cell);
						new_block_counts[cx][cy] -= 1;
					}
				}
				ripup_cells.sort_by(|a, b| new_assignments[*a].1.cmp(&new_assignments[*b].1));
				for cell in ripup_cells {
					let (cx, cy) = new_assignments[cell];
					if new_block_counts[cx][cy - 1] < max_density {
						new_block_counts[cx][cy - 1] += 1;
						new_assignments[cell] = (cx, cy - 1);
					} else {
						new_block_counts[cx][cy] += 1;
					}
				}
				let (cx, cy) = new_assignments[picked_cell];
				if new_block_counts[cx][cy - 1] < max_density {
					new_block_counts[cx][cy - 1] += 1;
					new_block_counts[cx][cy] -= 1;
					new_assignments[picked_cell] = (cx, cy - 1);
				}
			} else {
				let mut ripup_cells = vec![];
				for cell in 0..num_cells {
					let (cx, cy) = new_assignments[cell];
					if cy == side_length - 1 {
						continue;
					}
					if cy > line_y && cx == assignments[picked_cell].0 {
						ripup_cells.push(cell);
						new_block_counts[cx][cy] -= 1;
					}
				}
				ripup_cells.sort_by(|a, b| new_assignments[*b].1.cmp(&new_assignments[*a].1));
				for cell in ripup_cells {
					let (cx, cy) = new_assignments[cell];
					if new_block_counts[cx][cy + 1] < max_density {
						new_block_counts[cx][cy + 1] += 1;
						new_assignments[cell] = (cx, cy + 1);
					} else {
						new_block_counts[cx][cy] += 1;
					}
				}
				let (cx, cy) = new_assignments[picked_cell];
				if new_block_counts[cx][cy + 1] < max_density {
					new_block_counts[cx][cy + 1] += 1;
					new_block_counts[cx][cy] -= 1;
					new_assignments[picked_cell] = (cx, cy + 1);
				}
			}
		} else if new_assignments[picked_cell].0 != 0
			&& new_assignments[picked_cell].0 < side_length - 2
		{
			let line_x = new_assignments[picked_cell].0;
			if rng.random_bool(0.5) {
				let mut ripup_cells = Vec::new();
				for cell in 0..num_cells {
					let (cx, cy) = new_assignments[cell];
					if cx == 0 {
						continue;
					}
					if cx > line_x && cy == assignments[picked_cell].1 {
						ripup_cells.push(cell);
						new_block_counts[cx][cy] -= 1;
					}
				}
				ripup_cells.sort_by(|a, b| new_assignments[*b].0.cmp(&new_assignments[*a].0));

				for cell in ripup_cells {
					let (cx, cy) = new_assignments[cell];
					if new_block_counts[cx - 2][cy] < max_density {
						new_block_counts[cx - 2][cy] += 1;
						new_assignments[cell] = (cx - 2, cy);
					} else {
						new_block_counts[cx][cy] += 1;
					}
				}
				let (cx, cy) = new_assignments[picked_cell];
				if new_block_counts[cx - 2][cy] < max_density {
					new_block_counts[cx - 2][cy] += 1;
					new_block_counts[cx][cy] -= 1;
					new_assignments[picked_cell] = (cx - 2, cy);
				}
			} else {
				let mut ripup_cells = Vec::new();
				for cell in 0..num_cells {
					let (cx, cy) = new_assignments[cell];
					if cx == side_length - 1 {
						continue;
					}
					if cx < line_x && cy == assignments[picked_cell].1 {
						ripup_cells.push(cell);
						new_block_counts[cx][cy] -= 1;
					}
				}
				ripup_cells.sort_by(|a, b| new_assignments[*a].0.cmp(&new_assignments[*b].0));

				for cell in ripup_cells {
					let (cx, cy) = new_assignments[cell];
					if new_block_counts[cx + 2][cy] < max_density {
						new_block_counts[cx + 2][cy] += 1;
						new_assignments[cell] = (cx + 2, cy);
					} else {
						new_block_counts[cx][cy] += 1;
					}
				}
				let (cx, cy) = new_assignments[picked_cell];
				if new_block_counts[cx + 2][cy] < max_density {
					new_block_counts[cx + 2][cy] += 1;
					new_block_counts[cx][cy] -= 1;
					new_assignments[picked_cell] = (cx + 2, cy);
				}
			}
		}
	}
}

fn overflowing_cells_swap_local_method(
	rng: &mut StdRng,
	_assignments: &Vec<(usize, usize)>,
	_block_counts: &Vec<Vec<i32>>,
	new_assignments: &mut Vec<(usize, usize)>,
	new_block_counts: &mut Vec<Vec<i32>>,
	side_length: usize,
	_max_density: i32,
) {
	let mut swap_cells = vec![];
	for (idx, (cx, cy)) in new_assignments.iter().enumerate() {
		if new_block_counts[*cx][*cy] > 1 {
			swap_cells.push(idx);
		}
	}

	for cell in &swap_cells {
		let (cx, cy) = new_assignments[*cell];
		loop {
			let pick_x: i32 = rng.random_range(0..=1) * 2;
			let pick_y: i32 = rng.random_range(-1..=1);
			let want_x = cx as isize + pick_x as isize;
			let want_y = cy as isize + pick_y as isize;
			let want_assignment = (want_x, want_y);
			if (cx as isize, cy as isize) == want_assignment
				|| want_assignment.0 < 0
				|| want_assignment.0 >= side_length as isize
				|| want_assignment.1 < 0
				|| want_assignment.1 >= side_length as isize
			{
				continue;
			}
			let want_assignment = (want_x as usize, want_y as usize);
			let mut did_swap = false;
			for (cell2, cxy2) in new_assignments.iter().enumerate() {
				if *cxy2 != want_assignment {
					continue;
				}
				did_swap = true;
				new_assignments.swap(*cell, cell2);
				break;
			}
			if !did_swap {
				new_assignments[*cell] = want_assignment;
				new_block_counts[want_assignment.0][want_assignment.1] += 1;
				new_block_counts[cx][cy] -= 1;
			}
			break;
		}
	}
}

fn exponential_distr_sample(u: f64, median: f64, num_cells: usize) -> usize {
	let lambda = std::f64::consts::LN_2 / (median * (num_cells as f64));
	let x = -1.0 / lambda * u.ln();
	let mut r = x.round() as isize;
	if r < 1 {
		r = 1;
	} else if r > (num_cells as isize) {
		r = num_cells as isize;
	}
	r as usize
}

#[cfg(test)]
mod test {

	use std::{fs::File, io::Write};

	use crate::{logical_design::get_large_logical_design, serializable_design};

	use super::*;
	#[test]
	fn new() {
		let mut p = PhysicalDesign::new();
		let l = ld::get_simple_logical_design();
		p.build_from(&l, PlacementStrategy::default());
	}

	#[test]
	fn n_combs() {
		let mut p = PhysicalDesign::new();
		let l = get_large_logical_design(200);
		p.build_from(&l, PlacementStrategy::ConnectivityAveraging);
		p.save_svg(&l, "svg/n_combs_connectivity_averaging.svg");
	}

	#[test]
	fn simple_connectivity_averaging() {
		let mut p = PhysicalDesign::new();
		let l = ld::get_simple_logical_design();
		println!("{l}");
		p.build_from(&l, PlacementStrategy::ConnectivityAveraging);
	}

	#[test]
	fn complex_40_mcmc_dense() {
		let mut p = PhysicalDesign::new();
		let l = ld::get_complex_40_logical_design();
		p.build_from(&l, PlacementStrategy::MCMCSADense);
		p.save_svg(&l, "svg/complex40_mcmc_coarse15.svg");
	}

	#[test]
	fn memory_n_mcmc_dense() {
		let mut p = PhysicalDesign::new();
		let l = ld::get_large_memory_test_design(1_000);
		p.build_from(&l, PlacementStrategy::MCMCSADense);
		p.save_svg(&l, "svg/memory_n_mcmc_dense.svg");
	}

	#[test]
	fn dense_memory_n_mcmc_dense() {
		let mut p = PhysicalDesign::new();
		let l = ld::get_large_dense_memory_test_design(1_024);
		p.build_from(&l, PlacementStrategy::MCMCSADense);
		p.save_svg(&l, "svg/dense_memory_n_mcmc_dense.svg");

		let mut s = serializable_design::SerializableDesign::new();
		s.build_from(&p, &l);
		let blueprint_json: String = serde_json::to_string(&s).unwrap();
		let mut output = File::create("./output/dense_memory_n_mcmc_dense.json").unwrap();
		output.write_all(blueprint_json.as_bytes()).unwrap();
	}

	#[test]
	fn synthetic_n_mcmc_dense() {
		let mut p = PhysicalDesign::new();
		let l = get_large_logical_design(200);
		p.build_from(&l, PlacementStrategy::MCMCSADense);
		p.save_svg(&l, "svg/synthetic_n_mcmc_dense.svg");
	}

	#[test]
	fn synthetic_n_mcmc_dense_parallel() {
		let mut p = PhysicalDesign::new();
		let l = get_large_logical_design(200);
		//p.build_from(&l, PlacementStrategy::MCMCSADenseParallel);
		//p.save_svg(&l, "svg/synthetic_n_mcmc_dense_parallel.svg");
	}
}

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
use std::{isize, usize};

use crate::physical_planning::*;
use crate::{
	logical_design::{self as ld, LogicalDesign, WireColour},
	svg::SVG,
};

#[derive(Debug, Clone, Copy, Default, clap::ValueEnum)]
pub enum PlacementStrategy {
	ConnectivityAveraging,
	#[default]
	MCMCSADense,
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
					match self.solve_as_mcmc_dense(logical, 4.0, &initializations, None) {
						Ok(pos) => pos,
						Err(e) => {
							println!("WARN: MCMC failed to place with scale 1.6");
							println!("WARN: {}", e.0);
							let _ = self.place_combs_physical_dense(&e.1, logical);
							self.connect_combs(logical);
							self.save_svg(logical, format!("./svg/failed{}.svg", 4.0).as_str());
							panic!("failed to place");
						}
					};
				if let Err(_) = self.place_combs_physical_dense(&comb_positions, logical) {
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
			}
		}
	}

	fn place_combs_physical_dense(
		&mut self,
		comb_positions: &Vec<(usize, usize)>,
		logical: &LogicalDesign,
	) -> Result<(), String> {
		let mut failure = Ok(());
		for (idx, (x, y)) in comb_positions.iter().enumerate() {
			let p = (*x as f64, *y as f64);
			let res2 = self.place_comb_physical(p, CombinatorId(idx), logical);
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
		let connections_per_node = self
			.get_connectivity_as_vec(ld)
			.into_iter()
			.map(|x| x.into_iter().map(|c| c.0).collect_vec())
			.collect_vec();

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
				mthd!(10, false, true, swap_random_energy_method),
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
				best_cost = new.compute_cost(&connections, max_density);
				best = new.clone();
				curr = new;
				best_cost = new_cost;
				assignments_cost = new_cost;
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
	) -> Result<(), String> {
		let pos_start = (position.0 as i32, position.1 as i32);
		let comb_to_place = &mut self.combs[id_to_place.0];
		let ld_node = logical.get_node(comb_to_place.logic);
		let hop_spec = ld_node.function.wire_hop_type().wire_hop_spec();
		for x in 0..hop_spec.dim.0 {
			for y in 0..hop_spec.dim.1 {
				let key = (pos_start.0 + x, pos_start.1 + y);
				if self.space.contains_key(&key) {
					return Result::Err(format!(
						"Sub-cell ({x}, {y}) overlaps with an existing cell."
					));
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
		unreachable!();
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
		let mut retval = Vec::with_capacity(self.combs.len());
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
		let l = ld::get_large_memory_test_design(2_000);
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
	fn sweep_synthetic_n_mcmc_dense() {
		for x in (20..=200).step_by(20) {
			println!("==============={x}===============");
			let mut p = PhysicalDesign::new();
			let l = get_large_logical_design(x);
			p.build_from(&l, PlacementStrategy::MCMCSADense);
		}
	}

	#[test]
	fn synthetic_n_mcmc_dense() {
		let mut p = PhysicalDesign::new();
		let l = get_large_logical_design(500);
		p.build_from(&l, PlacementStrategy::MCMCSADense);
		p.save_svg(&l, "svg/synthetic_n_mcmc_dense.svg");
	}

	#[test]
	fn synthetic_2d_n_mcmc_dense() {
		let mut p = PhysicalDesign::new();
		let l = get_large_logical_design_2d(50);
		p.build_from(&l, PlacementStrategy::MCMCSADense);
		p.save_svg(&l, "svg/synthetic_2d_n_mcmc_dense.svg");
	}
}

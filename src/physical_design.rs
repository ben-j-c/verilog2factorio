use core::panic;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};
use std::{
	cmp::Ordering,
	collections::{HashMap, HashSet, LinkedList},
	hash::Hash,
	vec,
};

use good_lp::{constraint, variable, ResolutionError, Solution, SolverModel};

use crate::{
	logical_design::{self as ld, LogicalDesign, WireColour},
	svg::SVG,
};

#[derive(Debug, Clone, Copy, Default, clap::ValueEnum)]
pub enum PlacementStrategy {
	#[default]
	ConnectivityAveraging,
	ILP,
	ILPCoarse15,
	ILPCoarse8,
	MCMCSACoarse15,
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

	fn place_combs(&mut self, logical: &LogicalDesign, placement_strategy: PlacementStrategy) {
		match placement_strategy {
			PlacementStrategy::ConnectivityAveraging => {
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
					c.position.0 -= min_x - 1.0;
					c.position.1 -= min_y - 1.0;
				}
			}
			PlacementStrategy::ILP => {
				let scale_factors = [1.0, 1.1, 1.5, 2.0, 4.0];
				for scale in scale_factors {
					let comb_positions = match self.solve_as_ilp_dense(logical, scale) {
						Ok(pos) => pos,
						Err(e) => {
							println!("WARN: ILP failed to place with scale {scale}");
							println!("WARN: {e}");
							continue;
						}
					};
					for (idx, (x, y)) in comb_positions.iter().enumerate() {
						self.place_comb_physical((x * 2.0, *y), CombinatorId(idx), logical)
							.unwrap();
					}
					return;
				}
				panic!("Well we tried a very large area and it didn't work.");
			}
			PlacementStrategy::ILPCoarse15 => {
				let scale_factors = [1.0, 1.1, 1.5, 2.0, 4.0];
				for scale in scale_factors {
					let comb_positions = match self.solve_as_ilp_coarse15(logical, scale) {
						Ok(pos) => pos,
						Err(e) => {
							println!("WARN: ILP failed to place with scale {scale}");
							println!("WARN: {e}");
							continue;
						}
					};
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
					return;
				}
				panic!("Well we tried a very large area and it didn't work.");
			}
			PlacementStrategy::ILPCoarse8 => {
				let scale_factors = [1.0, 1.1, 1.5, 2.0, 4.0];
				for scale in scale_factors {
					let comb_positions = match self.solve_as_ilp_coarse8(logical, scale) {
						Ok(pos) => pos,
						Err(e) => {
							println!("WARN: ILP failed to place with scale {scale}");
							println!("WARN: {e}");
							continue;
						}
					};
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
							(*x * 2 + (allocated_resources % 2)) as f64 * 2.0,
							(*y * 4 + allocated_resources / 2) as f64,
						);
						self.place_comb_physical(p, CombinatorId(idx), logical)
							.unwrap();
					}
					return;
				}
				panic!("Well we tried a very large area and it didn't work.");
			}
			PlacementStrategy::MCMCSACoarse15 => {
				let scale_factors = [1.0, 2.0, 4.0, 8.0];
				let mut last_optimal = None;
				for scale in scale_factors {
					self.reset_place_route();
					let comb_positions =
						match self.solve_as_mcmc_coarse15(logical, scale, last_optimal) {
							Ok(pos) => pos,
							Err(e) => {
								println!("WARN: MCMC failed to place with scale {scale}");
								println!("WARN: {}", e.0);
								self.place_combs_physical_coarse15(&e.1, logical);
								self.connect_combs(logical);
								self.save_svg(
									logical,
									format!("./svg/failed{}.svg", scale).as_str(),
								);
								last_optimal = Some(e.1);
								continue;
							}
						};
					self.place_combs_physical_coarse15(&comb_positions, logical);
					return;
				}
				panic!("Well we tried a very large area and it didn't work.");
			}
		}
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

	/// This function should support up to 1000 nodes to be placed.
	fn solve_as_ilp_dense(
		&self,
		ld: &LogicalDesign,
		scale_factor: f64,
	) -> Result<Vec<(f64, f64)>, ResolutionError> {
		let side_length = (2.0 * self.combs.len() as f64 * scale_factor).sqrt().ceil() as i32;
		use good_lp as ilp;
		let mut problem = ilp::ProblemVariables::new();
		let mut connections = vec![vec![false; self.combs.len()]; self.combs.len()];
		let mut vars = vec![];
		for (i, comb_i) in self.combs.iter().enumerate() {
			for (j, comb_j) in self.combs.iter().enumerate() {
				if j >= i {
					break;
				}
				if ld.have_shared_wire(comb_i.logic, comb_j.logic) {
					connections[i][j] = true;
				}
			}
		}
		for _comb_i in self.combs.iter().enumerate() {
			let mut vars_plane = vec![vec![]; (side_length / 2) as usize];
			for x in 0..side_length / 2 {
				for _y in 0..side_length {
					vars_plane[x as usize].push(problem.add(variable().binary()));
				}
			}
			vars.push(vars_plane);
		}
		let mut problem = ilp::default_solver(problem.minimise(0));
		for i in 0..vars.len() {
			let mut expr = ilp::Expression::with_capacity((side_length * side_length / 2) as usize);
			for x in 0..vars[i].len() {
				for y in 0..vars[i][x].len() {
					expr.add_mul(1, vars[i][x][y]);
				}
			}
			problem.add_constraint(ilp::constraint!(expr == 1));
		}
		for x in 0..side_length as usize / 2 {
			for y in 0..side_length as usize {
				let mut expr = ilp::Expression::with_capacity(self.combs.len());
				for i in 0..self.combs.len() {
					expr.add_mul(1, vars[i][x][y]);
				}
				problem.add_constraint(ilp::constraint!(expr <= 1));
			}
		}
		for i in 0..vars.len() {
			for j in 0..i {
				if connections[i][j] {
					for x1 in 0..vars[i].len() {
						for y1 in 0..vars[i][x1].len() {
							for x2 in 0..vars[j].len() {
								for y2 in 0..vars[j][x2].len() {
									let dist = (2 * x1 as i32 - 2 * x2 as i32).pow(2)
										+ (y1 as i32 - y2 as i32).pow(2);
									let distance = self.max_allowable_distance(
										ld,
										CombinatorId(i),
										CombinatorId(j),
									);
									if dist as f64 > distance * distance {
										problem.add_constraint(constraint!(
											vars[i][x1][y1] + vars[j][x2][y2] <= 1
										));
									}
								}
							}
						}
					}
				}
			}
		}

		match problem.solve() {
			Ok(solution) => {
				let mut pos_vec = vec![];
				for i in 0..vars.len() {
					for x in 0..vars[i].len() {
						for y in 0..vars[i][x].len() {
							if solution.value(vars[i][x][y]) == 1.0 {
								pos_vec.push((x as f64, y as f64));
							}
						}
					}
				}
				assert_eq!(pos_vec.len(), self.combs.len());
				Ok(pos_vec)
			}
			Err(e) => Err(e),
		}
	}

	fn solve_as_ilp_coarse15(
		&self,
		ld: &LogicalDesign,
		scale_factor: f64,
	) -> Result<Vec<(usize, usize)>, ResolutionError> {
		let side_length = (self.combs.len() as f64 * scale_factor / 15.0)
			.sqrt()
			.ceil() as i32;
		use good_lp as ilp;
		let mut problem = ilp::ProblemVariables::new();
		let mut connections = vec![vec![false; self.combs.len()]; self.combs.len()];
		let mut vars = vec![];
		for (i, comb_i) in self.combs.iter().enumerate() {
			for (j, comb_j) in self.combs.iter().enumerate() {
				if j >= i {
					break;
				}
				if ld.have_shared_wire(comb_i.logic, comb_j.logic) {
					connections[i][j] = true;
				}
			}
		}
		for _comb_i in self.combs.iter().enumerate() {
			let mut vars_plane = vec![vec![]; (side_length) as usize];
			for x in 0..side_length {
				for _y in 0..side_length {
					vars_plane[x as usize].push(problem.add(variable().binary()));
				}
			}
			vars.push(vars_plane);
		}
		let mut problem = ilp::default_solver(problem.minimise(0));
		for i in 0..vars.len() {
			let mut expr = ilp::Expression::with_capacity((side_length * side_length) as usize);
			for x in 0..vars[i].len() {
				for y in 0..vars[i][x].len() {
					expr.add_mul(1, vars[i][x][y]);
				}
			}
			problem.add_constraint(ilp::constraint!(expr == 1));
		}
		for x in 0..side_length as usize {
			for y in 0..side_length as usize {
				let mut expr = ilp::Expression::with_capacity(self.combs.len());
				for i in 0..self.combs.len() {
					expr.add_mul(1, vars[i][x][y]);
				}
				problem.add_constraint(ilp::constraint!(expr <= 15));
			}
		}
		for i in 0..vars.len() {
			for j in 0..i {
				if connections[i][j] {
					for x1 in 0..vars[i].len() {
						for y1 in 0..vars[i][x1].len() {
							for x2 in 0..vars[j].len() {
								for y2 in 0..vars[j][x2].len() {
									let dist = (x1 as i32 - 2 * x2 as i32).abs()
										+ (y1 as i32 - y2 as i32).abs();
									if dist > 1 {
										problem.add_constraint(constraint!(
											vars[i][x1][y1] + vars[j][x2][y2] <= 1
										));
									}
								}
							}
						}
					}
				}
			}
		}

		match problem.solve() {
			Ok(solution) => {
				let mut pos_vec = vec![];
				for i in 0..vars.len() {
					for x in 0..vars[i].len() {
						for y in 0..vars[i][x].len() {
							if solution.value(vars[i][x][y]) == 1.0 {
								pos_vec.push((x, y));
							}
						}
					}
				}
				assert_eq!(pos_vec.len(), self.combs.len());
				Ok(pos_vec)
			}
			Err(e) => Err(e),
		}
	}

	fn solve_as_ilp_coarse8(
		&self,
		ld: &LogicalDesign,
		scale_factor: f64,
	) -> Result<Vec<(usize, usize)>, ResolutionError> {
		let side_length = (self.combs.len() as f64 * scale_factor / 8.0).sqrt().ceil() as i32;
		use good_lp as ilp;
		let mut problem = ilp::ProblemVariables::new();
		let mut connections = vec![vec![false; self.combs.len()]; self.combs.len()];
		let mut vars = vec![];
		for (i, comb_i) in self.combs.iter().enumerate() {
			for (j, comb_j) in self.combs.iter().enumerate() {
				if j >= i {
					break;
				}
				if ld.have_shared_wire(comb_i.logic, comb_j.logic) {
					connections[i][j] = true;
				}
			}
		}
		for _comb_i in self.combs.iter().enumerate() {
			let mut vars_plane = vec![vec![]; (side_length) as usize];
			for x in 0..side_length {
				for _y in 0..side_length {
					vars_plane[x as usize].push(problem.add(variable().binary()));
				}
			}
			vars.push(vars_plane);
		}
		let mut problem = ilp::default_solver(problem.minimise(0));
		for i in 0..vars.len() {
			let mut expr = ilp::Expression::with_capacity((side_length * side_length) as usize);
			for x in 0..vars[i].len() {
				for y in 0..vars[i][x].len() {
					expr.add_mul(1, vars[i][x][y]);
				}
			}
			problem.add_constraint(ilp::constraint!(expr == 1));
		}
		for x in 0..side_length as usize {
			for y in 0..side_length as usize {
				let mut expr = ilp::Expression::with_capacity(self.combs.len());
				for i in 0..self.combs.len() {
					expr.add_mul(1, vars[i][x][y]);
				}
				problem.add_constraint(ilp::constraint!(expr <= 8));
			}
		}
		for i in 0..vars.len() {
			for j in 0..i {
				if connections[i][j] {
					for x1 in 0..vars[i].len() {
						for y1 in 0..vars[i][x1].len() {
							for x2 in 0..vars[j].len() {
								for y2 in 0..vars[j][x2].len() {
									let dx = (x1 as i32 - 2 * x2 as i32).abs();
									let dy = (y1 as i32 - 2 * y2 as i32).abs();
									if dx > 1 || dy > 1 {
										problem.add_constraint(constraint!(
											vars[i][x1][y1] + vars[j][x2][y2] <= 1
										));
									}
								}
							}
						}
					}
				}
			}
		}

		match problem.solve() {
			Ok(solution) => {
				let mut pos_vec = vec![];
				for i in 0..vars.len() {
					for x in 0..vars[i].len() {
						for y in 0..vars[i][x].len() {
							if solution.value(vars[i][x][y]) == 1.0 {
								pos_vec.push((x, y));
							}
						}
					}
				}
				assert_eq!(pos_vec.len(), self.combs.len());
				Ok(pos_vec)
			}
			Err(e) => Err(e),
		}
	}

	pub fn solve_as_mcmc_coarse15(
		&self,
		ld: &LogicalDesign,
		scale_factor: f64,
		init: Option<Vec<(usize, usize)>>,
	) -> Result<Vec<(usize, usize)>, (String, Vec<(usize, usize)>)> {
		let side_length = (self.combs.len() as f64 * scale_factor / 15.0)
			.sqrt()
			.ceil() as usize;
		let num_cells = self.combs.len();

		let mut connections = vec![];
		for i in 0..num_cells {
			for j in 0..i {
				if ld.have_shared_wire(self.combs[i].logic, self.combs[j].logic) {
					connections.push((i, j));
				}
			}
		}

		let compute_cost = |assign: &Vec<(usize, usize)>| -> f64 {
			let mut cost = 0.0;
			for &(i, j) in &connections {
				let (x_i, y_i) = assign[i];
				let (x_j, y_j) = assign[j];
				let dx = (x_i as isize - x_j as isize).abs();
				let dy = (y_i as isize - y_j as isize).abs();
				if dx + dy >= 1 {
					cost += (dx + dy).pow(2) as f64;
				}
			}
			cost
		};

		let mut rng = StdRng::seed_from_u64(0xCAFEBABE);
		let mut assignments = vec![(0, 0); num_cells];
		let mut block_counts = vec![vec![0; side_length]; side_length];

		if let Some(init) = init {
			assignments = init;
			for (cx, cy) in &assignments {
				block_counts[*cx][*cy] += 1;
			}
		} else {
			for cell in 0..num_cells {
				loop {
					let (cx, cy) = (
						rng.random_range(0..side_length),
						rng.random_range(0..side_length),
					);
					if block_counts[cx][cy] < 15 {
						assignments[cell] = (cx, cy);
						block_counts[cx][cy] += 1;
						break;
					}
				}
			}
		}

		let mut best_assign = assignments.clone();
		let mut best_block_counts = block_counts.clone();
		let mut best_cost = compute_cost(&assignments);

		let mut temp = 10.0;
		let cooling_rate = 0.99992;
		let mut iterations = 500000;

		let pick_ripup = |u: f64| -> usize {
			let lambda = std::f64::consts::LN_2 / (0.05 * (num_cells as f64));
			let x = -1.0 / lambda * u.ln();
			let mut r = x.round() as isize;
			if r < 1 {
				r = 1;
			} else if r > (num_cells as isize) {
				r = num_cells as isize;
			}
			r as usize
		};

		let mut step = 0;
		while step < iterations {
			let ripup = pick_ripup(rng.random());
			let mut ripup_cells = vec![];
			while ripup_cells.len() != ripup {
				let cell = rng.random_range(0..num_cells);
				if ripup_cells.contains(&cell) {
					continue;
				}
				ripup_cells.push(cell);
			}

			let mut new_block_counts = block_counts.clone();
			let mut new_assignments = assignments.clone();
			for cell in &ripup_cells {
				let ass = assignments[*cell];
				new_block_counts[ass.0][ass.1] -= 1;
			}

			for cell in &ripup_cells {
				loop {
					let (cx, cy) = (
						rng.random_range(0..side_length),
						rng.random_range(0..side_length),
					);
					if block_counts[cx][cy] < 15 {
						new_assignments[*cell] = (cx, cy);
						new_block_counts[cx][cy] += 1;
						break;
					}
				}
			}

			let new_cost = compute_cost(&new_assignments);
			let delta = new_cost - best_cost;

			if delta < 0.0 {
				best_cost = new_cost;
				best_assign = new_assignments.clone();
				assignments = new_assignments;
				block_counts = new_block_counts;
				println!("Best cost {}, temp {}", best_cost, temp);
				if iterations - step < iterations * 1 / 20 {
					println!("Boosting iterations {}", best_cost);
					iterations = iterations * 20 / 19;
				}
				if best_cost == 0.0 {
					break;
				}
			} else {
				let p = f64::exp(-delta / temp);
				if rng.random_bool(p) {
					assignments = new_assignments;
					block_counts = new_block_counts;
				}
			}

			temp *= cooling_rate;
			if temp < 1e-3 {
				temp = 1e-3;
			}
			step += 1;
		}

		for &(i, j) in &connections {
			let (x_i, y_i) = best_assign[i];
			let (x_j, y_j) = best_assign[j];
			let dist = ((x_i as isize - x_j as isize).abs() + (y_i as isize - y_j as isize).abs())
				as usize;
			if dist > 1 {
				return Err((
					format!(
						"Could not satisfy distance requirements, lowest cost: {}",
						best_cost
					),
					best_assign,
				));
			}
		}

		Ok(best_assign)
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
		println!("starting {:?} {:?}", coid, terminal);
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
		println!("Queue: {:?}", queue);
		while !queue.is_empty() {
			let wiid = queue.pop_front().unwrap();
			if seen.contains(&wiid) {
				continue;
			}
			seen.insert(wiid);
			let wire = &self.wires[wiid.0];
			println!("Taking: {:?}", wire);
			add_wires_on_terminal(wire.node1_id, wire.terminal1_id, &mut queue);
			add_wires_on_terminal(wire.node2_id, wire.terminal2_id, &mut queue);
			retval.insert((wire.node1_id, wire.terminal1_id));
			retval.insert((wire.node2_id, wire.terminal2_id));
			println!("Queue: {:?}", queue);
		}
		retval
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
	use ld::get_large_memory_test_design;

	use crate::logical_design::{get_large_logical_design, NodeId};

	use super::*;
	#[test]
	fn new() {
		let mut p = PhysicalDesign::new();
		let l = ld::get_simple_logical_design();
		p.build_from(&l, PlacementStrategy::default());
	}

	#[test]
	fn complex_40_ilp() {
		let mut p = PhysicalDesign::new();
		let l = ld::get_complex_40_logical_design();
		p.build_from(&l, PlacementStrategy::ILP);
	}

	#[test]
	fn simple_ilp() {
		let mut p = PhysicalDesign::new();
		let l = ld::get_simple_logical_design();
		p.build_from(&l, PlacementStrategy::ILP);
	}

	#[test]
	fn n_combs_ilp() {
		let mut p = PhysicalDesign::new();
		let mut l = LogicalDesign::new();
		for _ in 0..500 {
			l.add_nop(ld::Signal::Id(0), ld::Signal::Id(0));
		}
		p.build_from(&l, PlacementStrategy::ILP);
	}

	#[test]
	fn complex_40_ilp_coarse15_objective() {
		let mut p = PhysicalDesign::new();
		let l = ld::get_complex_40_logical_design();
		p.build_from(&l, PlacementStrategy::ILPCoarse15);
		p.save_svg(&l, "svg/complex40_combs_ilp_coarse15.svg");
	}

	#[test]
	fn complex_40_ilp_coarse8() {
		let mut p = PhysicalDesign::new();
		let l = ld::get_complex_40_logical_design();
		p.build_from(&l, PlacementStrategy::ILPCoarse8);
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
	fn complex_40_mcmc_coarse15() {
		let mut p = PhysicalDesign::new();
		let l = ld::get_complex_40_logical_design();
		p.build_from(&l, PlacementStrategy::MCMCSACoarse15);
		p.save_svg(&l, "svg/complex40_mcmc_coarse15.svg");
	}

	#[test]
	fn memory_n_mcmc_coarse15() {
		let mut p = PhysicalDesign::new();
		let l = ld::get_large_memory_test_design(80);
		p.build_from(&l, PlacementStrategy::MCMCSACoarse15);
		p.save_svg(&l, "svg/memory_n_mcmc_coarse15.svg");
	}
}

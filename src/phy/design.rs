use super::placement::global::GlobalPlacement;
use super::route::Topology;
use super::*;

use core::{f64, panic};
use itertools::Itertools;
use rand::rngs::StdRng;
use rand::seq::SliceRandom;
use rand::{Rng, SeedableRng};
use std::ops::Rem;
use std::usize;

use std::{
	collections::{HashMap, HashSet, LinkedList},
	hash::Hash,
	vec,
};

use crate::logical_design::{NodeFunction, NodeId, NET_GREEN, NET_RED};
use crate::mapped_design::Direction;
use crate::ndarr::Arr2;
use crate::phy::placement::*;
use crate::sim::{NetId, SimState, WireNetwork};
use crate::util::{self, hash_map, hash_set, HashM, HashS};
use crate::{
	logical_design::{self as ld, LogicalDesign, WireColour},
	svg::SVG,
};

use super::placement::Placement;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PhyId(pub(crate) usize);

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
	fn input(colour: ld::WireColour) -> TerminalId {
		match colour {
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

#[derive(Debug, Clone)]
pub struct PhyNode {
	pub id: PhyId,
	pub logic: ld::NodeId,
	pub position: (f64, f64),
	pub placed: bool,
	pub orientation: u32,
	pub partition: i32,
	pub hop_type: WireHopType,
}

#[derive(Debug, Clone)]
pub struct Wire {
	pub id: WireId,
	pub node1_id: PhyId,
	pub node2_id: PhyId,
	pub terminal1_id: TerminalId,
	pub terminal2_id: TerminalId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WireHopType {
	Small,
	Medium,
	Big,
	Substation,
	Combinator,
	Lamp,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Settings {
	pub group_io: bool,
}

#[derive(Debug, Clone)]
pub struct PhysicalDesign {
	settings: Settings,

	nodes: Vec<PhyNode>,
	wires: Vec<Wire>,

	idx_combs: HashM<ld::NodeId, PhyId>,
	space: Vec<Arr2<PhyId>>,

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

	failed_routes: Vec<(usize, usize)>,
}

#[derive(Debug)]
struct Netlist {
	networks: Vec<WireNetwork>,
	network_map: Vec<Option<NetId>>,
}

#[derive(Debug, Default, Clone)]
struct RouteCache {
	existing_poles: HashM<usize, Vec<PhyId>>,
}

impl Netlist {
	fn compute_networks(logd: &LogicalDesign) -> Netlist {
		let mut nets = Netlist {
			networks: vec![],
			network_map: vec![None; logd.nodes.len()],
		};
		for node in logd.nodes.iter() {
			let colour = if let NodeFunction::WireSum(colour) = node.function {
				colour
			} else {
				continue;
			};
			if let Some(_existing_network) = nets.network_map[node.id.0] {
				continue;
			}
			let network_wires = logd.get_wire_network(node.id);
			assert!(
				!network_wires.is_empty(),
				"A \"wire\" network was found with no wires."
			);
			assert!(
				network_wires.contains(&node.id),
				"Wire not in it's own network!"
			);
			let network_id = NetId(nets.networks.len());
			let mut fanin = hash_set();
			let mut fanout = hash_set();
			for wire in &network_wires {
				let wire_node = &logd.nodes[wire.0];
				for fiid in wire_node.iter_fanin(colour) {
					fanin.insert(*fiid);
				}
				for foid in wire_node.iter_fanout(colour) {
					fanout.insert(*foid);
				}
			}
			for wire in &network_wires {
				nets.network_map[wire.0] = Some(network_id);
			}
			nets.networks.push(WireNetwork {
				fanin: fanin.into_iter().sorted().collect_vec(),
				fanout: fanout.into_iter().sorted().collect_vec(),
				wires: network_wires,
				colour,
			});
		}
		nets
	}
}

impl PhysicalDesign {
	pub fn new() -> Self {
		PhysicalDesign {
			settings: Settings::default(),

			nodes: vec![],
			wires: vec![],
			idx_combs: hash_map(),
			space: vec![],

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

			failed_routes: vec![],
		}
	}

	pub fn width(&self) -> usize {
		self.global_space.dims().0
	}

	pub fn height(&self) -> usize {
		self.global_space.dims().1
	}

	pub fn n_nodes(&self) -> usize {
		self.nodes.len()
	}

	pub fn set_group_io(&mut self, group_io: bool) {
		self.settings.group_io = group_io;
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

	pub(crate) fn build_from(&mut self, logical: &LogicalDesign) -> bool {
		let partition_size = CFG.parition.target_size;
		let scale_factor = CFG.parition.side_length_single_partition_scale_factor;
		self.side_length_single_partition =
			(partition_size as f64 * scale_factor * 2.0).sqrt().ceil() as usize;
		self.side_length_single_partition += self.side_length_single_partition & 1;
		self.extract_combs(logical);
		let n_grouped_io_blocks = self.partition(logical, partition_size as i32) as i32;
		self.side_length_partitions = (self.n_partitions as f64).sqrt().ceil() as usize;

		let connectivity = self.get_connectivity_as_vec_usize(logical);
		let (
			partition_local_connectivity,
			partition_global_connectivity,
			local_to_global,
			global_to_local,
		) = self.split_connections_to_local_and_global(&connectivity);

		self.place_global(&partition_global_connectivity);
		if n_grouped_io_blocks > 0 {
			let mut count = 0;
			for i in (self.n_partitions - n_grouped_io_blocks)..self.n_partitions {
				self.partition_assignments[i as usize] = (
					self.side_length_partitions + count / self.side_length_partitions,
					count % self.side_length_partitions,
				);
				count += 1;
			}
			self.side_length_partitions +=
				(count + self.side_length_partitions - 1) / self.side_length_partitions;
		}
		self.place_local(
			logical,
			&partition_local_connectivity,
			&partition_global_connectivity,
			&local_to_global,
		);
		let res = self.global_freeze_and_route2(
			logical,
			//&partition_global_connectivity,
			&local_to_global,
			&global_to_local,
		);
		self.validate_against(logical);
		self.make_assertions(logical);
		self.fill_coverage();
		res
	}

	pub(crate) fn for_all_phy<F>(&self, mut func: F)
	where
		F: FnMut(&PhyNode),
	{
		for x in &self.nodes {
			func(x)
		}
	}

	pub(crate) fn for_all_wires<F>(&self, mut func: F)
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
			| ld::NodeFunction::Lamp { .. }
			| ld::NodeFunction::DisplayPanel { .. } => {
				let id = PhyId(self.nodes.len());
				self.nodes.push(PhyNode {
					id,
					logic: ld_node.id,
					position: (0.0, 0.0),
					placed: false,
					orientation: 4,
					partition: 0,
					hop_type: WireHopType::Combinator,
				});
				self.idx_combs.insert(ld_node.id, id);
			},
			ld::NodeFunction::WireSum(_c) => { /* Do nothing for now */ },
		});
	}

	pub(crate) fn get_logical<'a>(&self, id: PhyId, logical: &'a LogicalDesign) -> &'a ld::Node {
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
				initial[c] = (x, y);
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
		partition_global_connectivity: &Vec<Vec<Vec<(i32, usize, usize)>>>,
		local_to_global: &Vec<Vec<usize>>,
	) {
		for partition in 0..self.n_partitions {
			println!("=====PARTITION {partition}=====");
			let initializations =
				Self::get_initializations(&partition_local_connectivity[partition as usize]);
			self.space[partition as usize] = Arr2::new([
				self.side_length_single_partition,
				self.side_length_single_partition,
			]);
			let algo = Self::solve_as_mcmc_dense(
				&partition_local_connectivity[partition as usize],
				&partition_global_connectivity[partition as usize],
				&initializations,
				None,
				self.side_length_single_partition,
			);
			let comb_positions = match algo {
				Ok(pos) => pos,
				Err(e) => {
					println!("ERR: Placement algorithm failed");
					println!("ERR: {}", e.0);
					let _ =
						self.place_combs_physical_dense(&e.1, logical, partition, local_to_global);
					let _ = self.save_svg(logical, format!("./svg/failed{}.svg", 1.5).as_str());
					panic!("failed to place");
				},
			};
			let algo = self.solve_analytical_dense(
				logical,
				&partition_local_connectivity[partition as usize],
				&partition_global_connectivity[partition as usize],
				&comb_positions,
				self.side_length_single_partition,
				partition,
				local_to_global,
				false,
			);
			let comb_positions = match algo {
				Ok(pos) => pos,
				Err(e) => {
					println!("ERR: Placement algorithm failed");
					println!("ERR: {}", e.0);
					let _ =
						self.place_combs_physical_dense(&e.1, logical, partition, local_to_global);
					let _ = self.save_svg(logical, format!("./svg/failed{}.svg", 1.5).as_str());
					panic!("failed to place");
				},
			};
			if self
				.place_combs_physical_dense(&comb_positions, logical, partition, local_to_global)
				.is_err()
			{
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
			println!("=====DONE PARTITION=====");
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
				Ok(_) => {},
				Err(e) => {
					println!("Failed to place cell {idx} at position {:?}. {}", p, e);
				},
			}
			if failure.is_ok() {
				failure = res2;
			}
		}
		failure
	}

	fn solve_analytical_dense(
		&self,
		logical: &LogicalDesign,
		connections_per_node: &Vec<Vec<usize>>,
		global_connectivity: &Vec<Vec<(i32, usize, usize)>>,
		initialization: &Vec<(usize, usize)>,
		side_length: usize,
		partition_id: i32,
		local_to_global: &Vec<Vec<usize>>,
		animated: bool,
	) -> Result<Vec<(usize, usize)>, (String, Vec<(usize, usize)>)> {
		let num_cells = connections_per_node.len();
		let edges = adjacency_to_edges(connections_per_node, true);
		let mut placement = placement2::AnalyticalPlacement::new(side_length as f32, animated);

		for id in 0..num_cells {
			let node = self.get_logical(PhyId(local_to_global[partition_id as usize][id]), logical);
			let spec = node.function.wire_hop_type().wire_hop_spec();
			placement.add_cell(
				Some((initialization[id].0 as f32, initialization[id].1 as f32)),
				(spec.dim.0 as f32, spec.dim.1 as f32),
				false,
				spec.reach as f32,
			);
		}
		let substation = WireHopType::Substation.wire_hop_spec();
		for x in (8..side_length).step_by(18) {
			for y in (8..side_length).step_by(18) {
				placement.add_cell(
					Some((x as f32, y as f32)),
					(substation.dim.0 as f32, substation.dim.1 as f32),
					true,
					substation.reach as f32,
				);
			}
		}
		let mut cost = placement.compute_cost(&edges);
		let mut round = 0;
		//let mut momentum = vec![(0.0, 0.0); placement.num_cells()];
		while !cost.1 {
			if round.rem(1000) == 0 {
				println!("{round}");
				if !animated {
					placement
						.draw_placement(&edges, "svg/solve_analytical_dense.svg")
						.ok();
				}
				let mut placement2 = placement.clone();
				placement2.apply_legalization();
				if placement2.compute_cost(&edges).1 {
					break;
				}
			}
			if round == 1_000_000 {
				let mut ret = placement.legalized();
				ret.resize(num_cells, (0, 0));
				return Err(("Never satisfied constraints".to_owned(), ret));
			}

			let spring = placement.calculate_spring_force(connections_per_node);
			let overlap = placement.calculate_overlap_force();
			let legalization_force = placement.calculate_legalization_force();
			//let elec = placement.calculate_electrostatic_force();
			let buckle = placement.calculate_buckling_force();
			let access = placement.calculate_accessibility_force(global_connectivity);
			let radial = placement.calculate_radial_force();

			let cfg = &CFG.placement2;
			let t = round as f32 / cfg.round_to_time_divisor;

			//let spring_c = 4.0 / ((t * 3.0).powi(2) + 1.0);
			//let legalization_c =
			//	160.0 * (1.0 - (-t / 5.0).exp()) * ((t * f64::consts::PI * 64.0).sin() + 1.0);
			//let overlap_c = 500.0 * ((t * f64::consts::PI * 40.0).sin() + 1.0);
			//let buckle_c = 100.0 * ((-t * f64::consts::PI * 16.0).sin() + 1.0) + 10.0;
			//let access_c = 2.0;
			let spring_c = cfg.spring_k0 / (t * cfg.spring_k1 + cfg.spring_k2);
			let legalization_c =
				t / cfg.legalization_k0 + (t + cfg.legalization_k1).sin() * cfg.legalization_k2;
			//let elec_c = 10.0;
			let overlap_c = cfg.overlap_k0 + (t + cfg.overlap_k1).sin() * cfg.overlap_k2;
			let buckle_c = cfg.buckle_k0;
			let access_c = cfg.access_k0;
			let radial_c = cfg.radial_k0
				* side_length as f32
				* (1.0 - (t * t / cfg.radial_k1 + cfg.radial_k2).recip());

			let mut force = vec![(0.0, 0.0); spring.len()];
			for i in 0..num_cells {
				force[i].0 = spring_c * spring[i].0
					+ overlap_c * overlap[i].0
					+ legalization_c * legalization_force[i].0
					//- elec_c * elec[i].0
					+ buckle_c * buckle[i].0
					+ access_c * access[i].0
					+ radial_c * radial[i].0;
				force[i].1 = spring_c * spring[i].1
					+ overlap_c * overlap[i].1
					+ legalization_c * legalization_force[i].1
					//- elec_c * elec[i].1
					+ access_c * access[i].1
					+ buckle_c * buckle[i].1
					+ radial_c * radial[i].1;
			}

			placement.step_cells(force, cfg.step_size);
			cost = placement.compute_cost(&edges);
			round += 1;
		}
		placement.apply_legalization();
		placement
			.draw_placement(&edges, &format!("svg/partition{partition_id}.svg"))
			.ok();
		let mut ret = placement.legalized();
		ret.resize(num_cells, (0, 0));
		Ok(ret)
	}

	fn solve_as_mcmc_dense(
		connections_per_node: &Vec<Vec<usize>>,
		_global_connectivity: &Vec<Vec<(i32, usize, usize)>>,
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
			for (idx_init, init) in initializations.iter().enumerate() {
				let mut initplc = curr.clone();
				for (idx, assignment) in init.iter().enumerate() {
					initplc.mov(idx, *assignment);
				}
				let cost = initplc.compute_cost(&connections, max_density);
				if min_score.1 && min_score.2 == 0 {
					if cost.1 && cost.2 == 0 && cost.0 < min_score.0 {
						min_score = cost;
						min_idx = idx_init;
					}
				} else if cost.0 < min_score.0 {
					min_score = cost;
					min_idx = idx_init;
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
				if let Some(_idx_prior) = seen.insert((x, y), idx) {
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

		let mut new_best = true;
		let stage_2 = false;
		let mut final_stage = false;
		let mut step = 0;
		let mut trend_of_bests = vec![];
		while step < iterations {
			if best_cost.1 && !final_stage {
				max_density = 1;
				check_invariants(&curr, 1);
				final_stage = true;
				if (step as f64 / iterations as f64) < 0.05 {
					iterations = (step as f64 * 4.0) as i32;
				} else {
					iterations = (step as f64 * 1.3) as i32;
				}

				println!("Entering final compacting stage.");
				if best_cost.0 <= 0.0 && step > 1_000 {
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
				//(15, false, false, swap_random_method),
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
				for (weight, can_run_if_final, _, func, _method_name) in METHODS {
					if final_stage && !can_run_if_final {
						continue;
					}
					cumulative += weight;
					if pick < cumulative {
						func(
							&mut rng,
							&curr,
							&mut new,
							connections_per_node,
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
				graph.save("svg/sat_count.svg").ok();
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
				graph.save("svg/cost.svg").ok();
			}

			if !final_stage && (delta < 0.0 || new_cost.1)
				|| final_stage && (delta < 0.0 && new_cost.1)
			{
				//best_cost = new.compute_cost(&connections, max_density);
				new.draw_placement(&connections, max_density, "svg/best.svg")
					.ok();
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

	fn solve_as_mcmc_global(
		connections_per_node: &Vec<Vec<(usize, usize)>>,
		initializations: &Vec<Vec<(usize, usize)>>,
		init_temp: Option<f64>,
		side_length: usize,
		cell_size: usize,
	) -> Result<Vec<(usize, usize)>, (String, Vec<(usize, usize)>)> {
		let num_cells = connections_per_node.len();

		let connections = adjacency_to_edges_weighted(connections_per_node, false);

		let mut rng = StdRng::seed_from_u64(0xB1FFB0FF);
		let mut curr =
			GlobalPlacement::from_initialization(vec![(0, 0); num_cells], side_length, cell_size);

		let mut max_density = 1;

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
		let iterations = 20_000;

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
		let _check_invariant_unique_position = |plc: &GlobalPlacement| {
			let mut seen = HashMap::new();
			for (idx, (x, y)) in plc.assignments.iter().enumerate() {
				if let Some(_idx_prior) = seen.insert((x, y), idx) {
					assert!(false);
				}
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
		let _check_invariants = |plc: &GlobalPlacement, max_density: i32| {
			check_invariant_density(plc, max_density);
			check_invariant_blocks_assignments_congruent(plc);
		};

		let mut new_best = false;
		let mut step = 1000;
		let mut trend_of_bests = vec![];
		while step < iterations {
			step += 1;
			let mut new = curr.clone();

			type METHOD = (
				usize, //weight
				bool,  //final_stage
				bool,  // After best found
				fn(
					rng: &mut StdRng,
					curr: &GlobalPlacement,
					new: &mut GlobalPlacement,
					connections_per_node: &Vec<Vec<(usize, usize)>>,
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

			// Select weighted method
			{
				let total_weight: usize = METHODS
					.iter()
					.filter(
						|(_, _can_run_if_final, after_best_found, _, _)| {
							if new_best {
								*after_best_found
							} else {
								true
							}
						},
					)
					.map(|(weight, _, _, _, _)| *weight)
					.sum();

				let pick = rng.random_range(0..total_weight);
				let mut cumulative = 0;
				for (weight, _can_run_if_final, _, func, _method_name) in METHODS {
					cumulative += weight;
					if pick < cumulative {
						func(
							&mut rng,
							&curr,
							&mut new,
							connections_per_node,
							side_length,
							max_density,
						);
						check_invariant_blocks_assignments_congruent(&new);
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
				graph.save("svg/sat_count.svg").ok();
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

			if delta < 0.0 {
				new.draw_placement(
					&connections.iter().map(|(a, b, _w)| (*a, *b)).collect_vec(),
					max_density,
				)
				.save("svg/global_placement.svg")
				.ok();
				best = new.clone();
				curr = new;
				best_cost = new_cost;
				assignments_cost = best_cost;
				println!("Best cost {} ({}), temp {}", best_cost.0, best_cost.2, temp);
				trend_of_bests.push(best_cost);
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

	fn max_allowable_distance(
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
		self.nodes.retain(|n| !n.is_pole());
		self.wires.clear();
		self.failed_routes = vec![];
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
						dbg!(ld_node);
						return Result::Err(format!(
							"Sub-cell ({x}, {y}) overlaps with an existing cell."
						));
					}
				} else {
					if !self.global_space.index_good(key) {
						return Result::Err(format!("Sub-cell ({x}, {y}) is OOB."));
					}
					if self.global_space[key] != PhyId::default() {
						dbg!(ld_node);
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
			position.0 + hop_spec.dim.0 / 2.0,
			position.1 + hop_spec.dim.1 / 2.0,
		);
		comb_to_place.placed = true;
		Result::Ok(())
	}

	fn close_enough_to_connect(&self, logical: &LogicalDesign, id_i: NodeId, id_j: NodeId) -> bool {
		let comb_i = &self.nodes[self.idx_combs.get(&id_i).unwrap().0];
		let comb_j = &self.nodes[self.idx_combs.get(&id_j).unwrap().0];
		let ld_node_i = logical.get_node(comb_i.logic);
		let ld_node_j = logical.get_node(comb_j.logic);
		let hop_spec_i = ld_node_i.function.wire_hop_type().wire_hop_spec();
		let hop_spec_j = ld_node_j.function.wire_hop_type().wire_hop_spec();
		let min_distance = f64::min(hop_spec_i.reach, hop_spec_j.reach);
		euclidean_distance_squared_f64_pair(comb_i.position, comb_j.position)
			< min_distance * min_distance
	}

	fn close_enough_to_connect_phy(
		&self,
		logical: &LogicalDesign,
		id_i: PhyId,
		id_j: PhyId,
	) -> bool {
		let comb_i = &self.nodes[id_i.0];
		let comb_j = &self.nodes[id_j.0];
		let ld_node_i = logical.get_node(comb_i.logic);
		let ld_node_j = logical.get_node(comb_j.logic);
		let hop_spec_i = ld_node_i.function.wire_hop_type().wire_hop_spec();
		let hop_spec_j = ld_node_j.function.wire_hop_type().wire_hop_spec();
		let min_distance = f64::min(hop_spec_i.reach, hop_spec_j.reach);
		euclidean_distance_squared_f64_pair(comb_i.position, comb_j.position)
			< min_distance * min_distance
	}

	fn connect_wire2(
		&mut self,
		direction_a: Direction,
		id_comb_a: NodeId,
		direction_b: Direction,
		id_comb_b: NodeId,
		colour: WireColour,
		logical: &LogicalDesign,
	) {
		let id_comb_a = self.idx_combs[&id_comb_a];
		let id_comb_b = self.idx_combs[&id_comb_b];
		let id_wire = self.wires.len();
		let comb_a = &self.nodes[id_comb_a.0];
		let comb_b = &self.nodes[id_comb_b.0];
		let ld_comb_a = logical.get_node(comb_a.logic);
		let ld_comb_b = logical.get_node(comb_b.logic);
		let term_a = match &ld_comb_a.function {
			ld::NodeFunction::Arithmetic { .. } | ld::NodeFunction::Decider { .. } => {
				if matches!(direction_a, Direction::Input) {
					TerminalId::input(colour)
				} else {
					TerminalId::output_combinator(colour)
				}
			},
			ld::NodeFunction::Constant { .. }
			| ld::NodeFunction::Lamp { .. }
			| ld::NodeFunction::DisplayPanel { .. } => TerminalId::output_constant(colour),
			ld::NodeFunction::WireSum(_c) => unreachable!(),
		};
		let term_b = match &ld_comb_b.function {
			ld::NodeFunction::Arithmetic { .. } | ld::NodeFunction::Decider { .. } => {
				if matches!(direction_b, Direction::Input) {
					TerminalId::input(colour)
				} else {
					TerminalId::output_combinator(colour)
				}
			},
			ld::NodeFunction::Constant { .. }
			| ld::NodeFunction::Lamp { .. }
			| ld::NodeFunction::DisplayPanel { .. } => TerminalId::input(colour),
			ld::NodeFunction::WireSum(_c) => unreachable!(),
		};

		self.wires.push(Wire {
			id: WireId(id_wire),
			node1_id: id_comb_a,
			node2_id: id_comb_b,
			terminal1_id: term_a,
			terminal2_id: term_b,
		});
	}

	pub(crate) fn save_svg_full<P>(
		&self,
		sim: Option<&SimState>,
		ld: &LogicalDesign,
		filename: P,
	) -> Result<(), std::io::Error>
	where
		P: Into<std::path::PathBuf>,
	{
		const SCALE: f64 = 20.0;
		const GREY: (u8, u8, u8) = (230, 230, 230);
		fn format_comb_hint(
			sim: Option<&SimState>,
			ld: &LogicalDesign,
			id: NodeId,
		) -> Option<String> {
			let node = ld.get_node(id);
			match sim {
				Some(sim) => {
					let green = sim.probe_green_out(id);
					let red = sim.probe_red_out(id);
					let mut ret = if let Some(desc) = &node.description {
						desc.clone() + "\n"
					} else {
						String::new()
					};
					ret += &format!("Outputs:\n\tRed: {red:?}\n\tGreen: {green:?}");

					let green = sim.probe_input(id, NET_GREEN);
					let red = sim.probe_input(id, NET_RED);
					ret += &format!("\nInputs:\n\tRed: {red:?}\n\tGreen: {green:?}");

					Some(ret)
				},
				None => node.description.clone(),
			}
		}

		fn pick_colour(ld: &LogicalDesign, id: NodeId) -> (u8, u8, u8) {
			match ld.is_port(id) {
				Some(Direction::Input) => (230, 100, 100),
				Some(Direction::Output) => (100, 100, 230),
				Some(Direction::Inout) => (230, 100, 230),
				None => GREY,
			}
		}
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
		let mut shapes = vec![];
		for c in &self.nodes {
			let mut x = (c.position.0 * (SCALE + 2.0)) as i32;
			let y = (c.position.1 * (SCALE + 2.0)) as i32;
			if c.is_pole() {
				match c.hop_type {
					WireHopType::Small => {
						// Otter brown
						shapes.push(svg.add_circle(
							x + SCALE as i32,
							y + SCALE as i32,
							SCALE as i32 / 6,
							(101, 67, 33),
							None,
							None,
						));
					},
					WireHopType::Medium => {
						// Otter brown
						shapes.push(svg.add_circle(
							x + SCALE as i32,
							y + SCALE as i32,
							SCALE as i32 / 4,
							(101, 67, 33),
							None,
							None,
						));
					},
					WireHopType::Big => {
						shapes.push(svg.add_circle(
							x + SCALE as i32 * 2,
							y + SCALE as i32 * 2,
							SCALE as i32,
							(101, 101, 101),
							None,
							None,
						));
					},
					WireHopType::Substation => {
						shapes.push(svg.add_rect(
							x,
							y,
							SCALE as i32 * 2,
							SCALE as i32 * 2,
							(101, 101, 101),
							Some("SS".to_owned()),
							None,
						));
					},
					_ => unreachable!(),
				}
				continue;
			}
			let node = ld.get_node(c.logic);
			let (w, h, label, hover) = match &node.function {
				ld::NodeFunction::Arithmetic { op, .. } => {
					x -= SCALE as i32 / 2;
					(
						SCALE * 2.0,
						SCALE,
						Some(op.resolve_string()),
						format_comb_hint(sim, ld, c.logic),
					)
				},
				ld::NodeFunction::Decider { expressions, .. } => {
					x -= SCALE as i32 / 2;
					if let Some(e) = expressions.first() {
						(
							SCALE * 2.0,
							SCALE,
							Some(e.1.resolve_string()),
							format_comb_hint(sim, ld, c.logic),
						)
					} else {
						(SCALE * 2.0, SCALE, Some("D"), node.description.clone())
					}
				},
				ld::NodeFunction::Constant { .. } => {
					(SCALE, SCALE, Some("C"), format_comb_hint(sim, ld, c.logic))
				},
				ld::NodeFunction::Lamp { .. } => {
					(SCALE, SCALE, Some("L"), format_comb_hint(sim, ld, c.logic))
				},
				ld::NodeFunction::DisplayPanel { .. } => {
					(SCALE, SCALE, Some("P"), format_comb_hint(sim, ld, c.logic))
				},
				ld::NodeFunction::WireSum(_) => {
					unreachable!()
				},
			};
			shapes.push(svg.add_rect(
				x,
				y,
				w as i32,
				h as i32,
				pick_colour(ld, c.logic),
				label.map(|x| x.to_owned()),
				hover,
			));
		}
		for wire in &self.wires {
			svg.add_wire(
				wire.node1_id.0 + 1,
				wire.node2_id.0 + 1,
				wire.terminal1_id.0 - 1,
				wire.terminal2_id.0 - 1,
			);
		}
		for edge in &self.failed_routes {
			let c1 = &self.nodes[edge.0];
			let c2 = &self.nodes[edge.1];
			let x1 = (c1.position.0 * (SCALE + 2.0)) as i32;
			let y1 = (c1.position.1 * (SCALE + 2.0)) as i32;
			let x2 = (c2.position.0 * (SCALE + 2.0)) as i32;
			let y2 = (c2.position.1 * (SCALE + 2.0)) as i32;
			svg.add_line_stroke_dasharray(
				x1,
				y1,
				x2,
				y2,
				Some("blue".to_owned()),
				Some(2),
				Some("1".to_owned()),
			);
		}
		svg.save(filename)
	}

	pub(crate) fn save_svg<P>(&self, ld: &LogicalDesign, filename: P) -> Result<(), std::io::Error>
	where
		P: Into<std::path::PathBuf>,
	{
		self.save_svg_full(None, ld, filename)
	}

	fn validate_against(&self, logd: &LogicalDesign) {
		let (wire_groups, membership) = self.get_wire_networks();
		for wire_group in &wire_groups {
			let wire = &self.wires[wire_group[0].0];
			let mut node = &self.nodes[wire.node1_id.0];
			let (dir1, colour) = if node.is_pole() {
				node = &self.nodes[wire.node2_id.0];
				if node.is_pole() {
					continue;
				}
				self.terminal_id_to_direction_colour(wire.node2_id, wire.terminal2_id, logd)
			} else {
				self.terminal_id_to_direction_colour(wire.node1_id, wire.terminal1_id, logd)
			};

			let logd_wires = logd.get_wire_network_on_cell(node.logic, colour, dir1);
			let phyd_wires = &wire_groups[membership[wire.id.0].expect("Membership unknown")];
			let phy_connections: HashS<(PhyId, TerminalId)> = phyd_wires
				.iter()
				.flat_map(|wire_id| {
					//
					let wire = &self.wires[wire_id.0];
					[
						(wire.node1_id, wire.terminal1_id),
						(wire.node2_id, wire.terminal2_id),
					]
				})
				.filter(|(id, _)| !self.nodes[id.0].is_pole())
				.collect();
			let mut logd_connections: HashS<(NodeId, Direction)> = logd_wires
				.iter()
				.flat_map(|wire_id| {
					//
					let node = logd.get_node(*wire_id);
					node.iter_fanin(colour)
						.map(|x| (*x, Direction::Output))
						.chain(node.iter_fanout(colour).map(|x| (*x, Direction::Input)))
				})
				.collect();
			assert_eq!(phy_connections.len(), logd_connections.len());
			for (phy_id, terminal) in &phy_connections {
				let logd_id = self.nodes[phy_id.0].logic;
				let (dir, _) = self.terminal_id_to_direction_colour(*phy_id, *terminal, logd);
				if !logd_connections.contains(&(logd_id, dir)) {
					println!("{:?}", phy_connections);
					println!("{:?}", logd_connections);
					panic!("Physical design has a connection the logical design does not!");
				}
				logd_connections.remove(&(logd_id, dir));
			}
			assert!(logd_connections.is_empty());
		}
	}

	fn get_wire_networks(&self) -> (Vec<Vec<WireId>>, Vec<Option<usize>>) {
		let mut membership = vec![None; self.wires.len()];
		let mut groups = vec![];
		let mut index: HashM<(PhyId, TerminalId), Vec<WireId>> = hash_map();
		for wire in &self.wires {
			let key = (wire.node1_id, wire.terminal1_id);
			index.entry(key).or_default().push(wire.id);
			let key = (wire.node2_id, wire.terminal2_id);
			index.entry(key).or_default().push(wire.id);
		}
		#[cfg(false)]
		for i in 0..self.nodes.len() {
			for j in 1..=4 {
				match index.entry((PhyId(i), TerminalId(j))) {
					std::collections::hash_map::Entry::Occupied(entry) => {
						//
						println!(
							"{:?} -> {:?}",
							entry.key(),
							entry.get().iter().map(|x| x.0).collect_vec()
						);
					},
					std::collections::hash_map::Entry::Vacant(_) => {},
				}
			}
		}
		for wire in &self.wires {
			if membership[wire.id.0].is_some() {
				continue;
			}
			let mut stack = vec![wire.id];
			let group_id = Some(groups.len());
			groups.push(vec![]);
			while !stack.is_empty() {
				let wire_id = stack.pop().unwrap();
				let wire = &self.wires[wire_id.0];
				if membership[wire_id.0].is_some() {
					continue;
				}
				membership[wire_id.0] = group_id;
				groups[group_id.unwrap()].push(wire_id);
				for &other_wire in &index[&(wire.node1_id, wire.terminal1_id)] {
					if other_wire == wire_id {
						continue;
					}
					assert!(
						membership[other_wire.0].is_none() || membership[other_wire.0] == group_id,
						"Should only have one group at this point"
					);
					if membership[other_wire.0].is_some() {
						continue;
					}
					stack.push(other_wire);
				}
				for &other_wire in &index[&(wire.node2_id, wire.terminal2_id)] {
					if other_wire == wire_id {
						continue;
					}
					assert!(
						membership[other_wire.0].is_none() || membership[other_wire.0] == group_id,
						"Should only have one group at this point"
					);
					if membership[other_wire.0].is_some() {
						continue;
					}
					stack.push(other_wire);
				}
			}
		}
		(groups, membership)
	}

	fn terminal_id_to_direction_colour(
		&self,
		id: PhyId,
		terminal: TerminalId,
		logd: &LogicalDesign,
	) -> (Direction, WireColour) {
		let node = &self.nodes[id.0];
		let logd_node = logd.get_node(node.logic);
		match logd_node.function {
			NodeFunction::Arithmetic { .. } | NodeFunction::Decider { .. } => {
				if terminal == TerminalId::input(WireColour::Red)
					|| terminal == TerminalId::input(WireColour::Green)
				{
					if terminal == TerminalId::input(WireColour::Red) {
						(Direction::Input, WireColour::Red)
					} else {
						(Direction::Input, WireColour::Green)
					}
				} else {
					if terminal == TerminalId::output_combinator(WireColour::Red) {
						(Direction::Output, WireColour::Red)
					} else {
						(Direction::Output, WireColour::Green)
					}
				}
			},
			NodeFunction::Constant { .. } => {
				let colour = if terminal == TerminalId::output_constant(WireColour::Red) {
					WireColour::Red
				} else {
					WireColour::Green
				};
				(Direction::Output, colour)
			},
			NodeFunction::DisplayPanel { .. } | NodeFunction::Lamp { .. } => {
				let colour = if terminal == TerminalId::input(WireColour::Red) {
					WireColour::Red
				} else {
					WireColour::Green
				};
				(Direction::Input, colour)
			},
			NodeFunction::WireSum(..) => unreachable!(),
		}
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

	fn get_connectivity_as_vec(&self, ld: &LogicalDesign) -> Vec<Vec<PhyId>> {
		let mut connections = vec![vec![]; self.nodes.len()];
		for cell in &self.nodes {
			for ldid in ld.get_connected_combs(cell.logic) {
				connections[cell.id.0].push(*self.idx_combs.get(&ldid).unwrap());
			}
		}
		connections
	}

	pub(crate) fn get_connectivity_as_vec_usize(&self, ld: &LogicalDesign) -> Vec<Vec<usize>> {
		let mut connections = vec![vec![]; self.nodes.len()];
		for cell in &self.nodes {
			for ldid in ld.get_connected_combs(cell.logic) {
				connections[cell.id.0].push(self.idx_combs.get(&ldid).unwrap().0);
			}
		}
		connections
	}

	fn split_connections_to_local_and_global(
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
						let other_local_id = *global_to_local[part].get(other_global_id).unwrap();
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

	pub(crate) fn get_connectivity_as_matrix(
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

	pub(crate) fn get_connectivity_as_edges(
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

	fn partition(&mut self, ld: &LogicalDesign, target_size: i32) -> usize {
		let connectivity = self.get_connectivity_as_vec_usize(ld);
		if target_size * 3 / 2 > connectivity.len() as i32 {
			self.space = vec![Arr2::new([0, 0]); self.n_partitions as usize];
			println!("Design small enough to skip partitioning.");
			return 0;
		}
		let mut n_parts = (connectivity.len() as f64 / target_size as f64).ceil() as i32;
		let (mut partition, n_cuts) = partition::metis(&connectivity, n_parts);
		partition::report_partition_quality(&partition, n_cuts, &connectivity, n_parts);
		let mut ret = 0;
		if self.settings.group_io {
			ret += 1;
			n_parts += 1;
			let mut count = 0;
			for (id, p) in partition.iter_mut().enumerate() {
				if ld.is_port(self.nodes[id].logic).is_none() {
					continue;
				}
				if count > target_size {
					n_parts += 1;
					count = 0;
					ret += 1;
				}
				self.nodes[id].partition = n_parts - 1;
				*p = n_parts - 1;
				count += 1;
			}
		}
		self.n_edges_cut = n_cuts;
		self.n_partitions = n_parts;
		for (id, p) in partition.iter().enumerate() {
			assert!(*p >= 0);
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
		ret
	}

	fn place_global(
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
		match Self::solve_as_mcmc_global(
			&connectivity,
			&vec![self.partition_assignments.clone()],
			None,
			self.side_length_partitions,
			1,
		) {
			Ok(pos) => self.partition_assignments = pos,
			Err((err, _)) => {
				println!("Failed to do global placement, defaulting to dumb initialization. {err}")
			},
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
			self.side_length_partitions * (partition_dims.0 + margin) + 2 * margin,
			self.side_length_partitions * (partition_dims.1 + margin) + 2 * margin,
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
					Ok(_) => {},
					Err(e) => {
						println!("Failed to place cell {idx} at position {:?}. {}", pos, e);
					},
				}
				if failure.is_ok() {
					failure = res2;
				}
			}
			for x in (8..partition_dims.0).step_by(18) {
				for y in (8..partition_dims.1).step_by(18) {
					let id = PhyId(self.nodes.len());
					let x = part_x * (partition_dims.0 + margin) + x + margin;
					let y = part_y * (partition_dims.1 + margin) + y + margin;
					let pos = (x as f64 + 0.5, y as f64 + 0.5);
					self.nodes.push(PhyNode {
						id,
						logic: NodeId(usize::MAX),
						position: pos,
						placed: true,
						orientation: 4,
						partition: part as i32,
						hop_type: WireHopType::Substation,
					});
					assert_eq!(self.global_space[(x, y)], PhyId::default());
					assert_eq!(self.global_space[(x + 1, y)], PhyId::default());
					assert_eq!(self.global_space[(x, y + 1)], PhyId::default());
					assert_eq!(self.global_space[(x + 1, y + 1)], PhyId::default());
					self.global_space[(x, y)] = id;
					self.global_space[(x + 1, y)] = id;
					self.global_space[(x, y + 1)] = id;
					self.global_space[(x + 1, y + 1)] = id;
				}
			}
		}
	}

	fn print_ascii_global_space(&self) {
		println!(
			"global_space {} x {}:",
			self.global_space.dims().0,
			self.global_space.dims().1
		);
		print!("   ");
		for i in 0..self.global_space.dims().0 {
			print!(" {:3} ", i);
		}
		println!();
		for y in 0..self.global_space.dims().1 {
			print!("{y:3} ");
			for x in 0..self.global_space.dims().0 {
				if self.global_space[(x, y)] == PhyId::default() {
					print!(" ___ ");
					continue;
				}
				let node = &self.nodes[self.global_space[(x, y)].0];
				if node.is_pole() {
					if node.hop_type == WireHopType::Substation {
						print!(" _S_ ");
					} else {
						print!(" _P_ ");
					}
				} else {
					print!(" {:3} ", self.global_space[(x, y)].0)
				}
			}
			println!();
		}
	}

	fn route_nets(&mut self, logical: &LogicalDesign) -> bool {
		let mut success = true;
		let nets = Netlist::compute_networks(logical);
		let mut _net_id = 0;
		for net in nets.networks {
			_net_id += 1;
			let terminals = net
				.fanin
				.iter()
				.map(|id| (Direction::Output, *id))
				.chain(net.fanout.iter().map(|id| (Direction::Input, *id)))
				.collect_vec();
			let mut connected_group_count = 0;
			let mut connected_map: Vec<Option<usize>> = vec![None; terminals.len()];
			let mut cache = RouteCache::default();
			for v in connected_map.iter_mut() {
				if v.is_none() {
					connected_group_count += 1;
					*v = Some(connected_group_count);
				}
			}
			for idx_1 in 0..connected_map.len() {
				let group_1 = connected_map[idx_1];
				if connected_map[idx_1] != group_1 {
					continue;
				}
				for idx_2 in 0..connected_map.len() {
					let group_2 = connected_map[idx_2];
					if group_2 == group_1 {
						continue;
					}
					let node_1 = terminals[idx_1].1;
					let node_2 = terminals[idx_2].1;
					if !self.close_enough_to_connect(logical, node_1, node_2) {
						continue;
					}
					self.connect_wire2(
						terminals[idx_1].0,
						node_1,
						terminals[idx_2].0,
						node_2,
						net.colour,
						logical,
					);
					for x in &mut connected_map.iter_mut() {
						if *x == group_2 {
							*x = group_1;
						}
					}
				}
			}
			while connected_map.iter().any(|x| *x != connected_map[0]) {
				for (idx_1, (_dir_1, node_1)) in terminals.iter().enumerate() {
					if connected_map.iter().all(|x| *x == connected_map[0]) {
						break;
					}
					let idx_closest = (0..terminals.len())
						.filter(|idx_2| *idx_2 != idx_1)
						.filter(|idx_2| connected_map[*idx_2] != connected_map[idx_1])
						.min_by(|left, right| {
							let pos_1 = self.nodes[self.idx_combs[node_1].0].position;
							let left = self.idx_combs[&terminals[*left].1];
							let right = self.idx_combs[&terminals[*right].1];
							let left = self.nodes[left.0].position;
							let right = self.nodes[right.0].position;
							distance_diff(&pos_1, &left)
								.partial_cmp(&distance_diff(&pos_1, &right))
								.unwrap()
						})
						.unwrap();
					match self.route_partial_net(
						&terminals,
						&connected_map,
						idx_1,
						idx_closest,
						&mut cache,
					) {
						Ok(path) => {
							self.commit_route2(
								path,
								&terminals,
								&mut connected_map,
								idx_1,
								idx_closest,
								net.colour,
								&mut cache,
								logical,
							);
						},
						Err(s) => {
							self.failed_routes.push((
								self.idx_combs[&terminals[idx_1].1].0,
								self.idx_combs[&terminals[idx_closest].1].0,
							));
							println!("{}", s);
							success = false;
						},
					};
					let connected_group_1 = connected_map[idx_1];
					let connected_group_2 = connected_map[idx_closest];
					for x in &mut connected_map.iter_mut() {
						if *x == connected_group_1 {
							*x = connected_group_2;
						}
					}
				}
			}
		}

		success
	}

	fn add_pole(&mut self, loc: SpaceIndex, logic: NodeId) -> PhyId {
		let id = PhyId(self.nodes.len());
		assert!(self.global_space[(loc.0, loc.1)] == PhyId::default());
		self.nodes.push(PhyNode {
			id,
			logic,
			position: (loc.0 as f64, loc.1 as f64),
			placed: true,
			orientation: 4,
			partition: -1,
			hop_type: WireHopType::Medium,
		});
		self.global_space[(loc.0, loc.1)] = id;
		id
	}

	fn commit_route2(
		&mut self,
		mut path: Vec<SpaceIndex>,
		terminals: &Vec<(Direction, NodeId)>,
		connected_map: &mut Vec<Option<usize>>,
		idx_1: usize,
		idx_2: usize,
		colour: WireColour,
		cache: &mut RouteCache,
		logical: &LogicalDesign,
	) {
		path.reverse();
		let path = path;
		for i in 1..path.len() {
			let p1 = path[i - 1];
			let p2 = path[i];
			let dx = p2.0 - p1.0;
			let dy = p2.1 - p1.1;
			assert!(((dx * dx + dy * dy) as f64).sqrt() <= 11.0);
		}
		let group_1 = connected_map[idx_1];
		let group_2 = connected_map[idx_2];
		// find closest PhyId to path[end]
		let start_pos = path[0];
		let end_pole = path[path.len() - 1];
		let poles = path
			.into_iter()
			.skip(1)
			.map(|pole_pos| self.add_pole(pole_pos, NodeId(usize::MAX)))
			.collect_vec();

		let attachment_2 = (0..connected_map.len())
			.into_iter()
			.filter(|idx| connected_map[*idx] == group_2)
			.map(|idx| (&self.idx_combs[&terminals[idx].1], terminals[idx].0))
			.chain(
				cache
					.existing_poles
					.entry(group_2.unwrap())
					.or_default()
					.iter()
					.map(|x| (x, Direction::Input)),
			)
			.min_by(|(left, _), (right, _)| {
				let comb_left = self.nodes[left.0].position;
				let comb_right = self.nodes[right.0].position;
				let end_pole_pos = (end_pole.0 as f64, end_pole.1 as f64);
				let dist_left = distance_diff(&end_pole_pos, &comb_left);
				let dist_right = distance_diff(&end_pole_pos, &comb_right);
				dist_left.partial_cmp(&dist_right).unwrap()
			})
			.unwrap();

		let start_phy = self.global_space[start_pos.to_pair()];
		let attachment_1 = if self.nodes[start_phy.0].is_pole() {
			(start_phy, Direction::Input)
		} else {
			(
				start_phy,
				terminals
					.iter()
					.find(|v| v.1 == self.nodes[start_phy.0].logic)
					.unwrap()
					.0,
			)
		};

		for i in 1..poles.len() {
			let wire_id = self.wires.len();
			self.wires.push(Wire {
				id: WireId(wire_id),
				node1_id: poles[i - 1],
				node2_id: poles[i],
				terminal1_id: TerminalId::input(colour),
				terminal2_id: TerminalId::input(colour),
			});
		}
		{
			let wire_id = self.wires.len();
			self.wires.push(Wire {
				id: WireId(wire_id),
				node1_id: attachment_1.0,
				node2_id: poles[0],
				terminal1_id: self.get_terminal_for_phy(
					attachment_1.0,
					attachment_1.1,
					colour,
					logical,
				),
				terminal2_id: TerminalId::input(colour),
			});
		}
		{
			let wire_id = self.wires.len();
			self.wires.push(Wire {
				id: WireId(wire_id),
				node1_id: *attachment_2.0,
				node2_id: poles[poles.len() - 1],
				terminal1_id: self.get_terminal_for_phy(
					*attachment_2.0,
					attachment_2.1,
					colour,
					logical,
				),
				terminal2_id: TerminalId::input(colour),
			});
			let p1 = self.nodes[attachment_2.0 .0].position;
			let p2 = self.nodes[poles[poles.len() - 1].0].position;
			let ds = distance_diff(&p1, &p2);
			assert!(ds <= 11.0);
		}
		for pole in poles {
			cache
				.existing_poles
				.entry(group_1.unwrap())
				.or_default()
				.push(pole);
		}
		let mut g2_poles = vec![];
		std::mem::swap(
			cache.existing_poles.entry(group_2.unwrap()).or_default(),
			&mut g2_poles,
		);
		cache
			.existing_poles
			.get_mut(&group_1.unwrap())
			.unwrap()
			.extend(g2_poles);
		for x in connected_map.iter_mut() {
			if *x == group_2 {
				*x = group_1;
			}
		}
	}

	fn get_terminal_for_phy(
		&self,
		id: PhyId,
		dir: Direction,
		colour: WireColour,
		logical: &LogicalDesign,
	) -> TerminalId {
		let comb = &self.nodes[id.0];
		if comb.is_pole() {
			return TerminalId::input(colour);
		}
		let ld_comb = logical.get_node(self.nodes[id.0].logic);
		match &ld_comb.function {
			ld::NodeFunction::Arithmetic { .. } | ld::NodeFunction::Decider { .. } => {
				if dir == Direction::Input {
					TerminalId::input(colour)
				} else {
					TerminalId::output_combinator(colour)
				}
			},
			ld::NodeFunction::Constant { .. }
			| ld::NodeFunction::Lamp { .. }
			| ld::NodeFunction::DisplayPanel { .. } => TerminalId::output_constant(colour),
			ld::NodeFunction::WireSum(_c) => unreachable!(),
		}
	}

	fn global_freeze_and_route2(
		&mut self,
		logical: &LogicalDesign,
		local_to_global: &Vec<Vec<usize>>,
		_global_to_local: &Vec<HashM<usize, usize>>,
	) -> bool {
		let partition_dims = calculate_minimum_partition_dim(&self.local_assignments);
		println!(
			"Starting global freeze and route with partition dims ({}, {}).",
			partition_dims.0, partition_dims.1
		);
		let margin_range = self.user_partition_margin.map(|m| m..=m).unwrap_or(1..=10);
		let mut success = false;

		for margin in margin_range {
			println!("Starting routing with margin {margin}");
			success = true;
			self.intra_partition_margin = margin;
			self.reset_place_route();
			self.global_freeze(logical, margin, partition_dims, local_to_global);
			self.save_svg(logical, format!("svg/global_freeze_{margin}.svg"))
				.ok();

			let success = self.route_nets(logical);
			if success {
				break;
			} else {
				println!("Failed to route nets. Writing svg for debugging at svg/global_freeze_routing_failure_{margin}.svg");
				self.save_svg(
					logical,
					format!("svg/global_freeze_routing_failure_{margin}.svg"),
				)
				.ok();
			}
		}
		#[cfg(debug_assertions)]
		{
			//self.print_ascii_global_space();
		}
		success
	}

	fn route_partial_net(
		&mut self,
		terminals: &Vec<(Direction, NodeId)>,
		connected_map: &Vec<Option<usize>>,
		idx_1: usize,
		idx_2: usize,
		cache: &mut RouteCache,
	) -> Result<Vec<SpaceIndex>, String> {
		let group_1 = connected_map[idx_1];
		let group_2 = connected_map[idx_2];
		let node_id_1 = terminals[idx_1].1;
		let node_id_2 = terminals[idx_2].1;

		let mut b = vec![];
		{
			// Add all ending points in group 2.
			for idx_3 in 0..terminals.len() {
				if connected_map[idx_3] != group_2 {
					continue;
				}
				let node_id_3 = terminals[idx_3].1;
				let comb3 = &self.nodes[self.idx_combs[&node_id_3].0];
				let neig3 = self.get_neighbors(
					&SpaceIndex(comb3.position.0 as usize, comb3.position.1 as usize),
					&SpaceIndex::default(),
				);
				for pos in neig3 {
					b.push(pos);
				}
				for pole_id in cache.existing_poles.entry(group_2.unwrap()).or_default() {
					let pole = &self.nodes[pole_id.0];
					let neig_pole = self.get_neighbors(
						&SpaceIndex(pole.position.0 as usize, pole.position.1 as usize),
						&SpaceIndex::default(),
					);
					for pos in neig_pole {
						b.push(pos);
					}
				}
			}
		}
		let b = b.into_iter().unique().collect_vec();

		let mut a = vec![];
		{
			// Add all starting points in group 1.
			for idx_3 in 0..terminals.len() {
				if connected_map[idx_3] != group_1 {
					continue;
				}
				let node_id_3 = terminals[idx_3].1;
				let comb3 = &self.nodes[self.idx_combs[&node_id_3].0];
				a.push(SpaceIndex(
					comb3.position.0 as usize,
					comb3.position.1 as usize,
				));
			}
			for pole_id in cache.existing_poles.entry(group_1.unwrap()).or_default() {
				let pole_pos = self.nodes[pole_id.0].position;
				a.push(SpaceIndex(pole_pos.0 as usize, pole_pos.1 as usize));
			}
		}

		let ret = route::a_star_initial_set(self, &a, &b, None);
		if ret.is_empty() {
			return Result::Err(format!("Could not route {} -> {}", node_id_1, node_id_2));
		}
		Result::Ok(ret)
	}

	pub(crate) fn route_single_net(
		&mut self,
		logical: &LogicalDesign,
		id_1: NodeId,
		dir_1: Direction,
		id_2: NodeId,
		dir_2: Direction,
		colour: WireColour,
	) -> Result<(), ()> {
		if self.close_enough_to_connect_phy(logical, self.idx_combs[&id_1], self.idx_combs[&id_2]) {
			self.connect_wire2(dir_1, id_1, dir_2, id_2, colour, logical);
			return Ok(());
		}
		let terminals = vec![(dir_1, id_1), (dir_2, id_2)];
		let mut connected_map = vec![Some(1), Some(2)];
		let idx_1 = 0;
		let idx_2 = 1;
		let mut cache = RouteCache::default();
		let path = self
			.route_partial_net(&terminals, &connected_map, idx_1, idx_2, &mut cache)
			.map_err(|_| ())?;
		self.commit_route2(
			path,
			&terminals,
			&mut connected_map,
			idx_1,
			idx_2,
			colour,
			&mut cache,
			logical,
		);
		Ok(())
	}

	fn fill_coverage(&mut self) {
		let mut coverage = Arr2::new([self.global_space.dims().0, self.global_space.dims().1]);
		for x in 0..self.global_space.dims().0 {
			for y in 0..self.global_space.dims().1 {
				if self.global_space[(x, y)] == PhyId::default() {
					continue;
				}
				let id = self.global_space[(x, y)];
				let node = &self.nodes[id.0];
				if !node.is_pole() {
					continue;
				}
				let spec = WireHopType::Medium.wire_hop_spec();
				for i in -spec.reach as usize..=((spec.area - 1) / 2) as usize {
					for j in -spec.reach as usize..=((spec.area - 1) / 2) as usize {
						if !self.global_space.index_good((x + i, y + j)) {
							continue;
						}
						coverage[(x + i, y + j)] = true;
					}
				}
			}
		}
		let mut no_coverage: HashS<_> = (0..self.n_nodes())
			.filter(|idx| !self.nodes[*idx].is_pole())
			.collect();
		for x in 0..self.global_space.dims().0 {
			for y in 0..self.global_space.dims().1 {
				if self.global_space[(x, y)] == PhyId::default() {
					continue;
				}
				if coverage[(x, y)] {
					no_coverage.remove(&self.global_space[(x, y)].0);
				}
			}
		}

		loop {
			let target = no_coverage.iter().find(|_| true);
			let target = if let Some(idx) = target {
				PhyId(*idx)
			} else {
				break;
			};
			no_coverage.remove(&target.0);
			let x = self.nodes[target.0].position.0 as usize;
			let y = self.nodes[target.0].position.1 as usize;
			let reach = WireHopType::Medium.wire_hop_spec().area as isize / 2;
			let mut best_pos = (x, y);
			let mut best_count = 0;
			for i in -reach..=reach {
				for j in -reach..=reach {
					let pos = (x + i as usize, y + j as usize);
					if !self.global_space.index_good(pos) {
						continue;
					}
					if self.global_space[pos] != PhyId::default() {
						continue;
					}
					let (x, y) = pos;
					let mut seen = hash_set();
					let mut count = 0;
					for i in -reach..=reach {
						for j in -reach..=reach {
							let pos = (x + i as usize, y + j as usize);
							if !self.global_space.index_good(pos) {
								continue;
							}
							if self.global_space[pos] == PhyId::default() {
								continue;
							}
							let new_seen = &self.global_space[pos].0;
							if seen.contains(&new_seen) {
								continue;
							}
							seen.insert(new_seen);
							if no_coverage.contains(new_seen) || *new_seen == target.0 {
								count += 1
							}
						}
					}
					if count > best_count {
						best_count = count;
						best_pos = (x, y);
					}
				}
			}
			if self.global_space[best_pos] == PhyId::default() {
				self.add_pole(SpaceIndex(best_pos.0, best_pos.1), NodeId(usize::MAX));
				let (x, y) = best_pos;
				for i in -reach..=reach {
					for j in -reach..=reach {
						let pos = (x + i as usize, y + j as usize);
						if !self.global_space.index_good(pos) {
							continue;
						}
						coverage[pos] = true;
						if self.global_space[pos] != PhyId::default() {
							no_coverage.remove(&self.global_space[pos].0);
						}
					}
				}
			}
		}
	}

	pub(crate) fn build_copper_network(&self) -> Vec<Vec<PhyId>> {
		let mut all_neighbours = vec![vec![]; self.n_nodes()];
		let mut rng = rand::rng();
		for x in 0..self.global_space.dims().0 {
			for y in 0..self.global_space.dims().1 {
				if self.global_space[(x, y)] == PhyId::default() {
					continue;
				}
				let id = self.global_space[(x, y)];
				let node = &self.nodes[id.0];
				if !node.is_pole() {
					continue;
				}
				let mut potential_neighbors = vec![];
				let spec = node.hop_type.wire_hop_spec();
				for i in -spec.reach as usize..=spec.reach as usize {
					for j in -spec.reach as usize..=spec.reach as usize {
						if !self.global_space.index_good((x + i, y + j)) {
							continue;
						}
						let dist_sq = (i * i + j * j) as f64;
						if dist_sq > spec.reach * spec.reach {
							continue;
						}
						let id2 = self.global_space[(x + i, y + j)];
						if id2 == PhyId::default() {
							continue;
						}
						let node2 = &self.nodes[id2.0];
						let spec2 = node2.hop_type.wire_hop_spec();
						if !node2.is_pole() {
							continue;
						}
						let dist_sq2 = (i * i + j * j) as f64;
						if dist_sq2 > spec2.reach * spec2.reach {
							continue;
						}
						potential_neighbors.push((id2, dist_sq));
					}
				}
				//potential_neighbors.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap());

				potential_neighbors.shuffle(&mut rng);
				all_neighbours[id.0] = potential_neighbors
					.iter()
					.map(|x| x.0)
					.unique()
					.collect_vec();
			}
		}

		let mut ret = vec![vec![]; self.n_nodes()];
		let mut seen = vec![false; self.n_nodes()];
		let mut n_neighbors = vec![0; self.n_nodes()];
		while seen
			.iter()
			.enumerate()
			.any(|(idx, x)| !*x && self.nodes[idx].is_pole())
		{
			let mut queue = all_neighbours
				.iter()
				.enumerate()
				.find(|x| !x.1.is_empty() && !seen[x.0])
				.map(|(idx, _)| (PhyId(idx), PhyId::default()))
				.into_iter()
				.collect_vec();
			loop {
				let (id, last) = if let Some(x) = queue.pop() {
					x
				} else {
					break;
				};
				if last != PhyId::default() && n_neighbors[last.0] < 4 {
					ret[id.0].push(last);
					ret[last.0].push(id);
					n_neighbors[id.0] += 1;
					n_neighbors[last.0] += 1;
				}
				if seen[id.0] {
					continue;
				}
				seen[id.0] = true;
				let mut counter = 0;
				for x in all_neighbours[id.0].iter() {
					if counter > 2 {
						break;
					}
					queue.push((*x, id));
					counter += 1;
				}
			}
		}
		ret
	}

	fn get_neighbors(&self, index: &SpaceIndex, _goal: &SpaceIndex) -> SpaceIter {
		let min_x = (index.0 as isize - 9).max(0) as usize;
		let min_y = (index.1 as isize - 9).max(0) as usize;
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

	pub fn extend(
		&mut self,
		other: PhysicalDesign,
		logical_design_offset: usize,
		pos: (usize, usize),
	) -> Result<usize, ()> {
		let offset = self.nodes.len();
		for x in pos.0..self.global_space.dims().0 {
			for y in pos.1..self.global_space.dims().1 {
				if self.global_space[(x, y)] != PhyId::default() {
					return Err(());
				}
			}
		}
		let new_dim0 = (pos.0 + other.global_space.dims().0).max(self.global_space.dims().0);
		let new_dim1 = (pos.1 + other.global_space.dims().1).max(self.global_space.dims().1);

		let mut new_global_space = Arr2::<PhyId>::new([new_dim0, new_dim1]);

		for x in 0..self.global_space.dims().0 {
			for y in 0..self.global_space.dims().1 {
				new_global_space[(x, y)] = self.global_space[(x, y)];
			}
		}
		for x in 0..other.global_space.dims().0 {
			for y in 0..other.global_space.dims().1 {
				new_global_space[(x + pos.0, y + pos.1)].0 = other.global_space[(x, y)].0 + offset;
			}
		}
		self.global_space = new_global_space;

		for mut node in other.nodes {
			node.id.0 += offset;
			node.logic.0 += logical_design_offset;
			node.position.0 += pos.0 as f64;
			node.position.1 += pos.1 as f64;
			self.idx_combs.insert(node.logic, node.id);
			self.nodes.push(node);
		}

		let offset_wire = self.wires.len();
		for mut wire in other.wires {
			wire.id.0 += offset_wire;
			wire.node1_id.0 += offset;
			wire.node2_id.0 += offset;
			self.wires.push(wire);
		}
		Ok(offset)
	}

	pub fn check_free_make_space(&mut self, x: usize, y: usize, dimx: usize, dimy: usize) -> bool {
		for x in x..(x + dimx).min(self.global_space.dims().0) {
			for y in y..(y + dimy).min(self.global_space.dims().1) {
				if self.global_space[(x, y)] != PhyId::default() {
					return false;
				}
			}
		}
		if x + dimx > self.global_space.dims().0 || y + dimy > self.global_space.dims().1 {
			let new_dim0 = (x + dimx).max(self.global_space.dims().0);
			let new_dim1 = (y + dimy).max(self.global_space.dims().1);
			let mut new_global_space = Arr2::<PhyId>::new([new_dim0, new_dim1]);

			for x in 0..self.global_space.dims().0 {
				for y in 0..self.global_space.dims().1 {
					new_global_space[(x, y)] = self.global_space[(x, y)];
				}
			}
			self.global_space = new_global_space;
		}
		true
	}

	pub fn place_new_comb(
		&mut self,
		pos: (usize, usize),
		logical: &LogicalDesign,
		id: NodeId,
	) -> Result<PhyId, String> {
		let node = logical.get_node(id);
		let hop_type = node.function.wire_hop_type();
		let hop_spec = hop_type.wire_hop_spec();

		if !self.check_free_make_space(
			pos.0,
			pos.1,
			hop_spec.dim.0 as usize,
			hop_spec.dim.1 as usize,
		) {
			return Err("No space".to_owned());
		}
		let phy_id = PhyId(self.nodes.len());
		self.idx_combs.insert(id, phy_id);
		self.nodes.push(PhyNode {
			id: phy_id,
			logic: id,
			position: (0.0, 0.0),
			placed: false,
			orientation: 4,
			partition: -1,
			hop_type,
		});
		self.place_comb_physical((pos.0 as f64, pos.1 as f64), phy_id, logical, None)
			.unwrap();
		Ok(phy_id)
	}

	pub(crate) fn is_pole(&self, id: PhyId) -> bool {
		self.nodes[id.0].is_pole()
	}

	fn make_assertions(&self, _logical: &LogicalDesign) {
		let mut failed = false;
		for wire in &self.wires {
			let id1 = wire.node1_id.0;
			let id2 = wire.node2_id.0;
			let node1 = &self.nodes[id1];
			let node2 = &self.nodes[id2];
			if node1.id != wire.node1_id {
				println!("Mismatch for {id1:?}, {id2:?} and {wire:?}");
				failed = true;
			}
			if node2.id != wire.node2_id {
				println!("Mismatch for {id1:?}, {id2:?} and {wire:?}");
				failed = true;
			}
			let ds = distance_diff(&node1.position, &node2.position);
			if ds > 11.0 {
				println!("Wire too long for {id1:?}, {id2:?} and {wire:?}: ds = {ds}");
				failed = true;
			}
		}
		assert!(!failed, "Design failed assertions");
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

pub(crate) fn adjacency_to_edges_weighted(
	conn_list: &Vec<Vec<(usize, usize)>>,
	triangular: bool,
) -> Vec<(usize, usize, usize)> {
	let mut ret = vec![];
	for (id_i, connected_j) in conn_list.iter().enumerate() {
		for id_j in connected_j {
			if triangular && id_j.0 < id_i {
				continue;
			}
			ret.push((id_i, id_j.0, id_j.1));
		}
	}
	ret
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

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, Default)]
struct SpaceIndex(usize, usize);

impl route::TopologyIndex for SpaceIndex {}

impl SpaceIndex {
	fn to_pair(self) -> (usize, usize) {
		(self.0, self.1)
	}
}

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
		let dist = (dx * dx + dy * dy).sqrt();
		1.0 + dist / 100.0
	}

	fn heuristic(&self, a: &SpaceIndex, b: &SpaceIndex) -> f32 {
		let dx = b.0.abs_diff(a.0) as f32;
		let dy = b.1.abs_diff(a.1) as f32;
		let dist = (dx * dx + dy * dy).sqrt();
		((dist + 4.5) / 9.0).round()
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
	dim: (f64, f64),
	reach: f64,
	area: i32,
}

impl WireHopSpec {
	fn new(dim: (f64, f64), reach: f64, area: i32) -> Self {
		WireHopSpec { dim, reach, area }
	}
}

impl WireHopType {
	fn wire_hop_spec(&self) -> WireHopSpec {
		match self {
			WireHopType::Small => WireHopSpec::new((1.0, 1.0), 7.5, 5),
			WireHopType::Medium => WireHopSpec::new((1.0, 1.0), 9.0, 7),
			WireHopType::Big => WireHopSpec::new((2.0, 2.0), 32.0, 4),
			WireHopType::Substation => WireHopSpec::new((2.0, 2.0), 18.0, 18),
			WireHopType::Combinator => WireHopSpec::new((2.0, 1.0), 10.0, 0),
			WireHopType::Lamp => WireHopSpec::new((1.0, 1.0), 9.0, 0),
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
			ld::NodeFunction::DisplayPanel { .. } => WireHopType::Lamp,
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

impl WireHopType {
	#[allow(dead_code)]
	pub fn is_comb(&self) -> bool {
		matches!(self, Self::Combinator | Self::Lamp)
	}

	pub fn is_pole(&self) -> bool {
		matches!(
			self,
			Self::Big | Self::Small | Self::Medium | Self::Substation
		)
	}
}

impl PhyNode {
	pub fn is_pole(&self) -> bool {
		self.hop_type.is_pole()
	}
}

#[cfg(test)]
mod test {

	use std::fs;

	use super::*;
	#[test]
	fn new() {
		let mut p = PhysicalDesign::new();
		let l = crate::tests::logical_design_tests::get_simple_logical_design();
		p.build_from(&l);
	}

	#[test]
	fn n_combs() {
		let mut p = PhysicalDesign::new();
		let l = crate::tests::logical_design_tests::get_large_logical_design(200);
		p.build_from(&l);
		p.save_svg(&l, "svg/large_logical_design.svg")
			.expect("Failed to save");
		fs::rename("svg/partition3.svg", "svg/large_logical_design_p3.svg")
			.expect("Failed to move");
	}

	#[test]
	fn memory_n_mcmc() {
		let mut p = PhysicalDesign::new();
		let l = crate::tests::logical_design_tests::get_large_memory_test_design(40);
		p.build_from(&l);
		p.save_svg(&l, "svg/memory_n_mcmc.svg")
			.expect("Failed to save");
	}

	#[test]
	fn dense_memory_n_mcmc() {
		let mut p = PhysicalDesign::new();
		let l = crate::tests::logical_design_tests::get_large_dense_memory_test_design(1_024);
		p.build_from(&l);
		p.save_svg(&l, "svg/dense_memory_n_mcmc.svg")
			.expect("Failed to save");

		//let mut s = serializable_design::SerializableDesign::new();
		//s.build_from(&p, &l);
		//let blueprint_json: String = serde_json::to_string(&s).unwrap();
		//let mut output = File::create("./output/dense_memory_n_mcmc_dense.json").unwrap();
		//output.write_all(blueprint_json.as_bytes()).unwrap();
	}

	#[test]
	fn sweep_synthetic_n_mcmc_dense() {
		for x in (20..=100).step_by(20) {
			println!("==============={x}===============");
			let mut p = PhysicalDesign::new();
			let l = crate::tests::logical_design_tests::get_large_logical_design(x);
			p.build_from(&l);
		}
	}

	#[test]
	fn synthetic_n_mcmc_dense() {
		let mut p = PhysicalDesign::new();
		let l = crate::tests::logical_design_tests::get_large_logical_design(200);
		p.build_from(&l);
		p.save_svg(&l, "svg/synthetic_n_mcmc_dense.svg")
			.expect("Failed to save");
	}

	#[test]
	fn synthetic_2d_n_mcmc_dense() {
		let mut p = PhysicalDesign::new();
		let l = crate::tests::logical_design_tests::get_large_logical_design_2d(20);
		p.user_partition_size = Some(200);
		let res = p.build_from(&l);
		p.save_svg(&l, "svg/synthetic_2d_n_mcmc_dense.svg")
			.expect("Failed to save");
		assert!(res)
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
		p.save_svg(&l, "svg/comb_pair.svg").expect("Failed to save");
	}
}

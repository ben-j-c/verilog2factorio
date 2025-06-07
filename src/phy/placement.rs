use std::collections::HashSet;
use std::{collections::BTreeSet, hash::BuildHasherDefault, isize, mem::swap, usize};

use hashers::fnv::FNV1aHasher64;
use itertools::Itertools;
use rand::{rngs::StdRng, Rng};

use crate::util;
use crate::{mapped_design::Integer, ndarr::Arr2, svg::SVG};

pub(crate) fn ripup_replace_method(
	rng: &mut StdRng,
	_curr: &Placement,
	new: &mut Placement,
	_connections_per_node: &Vec<Vec<usize>>,
	side_length: usize,
	max_density: i32,
) {
	let num_cells = new.num_cells();
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
		new.ripup(*cell);
	}

	for cell in &ripup_cells {
		loop {
			let cxcy = (
				rng.random_range(0..side_length) / 2 * 2,
				rng.random_range(0..side_length),
			);
			if new.density(cxcy) < max_density {
				new.place(*cell, cxcy);
				break;
			}
		}
	}
}

pub(crate) fn swap_local_method(
	rng: &mut StdRng,
	_curr: &Placement,
	new: &mut Placement,
	_connections_per_node: &Vec<Vec<usize>>,
	side_length: usize,
	_max_density: i32,
) {
	let num_cells = new.num_cells();
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
		let cxcy = new.assignment(*cell);
		loop {
			let pick_x: i32 = rng.random_range(-1..=1) * 2;
			let pick_y: i32 = rng.random_range(-1..=1);
			let want_x = cxcy.0 as isize + pick_x as isize;
			let want_y = cxcy.1 as isize + pick_y as isize;
			if pick_x == 0 && pick_y == 0
				|| want_x < 0
				|| want_x >= side_length as isize
				|| want_y < 0
				|| want_y >= side_length as isize
			{
				continue;
			}
			let want_assignment = (want_x as usize, want_y as usize);
			let candidates = new.id_at(want_assignment).iter().collect_vec();
			if candidates.is_empty() {
				new.mov(*cell, want_assignment);
			} else {
				let pick_cell = candidates[rng.random_range(0..candidates.len())];
				new.swap(*cell, *pick_cell);
			}
			break;
		}
	}
}

pub(crate) fn swap_random_method(
	rng: &mut StdRng,
	_curr: &Placement,
	new: &mut Placement,
	_connections_per_node: &Vec<Vec<usize>>,
	_side_length: usize,
	_max_density: i32,
) {
	let num_cells = new.num_cells();
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
		new.swap(c1, c2);
	}
}

pub(crate) fn ripup_range_method(
	rng: &mut StdRng,
	_curr: &Placement,
	new: &mut Placement,
	_connections_per_node: &Vec<Vec<usize>>,
	side_length: usize,
	max_density: i32,
) {
	let region_w = rng.random_range(1..=10);
	let region_h = rng.random_range(1..=10);

	let sx = rng.random_range(0..(side_length - region_w).max(1));
	let sy = rng.random_range(0..(side_length - region_h).max(1));

	let mut ripup_cells = vec![];
	for x in sx..sx + region_w {
		for y in sy..sy + region_h {
			ripup_cells.extend(new.ripup_loc((x, y)));
		}
	}

	for cell in &ripup_cells {
		loop {
			let assignment = (
				rng.random_range(0..side_length) / 2 * 2,
				rng.random_range(0..side_length),
			);
			if new.density(assignment) < max_density {
				new.place(*cell, assignment);
				break;
			}
		}
	}
}

pub(crate) fn crack_in_two_method(
	rng: &mut StdRng,
	_curr: &Placement,
	new: &mut Placement,
	_connections_per_node: &Vec<Vec<usize>>,
	side_length: usize,
	max_density: i32,
) {
	if side_length < 6 {
		return;
	}
	let num_cells = new.num_cells();
	let selection = (
		rng.random_range(2..side_length - 4) / 2 * 2,
		rng.random_range(1..side_length - 2),
	);
	let (offset, corner1, corner2, sorter) = match (rng.random_bool(0.5), rng.random_bool(0.5)) {
		(true, true) => ((-2, 0), (0, 0), (selection.0, side_length), 1),
		(true, false) => ((0, -1), (0, 0), (side_length, selection.1), 2),
		(false, true) => ((2, 0), (selection.0, 0), (side_length, side_length), 3),
		(false, false) => (
			(0, 1),
			(side_length, selection.1),
			(side_length, side_length),
			4,
		),
	};

	let mut ripup_cells = Vec::with_capacity(num_cells);
	for x in (0..side_length).step_by(2) {
		for y in 0..side_length {
			if x < corner1.0 || x > corner2.0 || y < corner1.1 || y > corner2.1 {
				continue;
			}
			ripup_cells.extend(new.ripup_loc((x, y)));
		}
	}

	match sorter {
		1 => ripup_cells
			.sort_by(|a: &usize, b: &usize| new.assignment(*a).0.cmp(&new.assignment(*b).0)),
		2 => ripup_cells
			.sort_by(|a: &usize, b: &usize| new.assignment(*a).1.cmp(&new.assignment(*b).1)),
		3 => ripup_cells
			.sort_by(|a: &usize, b: &usize| new.assignment(*b).0.cmp(&new.assignment(*a).0)),
		4 => ripup_cells
			.sort_by(|a: &usize, b: &usize| new.assignment(*b).1.cmp(&new.assignment(*a).1)),
		_ => {
			unreachable!()
		},
	};

	for cell in ripup_cells {
		let assignment = new.assignment(cell);
		let want_assignment = (
			(assignment.0 as isize + offset.0) as usize,
			(assignment.1 as isize + offset.1) as usize,
		);
		if want_assignment.0 < side_length
			&& want_assignment.1 < side_length
			&& new.density(want_assignment) < max_density
		{
			new.place(cell, want_assignment);
		} else {
			new.place(cell, assignment);
		}
	}
}

pub(crate) fn slide_puzzle_method(
	rng: &mut StdRng,
	_curr: &Placement,
	new: &mut Placement,
	_connections_per_node: &Vec<Vec<usize>>,
	side_length: usize,
	max_density: i32,
) {
	if side_length < 10 {
		return;
	}
	let num_cells = new.num_cells();
	let selection = (
		rng.random_range(4..side_length - 4) / 2 * 2,
		rng.random_range(3..side_length - 3),
	);
	let (offset, corner1, corner2, sorter) = match (rng.random_bool(0.5), rng.random_bool(0.5)) {
		(true, true) => ((-2, 0), (0, selection.1), (selection.0, selection.1), 1),
		(true, false) => ((0, -1), (selection.0, 0), (selection.0, selection.1), 2),
		(false, true) => (
			(2, 0),
			(selection.0, selection.1),
			(side_length, selection.1),
			3,
		),
		(false, false) => (
			(0, 1),
			(selection.0, selection.1),
			(selection.0, side_length),
			4,
		),
	};

	let mut ripup_cells = Vec::with_capacity(num_cells);
	for x in (0..side_length).step_by(2) {
		for y in 0..side_length {
			if x < corner1.0 || x > corner2.0 || y < corner1.1 || y > corner2.1 {
				continue;
			}
			ripup_cells.extend(new.ripup_loc((x, y)));
		}
	}

	match sorter {
		1 => ripup_cells
			.sort_by(|a: &usize, b: &usize| new.assignment(*a).0.cmp(&new.assignment(*b).0)),
		2 => ripup_cells
			.sort_by(|a: &usize, b: &usize| new.assignment(*a).1.cmp(&new.assignment(*b).1)),
		3 => ripup_cells
			.sort_by(|a: &usize, b: &usize| new.assignment(*b).0.cmp(&new.assignment(*a).0)),
		4 => ripup_cells
			.sort_by(|a: &usize, b: &usize| new.assignment(*b).1.cmp(&new.assignment(*a).1)),
		_ => {
			unreachable!()
		},
	};

	for cell in ripup_cells {
		let assignment = new.assignment(cell);
		let want_assignment = (
			(assignment.0 as isize + offset.0) as usize,
			(assignment.1 as isize + offset.1) as usize,
		);
		if want_assignment.0 < side_length
			&& want_assignment.1 < side_length
			&& new.density(want_assignment) < max_density
		{
			new.place(cell, want_assignment);
		} else {
			new.place(cell, assignment);
		}
	}
}

pub(crate) fn slide_puzzle_method_worst_cells(
	rng: &mut StdRng,
	_curr: &Placement,
	new: &mut Placement,
	_connections_per_node: &Vec<Vec<usize>>,
	_side_length: usize,
	max_density: i32,
) {
	let mut interesting_cells = vec![];
	let mut interesting_cells_assignment = vec![];
	for (idx, cass) in new.assignments.iter().enumerate() {
		if new.density(*cass) > max_density && interesting_cells_assignment.last() != Some(&cass) {
			interesting_cells.push(idx);
			interesting_cells_assignment.push(cass);
		}
	}
	for picked_cell in interesting_cells {
		let cass = new.assignment(picked_cell);
		new.slide(
			cass,
			max_density,
			rng.random_bool(0.5),
			rng.random_bool(0.5),
		);
		if new.density(cass) < max_density {
			new.mov(picked_cell, cass);
		}
	}
}

pub(crate) fn overflowing_cells_swap_local_method(
	rng: &mut StdRng,
	_curr: &Placement,
	new: &mut Placement,
	_connections_per_node: &Vec<Vec<usize>>,
	side_length: usize,
	_max_density: i32,
) {
	let mut swap_cells = vec![];
	for (idx, assignment) in new.assignments.iter().enumerate() {
		if new.density(*assignment) > 1 {
			swap_cells.push(idx);
		}
	}

	for cell in &swap_cells {
		let assignment = new.assignment(*cell);
		loop {
			let pick_x: i32 = rng.random_range(-1..=1) * 2;
			let pick_y: i32 = rng.random_range(-1..=1);
			let want_x = assignment.0 as isize + pick_x as isize;
			let want_y = assignment.1 as isize + pick_y as isize;
			let want_assignment = (want_x as usize, want_y as usize);
			if assignment == want_assignment
				|| want_assignment.0 >= side_length
				|| want_assignment.1 >= side_length
			{
				continue;
			}
			let candidates = new.id_at(want_assignment).iter().collect_vec();
			if candidates.is_empty() {
				new.mov(*cell, want_assignment);
			} else {
				let candidate = candidates[rng.random_range(0..candidates.len())];
				new.swap(*candidate, *cell);
			}
			break;
		}
	}
}

pub(crate) fn simulated_spring_method(
	rng: &mut StdRng,
	_curr: &Placement,
	new: &mut Placement,
	connections_per_node: &Vec<Vec<usize>>,
	side_length: usize,
	max_density: i32,
) {
	let num_cells = new.num_cells();
	let iters = rng.random_range(1..100);
	let mut offx = 2;
	let mut offy = 1;
	for _ in 0..0 {
		let i: isize = random_01(rng, 0.05);
		offx += 2 * i;
		offy += i;
	}
	for _ in 0..iters {
		let mut force = new
			.assignments
			.iter()
			.enumerate()
			.map(|(idx, (x0, y0))| {
				(
					idx,
					connections_per_node[idx]
						.iter()
						.map(|idx2| {
							let (x1, y1) = new.assignment(*idx2);
							let dx = x1 as isize - *x0 as isize;
							let dy = y1 as isize - *y0 as isize;
							(dx * dx * dx.signum(), dy * dy * dy.signum())
						})
						.reduce(|(x0, y0), (x1, y1)| (x0 + x1, y0 + y1))
						.unwrap_or((0, 0)),
				)
			})
			.collect_vec();
		for (cell1, (x, y)) in new.assignments.iter().enumerate() {
			let offsets = vec![-2, 0, 2]
				.into_iter()
				.cartesian_product(vec![-1, 0, 1])
				.collect_vec();
			for (dx, dy) in offsets {
				if dx == 0 && dy == 0 {
					continue;
				}
				let check_pos = ((*x as isize + dx) as usize, (*y as isize + dy) as usize);
				if check_pos.0 >= side_length || check_pos.1 >= side_length {
					continue;
				}
				for cell2 in new.id_at(check_pos) {
					if connections_per_node[cell1].contains(cell2) {
						continue;
					}
					let (idx, (fx, fy)) = force[*cell2];
					let dfx = dx.signum() * (1 - dx.abs() / 2 + 1) * 10;
					let dfy = dy.signum() * (1 - dy.abs() + 1) * 10;
					force[*cell2] = (idx, (fx + dfx, fy + dfy));
				}
			}
		}
		force.sort_by(|(_, fa), (_, fb)| {
			(fa.0 * fa.0 + fa.1 * fa.1)
				.cmp(&(fb.0 * fb.0 + fb.1 * fb.1))
				.reverse()
		});
		let swap_count = rng.random_range(1..num_cells);
		for (idx1, f1) in force.iter().take(swap_count) {
			let (fx, fy) = f1;
			if *fx == 0 && *fy == 0 {
				continue;
			}
			let assignment1 = new.assignment(*idx1);
			// candidates: (ids: hash_set<usize>, empty_positions: hash_set<(usize, usize)>)
			let candidates = if *fx > 0 {
				if *fy > 0 {
					new.id_in_range_exclude_a1(
						assignment1,
						(assignment1.0 as isize + offx, assignment1.1 as isize + offy),
					)
				} else if *fy == 0 {
					new.id_in_range_exclude_a1(
						assignment1,
						(assignment1.0 as isize + offx, assignment1.1 as isize),
					)
				} else {
					new.id_in_range_exclude_a1(
						assignment1,
						(assignment1.0 as isize + offx, assignment1.1 as isize - offy),
					)
				}
			} else if *fx == 0 {
				if *fy > 0 {
					new.id_in_range_exclude_a1(
						assignment1,
						(assignment1.0 as isize, assignment1.1 as isize + offy),
					)
				} else if *fy == 0 {
					new.id_in_range_exclude_a1(
						assignment1,
						(assignment1.0 as isize, assignment1.1 as isize),
					)
				} else {
					new.id_in_range_exclude_a1(
						assignment1,
						(assignment1.0 as isize, assignment1.1 as isize - offy),
					)
				}
			} else {
				if *fy > 0 {
					new.id_in_range_exclude_a1(
						assignment1,
						(assignment1.0 as isize - offx, assignment1.1 as isize + offy),
					)
				} else if *fy == 0 {
					new.id_in_range_exclude_a1(
						assignment1,
						(assignment1.0 as isize - offx, assignment1.1 as isize),
					)
				} else {
					new.id_in_range_exclude_a1(
						assignment1,
						(assignment1.0 as isize - offx, assignment1.1 as isize - offy),
					)
				}
			};
			// Improvement: Look across all candidates and find the one that minimizes force/energy for this node.
			if candidates.0.is_empty() && candidates.1.is_empty() {
				continue;
			}

			let mut min_cand = usize::MAX;
			let mut min_pos = (usize::MAX, usize::MAX);
			let mut min_sum = isize::MAX;
			for cand in &candidates.0 {
				let e_before = (new.energy(*idx1, connections_per_node) / 2
					+ new.energy(*cand, connections_per_node)) as isize
					/ 2;
				let e_after =
					(new.energy_if_mov(*idx1, new.assignment(*cand), connections_per_node) / 2
						+ new.energy_if_mov(*cand, new.assignment(*idx1), connections_per_node))
						as isize / 2;
				if e_after - e_before < min_sum {
					min_cand = *cand;
					min_sum = e_after - e_before;
				}
			}

			for cand in &candidates.1 {
				let e_diff = new.energy_if_mov(*idx1, *cand, connections_per_node) as isize
					- new.energy(*idx1, connections_per_node) as isize;
				if e_diff <= min_sum {
					min_pos = *cand;
					min_cand = usize::MAX;
					min_sum = e_diff;
				}
			}

			if min_cand == usize::MAX && min_pos.0 == usize::MAX {
				continue;
			}

			if min_cand != usize::MAX && min_cand != *idx1 {
				if new.density(new.assignment(min_cand)) < (max_density - 1).max(1) {
					new.mov(*idx1, new.assignment(min_cand));
				} else {
					new.swap(*idx1, min_cand);
				}
				continue;
			}

			if min_pos.0 != usize::MAX {
				new.mov(*idx1, min_pos);
			}
		}
	}
}

pub(crate) fn slide_puzzle_method_on_violations(
	rng: &mut StdRng,
	_curr: &Placement,
	new: &mut Placement,
	connections_per_node: &Vec<Vec<usize>>,
	_side_length: usize,
	max_density: i32,
) {
	let iters = 1;
	for _ in 0..iters {
		let mut interesting_cells = BTreeSet::new();
		for (i, js) in connections_per_node.iter().enumerate() {
			for j in js {
				if *j < i {
					continue;
				}
				let (x_i, y_i) = new.assignment(i);
				let (x_j, y_j) = new.assignment(*j);
				let dx = (x_i as isize - x_j as isize).abs() as f64;
				let dy = (y_i as isize - y_j as isize).abs() as f64;
				let r2distance = dx.powi(2) + dy.powi(2);
				if r2distance > (64.0 + 81.0) / 2.0 {
					interesting_cells.insert(i);
					interesting_cells.insert(*j);
				}
			}
		}
		for picked_cell in interesting_cells {
			let want_assignment = connections_per_node[picked_cell]
				.iter()
				.map(|idx2| {
					let (x1, y1) = new.assignment(*idx2);
					(x1, y1)
				})
				.reduce(|(x0, y0), (x1, y1)| (x0 + x1, y0 + y1))
				.map(|(x, y)| {
					(
						(x / connections_per_node[picked_cell].len()) / 2 * 2,
						y / connections_per_node[picked_cell].len(),
					)
				});
			let want_assignment = if let Some(assignment) = want_assignment {
				assignment
			} else {
				continue;
			};

			if want_assignment == new.assignment(picked_cell) {
				continue;
			}
			for _ in 0..8 {
				new.slide(
					want_assignment,
					max_density,
					rng.random_bool(0.5),
					rng.random_bool(0.5),
				);
				if new.density(want_assignment) < max_density {
					new.mov(picked_cell, want_assignment);
				}
			}
		}
	}
}

pub(crate) fn swap_random_energy_method(
	rng: &mut StdRng,
	_curr: &Placement,
	new: &mut Placement,
	connections_per_node: &Vec<Vec<usize>>,
	_side_length: usize,
	_max_density: i32,
) {
	let num_cells = new.num_cells();
	let swap_count = exponential_distr_sample(rng.random(), 0.05, num_cells) / 2 * 2;
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
		let e_before = new.energy(c1, connections_per_node) as f64
			+ new.energy(c2, connections_per_node) as f64;
		new.swap(c1, c2);
		let e_after = new.energy(c1, connections_per_node) as f64
			+ new.energy(c2, connections_per_node) as f64;
		if e_after > e_before {
			let acceptance = f64::exp(-(e_after - e_before) / e_before).min(1.0);
			if !rng.random_bool(acceptance) {
				new.swap(c1, c2);
			}
		}
	}
}

pub(crate) fn swap_local_energy_method(
	rng: &mut StdRng,
	_curr: &Placement,
	new: &mut Placement,
	connections_per_node: &Vec<Vec<usize>>,
	side_length: usize,
	_max_density: i32,
) {
	let num_cells = new.num_cells();
	let n_swap = exponential_distr_sample(rng.random(), 0.02, num_cells);
	let mut swap_cells = vec![];
	while swap_cells.len() != n_swap {
		let cell = rng.random_range(0..num_cells);
		if swap_cells.contains(&cell) {
			continue;
		}
		swap_cells.push(cell);
	}

	for c1 in swap_cells {
		let cxcy = new.assignment(c1);
		loop {
			let pick_x: i32 = rng.random_range(-1..=1) * 2;
			let pick_y: i32 = rng.random_range(-1..=1);
			let want_x = cxcy.0 as isize + pick_x as isize;
			let want_y = cxcy.1 as isize + pick_y as isize;
			if pick_x == 0 && pick_y == 0
				|| want_x < 0
				|| want_x >= side_length as isize
				|| want_y < 0
				|| want_y >= side_length as isize
			{
				continue;
			}
			let want_assignment = (want_x as usize, want_y as usize);
			let candidates = new.id_at(want_assignment).iter().collect_vec();
			if candidates.is_empty() {
				let e_before = new.energy(c1, connections_per_node);
				new.mov(c1, want_assignment);
				let e_after = new.energy(c1, connections_per_node);
				if e_after > e_before {
					new.mov(c1, cxcy);
				}
			} else {
				let c2 = *candidates[rng.random_range(0..candidates.len())];
				let e_before = new.energy(c1, connections_per_node) as f64
					+ new.energy(c2, connections_per_node) as f64;
				new.swap(c1, c2);
				let e_after = new.energy(c1, connections_per_node) as f64
					+ new.energy(c2, connections_per_node) as f64;
				if e_after > e_before {
					let acceptance = f64::exp(-(e_after - e_before) / e_before).min(1.0);
					if !rng.random_bool(acceptance) {
						new.swap(c1, c2);
					}
				}
			}
			break;
		}
	}
}

pub(crate) fn random_01<T: Integer>(rng: &mut StdRng, p: f64) -> T {
	if rng.random_bool(p) {
		Integer::one()
	} else {
		Integer::zero()
	}
}

pub(crate) fn exponential_distr_sample(u: f64, median: f64, num_cells: usize) -> usize {
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

type Num = usize;

#[derive(Clone, Debug)]
pub(crate) struct Placement {
	pub assignments: Vec<(Num, Num)>,
	pub block_counts: Arr2<Num>,
	pub pos_map: Arr2<HashSet<Num, BuildHasherDefault<FNV1aHasher64>>>,
	pub side_length: Num,
}

impl Placement {
	pub(crate) fn from_initialization(assignments: Vec<(Num, Num)>, side_length: Num) -> Self {
		let mut ret = Self {
			assignments,
			block_counts: Arr2::new([side_length, side_length]),
			pos_map: Arr2::new([side_length, side_length]),
			side_length,
		};
		for (id, (x, y)) in ret.assignments.iter().enumerate() {
			ret.block_counts[*x][*y] += 1;
			ret.pos_map[*x][*y].insert(id);
		}
		ret
	}

	pub(crate) fn ripup(&mut self, id: Num) {
		let (x, y) = self.assignment(id);
		self.block_counts[x][y] -= 1;
		self.pos_map[x][y].remove(&id);
	}

	pub(crate) fn ripup_loc(
		&mut self,
		assignment: (Num, Num),
	) -> HashSet<Num, BuildHasherDefault<FNV1aHasher64>> {
		let mut to_ripup = util::hash_set();
		swap(&mut to_ripup, &mut self.pos_map[assignment]);
		self.block_counts[assignment] -= to_ripup.len();
		to_ripup
	}

	pub(crate) fn place(&mut self, id: Num, assignment: (Num, Num)) {
		assert!(assignment.0 % 2 == 0, "Found odd x");
		assert!(assignment.0 < self.side_length, "Found OOB x");
		assert!(assignment.1 < self.side_length, "Found OOB y");
		self.assignments[id] = assignment;
		self.block_counts[assignment] += 1;
		self.pos_map[assignment].insert(id);
	}

	pub(crate) fn mov(&mut self, id: Num, assignment: (Num, Num)) {
		self.ripup(id);
		self.place(id, assignment);
	}

	pub(crate) fn swap(&mut self, id_a: Num, id_b: Num) {
		let assignment_a = self.assignment(id_a);
		let assignment_b = self.assignment(id_b);
		self.ripup(id_a);
		self.ripup(id_b);
		self.place(id_a, assignment_b);
		self.place(id_b, assignment_a);
	}

	pub(crate) fn swap_loc(&mut self, assignment_a: (Num, Num), assignment_b: (Num, Num)) {
		let mut tmp = util::hash_set();
		swap(&mut self.pos_map[assignment_a], &mut tmp);
		swap(&mut self.pos_map[assignment_b], &mut tmp);
		swap(&mut self.pos_map[assignment_a], &mut tmp);
		let tmp = self.block_counts[assignment_a];
		self.block_counts[assignment_a] = self.block_counts[assignment_b];
		self.block_counts[assignment_b] = tmp;
		for id in &self.pos_map[assignment_a] {
			self.assignments[*id] = assignment_a;
		}
		for id in &self.pos_map[assignment_b] {
			self.assignments[*id] = assignment_b;
		}
	}

	pub(crate) fn slide(
		&mut self,
		assignment: (Num, Num),
		max_density: i32,
		negative_direction: bool,
		x_direction: bool,
	) {
		let selection = assignment;
		let side_length = self.side_length;
		let num_cells = self.assignments.len();
		let (offset, corner1, corner2, sorter) = match (negative_direction, x_direction) {
			(true, true) => ((-2, 0), (0, selection.1), (selection.0, selection.1), 1),
			(true, false) => ((0, -1), (selection.0, 0), (selection.0, selection.1), 2),
			(false, true) => (
				(2, 0),
				(selection.0, selection.1),
				(side_length, selection.1),
				3,
			),
			(false, false) => (
				(0, 1),
				(selection.0, selection.1),
				(selection.0, side_length),
				4,
			),
		};

		let mut ripup_cells = Vec::with_capacity(num_cells);
		for x in (0..side_length).step_by(2) {
			for y in 0..side_length {
				if x < corner1.0 || x > corner2.0 || y < corner1.1 || y > corner2.1 {
					continue;
				}
				ripup_cells.extend(self.ripup_loc((x, y)));
			}
		}

		match sorter {
			1 => ripup_cells
				.sort_by(|a: &usize, b: &usize| self.assignment(*a).0.cmp(&self.assignment(*b).0)),
			2 => ripup_cells
				.sort_by(|a: &usize, b: &usize| self.assignment(*a).1.cmp(&self.assignment(*b).1)),
			3 => ripup_cells
				.sort_by(|a: &usize, b: &usize| self.assignment(*b).0.cmp(&self.assignment(*a).0)),
			4 => ripup_cells
				.sort_by(|a: &usize, b: &usize| self.assignment(*b).1.cmp(&self.assignment(*a).1)),
			_ => {
				unreachable!()
			},
		};

		for cell in ripup_cells {
			let assignment = self.assignment(cell);
			let want_assignment = (
				(assignment.0 as isize + offset.0) as usize,
				(assignment.1 as isize + offset.1) as usize,
			);
			if want_assignment.0 < side_length
				&& want_assignment.1 < side_length
				&& self.density(want_assignment) < max_density
			{
				self.place(cell, want_assignment);
			} else {
				self.place(cell, assignment);
			}
		}
	}

	pub(crate) fn density(&self, assignment: (Num, Num)) -> i32 {
		self.block_counts[assignment] as i32
	}

	pub(crate) fn assignment(&self, id: Num) -> (Num, Num) {
		self.assignments[id]
	}

	pub fn num_cells(&self) -> Num {
		self.assignments.len()
	}

	pub(crate) fn id_in_range_exclude_a1(
		&self,
		assignment1: (Num, Num),
		assignment2: (isize, isize),
	) -> (
		HashSet<Num, BuildHasherDefault<FNV1aHasher64>>,
		HashSet<(Num, Num), BuildHasherDefault<FNV1aHasher64>>,
	) {
		let assignment2 = (
			assignment2
				.0
				.min((self.side_length as isize - 1) * 2 / 2)
				.max(0) as usize,
			assignment2.1.min(self.side_length as isize - 1).max(0) as usize,
		);
		let min_x = assignment1.0.min(assignment2.0);
		let min_y = assignment1.1.min(assignment2.1);
		let max_x = assignment1.0.max(assignment2.0);
		let max_y = assignment1.1.max(assignment2.1);
		let mut ret = util::hash_set();
		let mut ret2 = util::hash_set();
		for x in (min_x..=max_x).step_by(2) {
			for y in min_y..=max_y {
				if x >= self.side_length || y >= self.side_length {
					continue;
				}
				let block = self.id_at((x, y));
				if block.is_empty() {
					ret2.insert((x, y));
				} else {
					ret.extend(block);
				}
			}
		}
		(ret, ret2)
	}

	pub(crate) fn id_at(
		&self,
		assignment: (Num, Num),
	) -> &HashSet<Num, BuildHasherDefault<FNV1aHasher64>> {
		&self.pos_map[assignment]
	}

	pub(crate) fn energy(&self, id: Num, connections_per_node: &Vec<Vec<usize>>) -> Num {
		let mut retval = 0;
		let (x1, y1) = self.assignment(id);
		for neighbor in &connections_per_node[id] {
			let (x2, y2) = self.assignment(*neighbor);
			let dx = x1.abs_diff(x2);
			let dy = y1.abs_diff(y2);
			retval += dx * dx + dy * dy;
		}
		retval
	}

	pub(crate) fn energy_if_mov(
		&self,
		id: Num,
		assignment: (Num, Num),
		connections_per_node: &Vec<Vec<usize>>,
	) -> Num {
		let mut retval = 0;
		let (x1, y1) = assignment;
		for neighbor in &connections_per_node[id] {
			let (x2, y2) = self.assignment(*neighbor);
			let dx = x1.abs_diff(x2);
			let dy = y1.abs_diff(y2);
			retval += dx * dx + dy * dy;
		}
		retval
	}

	pub(crate) fn compute_cost(
		&self,
		connections: &Vec<(usize, usize)>,
		max_density: i32,
	) -> (f64, bool, i32) {
		let mut cost = 0.0;
		let mut sat_count = 0;
		let mut sat = true;
		for &(i, j) in connections {
			let (x_i, y_i) = self.assignment(i);
			let (x_j, y_j) = self.assignment(j);
			let dx = (x_i as isize - x_j as isize).abs() as f64;
			let dy = (y_i as isize - y_j as isize).abs() as f64;
			let r2distance = dx.powi(2) + dy.powi(2);
			if r2distance > (64.0 + 81.0) / 2.0 {
				cost += r2distance.sqrt();
				sat_count += 1;
				sat = false;
			} else if dx > 2.0 || dy > 1.0 || dx == 2.0 && dy == 1.0 {
				cost += r2distance.sqrt() / 10.0;
			}
		}
		for cell in 0..self.num_cells() {
			let count = self.density(self.assignment(cell));
			if count > 1 {
				sat = false;
				cost += count as f64 * 10.0;
				sat_count += count - max_density;
			}
			// Oh god I need a better way to deliver power
			/*if (self.assignment(cell).0 + 10).rem(18) == 0
				&& ((self.assignment(cell).1 + 10).rem(18) == 0
					|| (self.assignment(cell).1 + 10).rem(18) == 1)
			{
				sat = false;
				cost += 10.0;
			}*/
		}
		(cost, sat, sat_count.max(0))
	}

	pub(crate) fn draw_placement(
		&self,
		connections: &Vec<(usize, usize)>,
		_max_density: i32,
		filename: &str,
	) -> Result<(), std::io::Error> {
		let mut svg = SVG::new();
		let grid = SVG::new_grid(50, 25, 3);
		for x in 0..self.side_length {
			for y in 0..self.side_length {
				let density = self.density((x, y));
				if density > 0 {
					let r = 100
						+ if density == 1 {
							0
						} else {
							(density - 1) * 155 / 4
						};

					svg.add_fill_cell(
						grid,
						x as i32 / 2,
						y as i32,
						(r as u8, 100, 100),
						None,
						Some(format!("{density}")),
					);
				}
			}
		}
		for &(i, j) in connections {
			let (x1, y1) = self.assignment(i);
			let (x2, y2) = self.assignment(j);
			svg.add_line_cell(
				grid,
				x1 as i32 / 2,
				y1 as i32,
				x2 as i32 / 2,
				y2 as i32,
				None,
				None,
			);
		}
		svg.save(filename)
	}
}

pub mod global {
	use std::{collections::HashSet, hash::BuildHasherDefault, mem::swap};

	use hashers::fnv::FNV1aHasher64;
	use itertools::Itertools;
	use rand::{rngs::StdRng, Rng};

	use crate::{ndarr::Arr2, svg::SVG, util};

	use super::{exponential_distr_sample, random_01, Num};

	#[derive(Clone, Debug)]
	pub(crate) struct GlobalPlacement {
		pub assignments: Vec<(Num, Num)>,
		pub block_counts: Arr2<Num>,
		pub pos_map: Arr2<HashSet<Num, BuildHasherDefault<FNV1aHasher64>>>,
		pub side_length: Num,
		pub cell_size: Num,
	}

	impl GlobalPlacement {
		pub(crate) fn from_initialization(
			assignments: Vec<(Num, Num)>,
			side_length: Num,
			cell_size: Num,
		) -> Self {
			let mut ret = Self {
				assignments,
				block_counts: Arr2::new([side_length, side_length]),
				pos_map: Arr2::new([side_length, side_length]),
				side_length,
				cell_size,
			};
			for (id, (x, y)) in ret.assignments.iter().enumerate() {
				ret.block_counts[*x][*y] += 1;
				ret.pos_map[*x][*y].insert(id);
			}
			ret
		}

		pub(crate) fn ripup(&mut self, id: Num) {
			let (x, y) = self.assignment(id);
			self.block_counts[x][y] -= 1;
			self.pos_map[x][y].remove(&id);
		}

		pub(crate) fn ripup_loc(
			&mut self,
			assignment: (Num, Num),
		) -> HashSet<Num, BuildHasherDefault<FNV1aHasher64>> {
			let mut to_ripup = util::hash_set();
			swap(&mut to_ripup, &mut self.pos_map[assignment]);
			self.block_counts[assignment] -= to_ripup.len();
			to_ripup
		}

		pub(crate) fn place(&mut self, id: Num, assignment: (Num, Num)) {
			assert!(assignment.0 < self.side_length, "Found OOB x");
			assert!(assignment.1 < self.side_length, "Found OOB y");
			self.assignments[id] = assignment;
			self.block_counts[assignment] += 1;
			self.pos_map[assignment].insert(id);
		}

		pub(crate) fn mov(&mut self, id: Num, assignment: (Num, Num)) {
			self.ripup(id);
			self.place(id, assignment);
		}

		pub(crate) fn swap(&mut self, id_a: Num, id_b: Num) {
			let assignment_a = self.assignment(id_a);
			let assignment_b = self.assignment(id_b);
			self.ripup(id_a);
			self.ripup(id_b);
			self.place(id_a, assignment_b);
			self.place(id_b, assignment_a);
		}

		pub(crate) fn swap_loc(&mut self, assignment_a: (Num, Num), assignment_b: (Num, Num)) {
			let mut tmp = util::hash_set();
			swap(&mut self.pos_map[assignment_a], &mut tmp);
			swap(&mut self.pos_map[assignment_b], &mut tmp);
			swap(&mut self.pos_map[assignment_a], &mut tmp);
			let tmp = self.block_counts[assignment_a];
			self.block_counts[assignment_a] = self.block_counts[assignment_b];
			self.block_counts[assignment_b] = tmp;
			for id in &self.pos_map[assignment_a] {
				self.assignments[*id] = assignment_a;
			}
			for id in &self.pos_map[assignment_b] {
				self.assignments[*id] = assignment_b;
			}
		}

		pub(crate) fn slide(
			&mut self,
			assignment: (Num, Num),
			max_density: i32,
			negative_direction: bool,
			x_direction: bool,
		) {
			let selection = assignment;
			let side_length = self.side_length;
			let num_cells = self.assignments.len();
			let (offset, corner1, corner2, sorter) = match (negative_direction, x_direction) {
				(true, true) => ((-1, 0), (0, selection.1), (selection.0, selection.1), 1),
				(true, false) => ((0, -1), (selection.0, 0), (selection.0, selection.1), 2),
				(false, true) => (
					(1, 0),
					(selection.0, selection.1),
					(side_length, selection.1),
					3,
				),
				(false, false) => (
					(0, 1),
					(selection.0, selection.1),
					(selection.0, side_length),
					4,
				),
			};

			let mut ripup_cells = Vec::with_capacity(num_cells);
			for x in 0..side_length {
				for y in 0..side_length {
					if x < corner1.0 || x > corner2.0 || y < corner1.1 || y > corner2.1 {
						continue;
					}
					ripup_cells.extend(self.ripup_loc((x, y)));
				}
			}

			match sorter {
				1 => ripup_cells.sort_by(|a: &usize, b: &usize| {
					self.assignment(*a).0.cmp(&self.assignment(*b).0)
				}),
				2 => ripup_cells.sort_by(|a: &usize, b: &usize| {
					self.assignment(*a).1.cmp(&self.assignment(*b).1)
				}),
				3 => ripup_cells.sort_by(|a: &usize, b: &usize| {
					self.assignment(*b).0.cmp(&self.assignment(*a).0)
				}),
				4 => ripup_cells.sort_by(|a: &usize, b: &usize| {
					self.assignment(*b).1.cmp(&self.assignment(*a).1)
				}),
				_ => {
					unreachable!()
				},
			};

			for cell in ripup_cells {
				let assignment = self.assignment(cell);
				let want_assignment = (
					(assignment.0 as isize + offset.0) as usize,
					(assignment.1 as isize + offset.1) as usize,
				);
				if want_assignment.0 < side_length
					&& want_assignment.1 < side_length
					&& self.density(want_assignment) < max_density
				{
					self.place(cell, want_assignment);
				} else {
					self.place(cell, assignment);
				}
			}
		}

		pub(crate) fn density(&self, assignment: (Num, Num)) -> i32 {
			self.block_counts[assignment] as i32
		}

		pub(crate) fn assignment(&self, id: Num) -> (Num, Num) {
			self.assignments[id]
		}

		pub fn num_cells(&self) -> Num {
			self.assignments.len()
		}

		pub(crate) fn id_in_range_exclude_a1(
			&self,
			assignment1: (Num, Num),
			assignment2: (isize, isize),
		) -> (
			HashSet<Num, BuildHasherDefault<FNV1aHasher64>>,
			HashSet<(Num, Num), BuildHasherDefault<FNV1aHasher64>>,
		) {
			let assignment2 = (
				assignment2.0.min(self.side_length as isize - 1).max(0) as usize,
				assignment2.1.min(self.side_length as isize - 1).max(0) as usize,
			);
			let min_x = assignment1.0.min(assignment2.0);
			let min_y = assignment1.1.min(assignment2.1);
			let max_x = assignment1.0.max(assignment2.0);
			let max_y = assignment1.1.max(assignment2.1);
			let mut ret = util::hash_set();
			let mut ret2 = util::hash_set();
			for x in min_x..=max_x {
				for y in min_y..=max_y {
					if x >= self.side_length || y >= self.side_length {
						continue;
					}
					let block = self.id_at((x, y));
					if block.is_empty() {
						ret2.insert((x, y));
					} else {
						ret.extend(block);
					}
				}
			}
			(ret, ret2)
		}

		pub(crate) fn id_at(
			&self,
			assignment: (Num, Num),
		) -> &HashSet<Num, BuildHasherDefault<FNV1aHasher64>> {
			&self.pos_map[assignment]
		}

		pub(crate) fn energy(
			&self,
			id: Num,
			connections_per_node: &Vec<Vec<(usize, usize)>>,
		) -> Num {
			let mut retval = 0;
			let (x1, y1) = self.assignment(id);
			for neighbor in &connections_per_node[id] {
				let (x2, y2) = self.assignment(neighbor.0);
				let dx = x1.abs_diff(x2);
				let dy = y1.abs_diff(y2);
				retval += (dx * dx + dy * dy) * neighbor.1;
			}
			retval
		}

		pub(crate) fn energy_if_mov(
			&self,
			id: Num,
			assignment: (Num, Num),
			connections_per_node: &Vec<Vec<(usize, usize)>>,
		) -> Num {
			let mut retval = 0;
			let (x1, y1) = assignment;
			for neighbor in &connections_per_node[id] {
				let (x2, y2) = self.assignment(neighbor.0);
				let dx = x1.abs_diff(x2);
				let dy = y1.abs_diff(y2);
				retval += (dx * dx + dy * dy) * neighbor.1;
			}
			retval
		}

		pub(crate) fn compute_cost(
			&self,
			connections: &Vec<(usize, usize, usize)>,
			max_density: i32,
		) -> (f64, bool, i32) {
			let mut cost = 0.0;
			let mut sat_count = 0;
			let mut sat = true;
			for &(i, j, weight) in connections {
				let (x_i, y_i) = self.assignment(i);
				let (x_j, y_j) = self.assignment(j);
				let dx = (x_i as isize - x_j as isize).abs() as f64;
				let dy = (y_i as isize - y_j as isize).abs() as f64;
				let r2distance = dx.powi(2) + dy.powi(2);
				//if dx > 1.0 || dy > 1.0 || dx == 1.0 && dy == 1.0 {
				cost += weight as f64 * r2distance.sqrt();
				//}
			}
			for cell in 0..self.num_cells() {
				let count = self.density(self.assignment(cell));
				if count > 1 {
					sat = false;
					cost += count as f64 * 10.0;
					sat_count += count - max_density;
				}
			}
			(cost, sat, sat_count.max(0))
		}

		pub(crate) fn draw_placement(
			&self,
			connections: &Vec<(usize, usize)>,
			_max_density: i32,
		) -> SVG {
			let mut svg = SVG::new();
			let grid = SVG::new_grid(50 * self.cell_size as i32, 25 * self.cell_size as i32, 3);
			for x in 0..self.side_length {
				for y in 0..self.side_length {
					let density = self.density((x, y));
					if density > 0 {
						let r = 100
							+ if density == 1 {
								0
							} else {
								(density - 1) * 155 / 4
							};

						svg.add_fill_cell(
							grid,
							x as i32,
							y as i32,
							(r as u8, 100, 100),
							None,
							Some(format!("{density}")),
						);
					}
				}
			}
			for &(i, j) in connections {
				let (x1, y1) = self.assignment(i);
				let (x2, y2) = self.assignment(j);
				svg.add_line_cell(grid, x1 as i32, y1 as i32, x2 as i32, y2 as i32, None, None);
			}
			svg
		}
	}

	pub(crate) fn ripup_replace_method(
		rng: &mut StdRng,
		_curr: &GlobalPlacement,
		new: &mut GlobalPlacement,
		_connections_per_node: &Vec<Vec<(usize, usize)>>,
		side_length: usize,
		max_density: i32,
	) {
		let num_cells = new.num_cells();
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
			new.ripup(*cell);
		}

		for cell in &ripup_cells {
			loop {
				let cxcy = (
					rng.random_range(0..side_length),
					rng.random_range(0..side_length),
				);
				if new.density(cxcy) < max_density {
					new.place(*cell, cxcy);
					break;
				}
			}
		}
	}

	pub(crate) fn swap_local_method(
		rng: &mut StdRng,
		_curr: &GlobalPlacement,
		new: &mut GlobalPlacement,
		_connections_per_node: &Vec<Vec<(usize, usize)>>,
		side_length: usize,
		_max_density: i32,
	) {
		let num_cells = new.num_cells();
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
			let cxcy = new.assignment(*cell);
			loop {
				let pick_x: i32 = rng.random_range(-1..=1);
				let pick_y: i32 = rng.random_range(-1..=1);
				let want_x = cxcy.0 as isize + pick_x as isize;
				let want_y = cxcy.1 as isize + pick_y as isize;
				if pick_x == 0 && pick_y == 0
					|| want_x < 0 || want_x >= side_length as isize
					|| want_y < 0 || want_y >= side_length as isize
				{
					continue;
				}
				let want_assignment = (want_x as usize, want_y as usize);
				let candidates = new.id_at(want_assignment).iter().collect_vec();
				if candidates.is_empty() {
					new.mov(*cell, want_assignment);
				} else {
					let pick_cell = candidates[rng.random_range(0..candidates.len())];
					new.swap(*cell, *pick_cell);
				}
				break;
			}
		}
	}

	pub(crate) fn swap_random_method(
		rng: &mut StdRng,
		_curr: &GlobalPlacement,
		new: &mut GlobalPlacement,
		_connections_per_node: &Vec<Vec<(usize, usize)>>,
		_side_length: usize,
		_max_density: i32,
	) {
		let num_cells = new.num_cells();
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
			new.swap(c1, c2);
		}
	}

	pub(crate) fn ripup_range_method(
		rng: &mut StdRng,
		_curr: &GlobalPlacement,
		new: &mut GlobalPlacement,
		_connections_per_node: &Vec<Vec<(usize, usize)>>,
		side_length: usize,
		max_density: i32,
	) {
		if side_length < 10 {
			return;
		}
		let region_w = rng.random_range(1..=3);
		let region_h = rng.random_range(1..=3);

		let sx = rng.random_range(0..(side_length - region_w).max(1));
		let sy = rng.random_range(0..(side_length - region_h).max(1));

		let mut ripup_cells = vec![];
		for x in sx..sx + region_w {
			for y in sy..sy + region_h {
				ripup_cells.extend(new.ripup_loc((x, y)));
			}
		}

		for cell in &ripup_cells {
			loop {
				let assignment = (
					rng.random_range(0..side_length),
					rng.random_range(0..side_length),
				);
				if new.density(assignment) < max_density {
					new.place(*cell, assignment);
					break;
				}
			}
		}
	}

	pub(crate) fn crack_in_two_method(
		rng: &mut StdRng,
		_curr: &GlobalPlacement,
		new: &mut GlobalPlacement,
		_connections_per_node: &Vec<Vec<(usize, usize)>>,
		side_length: usize,
		max_density: i32,
	) {
		if side_length < 6 {
			return;
		}
		let num_cells = new.num_cells();
		let selection = (
			rng.random_range(1..side_length - 2),
			rng.random_range(1..side_length - 2),
		);
		let (offset, corner1, corner2, sorter) = match (rng.random_bool(0.5), rng.random_bool(0.5))
		{
			(true, true) => ((-1, 0), (0, 0), (selection.0, side_length), 1),
			(true, false) => ((0, -1), (0, 0), (side_length, selection.1), 2),
			(false, true) => ((2, 0), (selection.0, 0), (side_length, side_length), 3),
			(false, false) => (
				(0, 1),
				(side_length, selection.1),
				(side_length, side_length),
				4,
			),
		};

		let mut ripup_cells = Vec::with_capacity(num_cells);
		for x in 0..side_length {
			for y in 0..side_length {
				if x < corner1.0 || x > corner2.0 || y < corner1.1 || y > corner2.1 {
					continue;
				}
				ripup_cells.extend(new.ripup_loc((x, y)));
			}
		}

		match sorter {
			1 => ripup_cells
				.sort_by(|a: &usize, b: &usize| new.assignment(*a).0.cmp(&new.assignment(*b).0)),
			2 => ripup_cells
				.sort_by(|a: &usize, b: &usize| new.assignment(*a).1.cmp(&new.assignment(*b).1)),
			3 => ripup_cells
				.sort_by(|a: &usize, b: &usize| new.assignment(*b).0.cmp(&new.assignment(*a).0)),
			4 => ripup_cells
				.sort_by(|a: &usize, b: &usize| new.assignment(*b).1.cmp(&new.assignment(*a).1)),
			_ => {
				unreachable!()
			},
		};

		for cell in ripup_cells {
			let assignment = new.assignment(cell);
			let want_assignment = (
				(assignment.0 as isize + offset.0) as usize,
				(assignment.1 as isize + offset.1) as usize,
			);
			if want_assignment.0 < side_length
				&& want_assignment.1 < side_length
				&& new.density(want_assignment) < max_density
			{
				new.place(cell, want_assignment);
			} else {
				new.place(cell, assignment);
			}
		}
	}

	pub(crate) fn slide_puzzle_method(
		rng: &mut StdRng,
		_curr: &GlobalPlacement,
		new: &mut GlobalPlacement,
		_connections_per_node: &Vec<Vec<(usize, usize)>>,
		side_length: usize,
		max_density: i32,
	) {
		if side_length < 10 {
			return;
		}
		let num_cells = new.num_cells();
		let selection = (
			rng.random_range(1..side_length - 2),
			rng.random_range(1..side_length - 2),
		);
		let (offset, corner1, corner2, sorter) = match (rng.random_bool(0.5), rng.random_bool(0.5))
		{
			(true, true) => ((-1, 0), (0, selection.1), (selection.0, selection.1), 1),
			(true, false) => ((0, -1), (selection.0, 0), (selection.0, selection.1), 2),
			(false, true) => (
				(1, 0),
				(selection.0, selection.1),
				(side_length, selection.1),
				3,
			),
			(false, false) => (
				(0, 1),
				(selection.0, selection.1),
				(selection.0, side_length),
				4,
			),
		};

		let mut ripup_cells = Vec::with_capacity(num_cells);
		for x in 0..side_length {
			for y in 0..side_length {
				if x < corner1.0 || x > corner2.0 || y < corner1.1 || y > corner2.1 {
					continue;
				}
				ripup_cells.extend(new.ripup_loc((x, y)));
			}
		}

		match sorter {
			1 => ripup_cells
				.sort_by(|a: &usize, b: &usize| new.assignment(*a).0.cmp(&new.assignment(*b).0)),
			2 => ripup_cells
				.sort_by(|a: &usize, b: &usize| new.assignment(*a).1.cmp(&new.assignment(*b).1)),
			3 => ripup_cells
				.sort_by(|a: &usize, b: &usize| new.assignment(*b).0.cmp(&new.assignment(*a).0)),
			4 => ripup_cells
				.sort_by(|a: &usize, b: &usize| new.assignment(*b).1.cmp(&new.assignment(*a).1)),
			_ => {
				unreachable!()
			},
		};

		for cell in ripup_cells {
			let assignment = new.assignment(cell);
			let want_assignment = (
				(assignment.0 as isize + offset.0) as usize,
				(assignment.1 as isize + offset.1) as usize,
			);
			if want_assignment.0 < side_length
				&& want_assignment.1 < side_length
				&& new.density(want_assignment) < max_density
			{
				new.place(cell, want_assignment);
			} else {
				new.place(cell, assignment);
			}
		}
	}

	pub(crate) fn slide_puzzle_method_worst_cells(
		rng: &mut StdRng,
		_curr: &GlobalPlacement,
		new: &mut GlobalPlacement,
		_connections_per_node: &Vec<Vec<(usize, usize)>>,
		_side_length: usize,
		max_density: i32,
	) {
		let mut interesting_cells = vec![];
		let mut interesting_cells_assignment = vec![];
		for (idx, cass) in new.assignments.iter().enumerate() {
			if new.density(*cass) > max_density
				&& interesting_cells_assignment.last() != Some(&cass)
			{
				interesting_cells.push(idx);
				interesting_cells_assignment.push(cass);
			}
		}
		for picked_cell in interesting_cells {
			let cass = new.assignment(picked_cell);
			new.slide(
				cass,
				max_density,
				rng.random_bool(0.5),
				rng.random_bool(0.5),
			);
			if new.density(cass) < max_density {
				new.mov(picked_cell, cass);
			}
		}
	}

	pub(crate) fn overflowing_cells_swap_local_method(
		rng: &mut StdRng,
		_curr: &GlobalPlacement,
		new: &mut GlobalPlacement,
		_connections_per_node: &Vec<Vec<(usize, usize)>>,
		side_length: usize,
		_max_density: i32,
	) {
		let mut swap_cells = vec![];
		for (idx, assignment) in new.assignments.iter().enumerate() {
			if new.density(*assignment) > 1 {
				swap_cells.push(idx);
			}
		}

		for cell in &swap_cells {
			let assignment = new.assignment(*cell);
			loop {
				let pick_x: i32 = rng.random_range(-1..=1);
				let pick_y: i32 = rng.random_range(-1..=1);
				let want_x = assignment.0 as isize + pick_x as isize;
				let want_y = assignment.1 as isize + pick_y as isize;
				let want_assignment = (want_x as usize, want_y as usize);
				if assignment == want_assignment
					|| want_assignment.0 >= side_length
					|| want_assignment.1 >= side_length
				{
					continue;
				}
				let candidates = new.id_at(want_assignment).iter().collect_vec();
				if candidates.is_empty() {
					new.mov(*cell, want_assignment);
				} else {
					let candidate = candidates[rng.random_range(0..candidates.len())];
					new.swap(*candidate, *cell);
				}
				break;
			}
		}
	}

	pub(crate) fn simulated_spring_method(
		rng: &mut StdRng,
		_curr: &GlobalPlacement,
		new: &mut GlobalPlacement,
		connections_per_node: &Vec<Vec<(usize, usize)>>,
		side_length: usize,
		max_density: i32,
	) {
		let num_cells = new.num_cells();
		let iters = rng.random_range(1..100);
		let mut offx = 1;
		let mut offy = 1;
		for _ in 0..0 {
			let i: isize = random_01(rng, 0.05);
			offx += i;
			offy += i;
		}
		for _ in 0..iters {
			let mut force = new
				.assignments
				.iter()
				.enumerate()
				.map(|(idx, (x0, y0))| {
					(
						idx,
						connections_per_node[idx]
							.iter()
							.map(|(idx2, weight)| {
								let (x1, y1) = new.assignment(*idx2);
								let dx = x1 as isize - *x0 as isize;
								let dy = y1 as isize - *y0 as isize;
								(
									dx * dx * dx.signum() * *weight as isize,
									dy * dy * dy.signum() * *weight as isize,
								)
							})
							.reduce(|(x0, y0), (x1, y1)| (x0 + x1, y0 + y1))
							.unwrap_or((0, 0)),
					)
				})
				.collect_vec();
			for (cell1, (x, y)) in new.assignments.iter().enumerate() {
				let offsets = vec![-1, 0, 1]
					.into_iter()
					.cartesian_product(vec![-1, 0, 1])
					.collect_vec();
				for (dx, dy) in offsets {
					if dx == 0 && dy == 0 {
						continue;
					}
					let check_pos = ((*x as isize + dx) as usize, (*y as isize + dy) as usize);
					if check_pos.0 >= side_length || check_pos.1 >= side_length {
						continue;
					}
					for cell2 in new.id_at(check_pos) {
						if connections_per_node[cell1]
							.iter()
							.map(|(a, b)| a)
							.contains(cell2)
						{
							continue;
						}
						let (idx, (fx, fy)) = force[*cell2];
						let dfx = dx.signum() * (1 - dx.abs() + 1) * 10;
						let dfy = dy.signum() * (1 - dy.abs() + 1) * 10;
						force[*cell2] = (idx, (fx + dfx, fy + dfy));
					}
				}
			}
			force.sort_by(|(_, fa), (_, fb)| {
				(fa.0 * fa.0 + fa.1 * fa.1)
					.cmp(&(fb.0 * fb.0 + fb.1 * fb.1))
					.reverse()
			});
			let swap_count = rng.random_range(1..num_cells);
			for (idx1, f1) in force.iter().take(swap_count) {
				let (fx, fy) = f1;
				if *fx == 0 && *fy == 0 {
					continue;
				}
				let assignment1 = new.assignment(*idx1);
				// candidates: (ids: hash_set<usize>, empty_positions: hash_set<(usize, usize)>)
				let candidates = if *fx > 0 {
					if *fy > 0 {
						new.id_in_range_exclude_a1(
							assignment1,
							(assignment1.0 as isize + offx, assignment1.1 as isize + offy),
						)
					} else if *fy == 0 {
						new.id_in_range_exclude_a1(
							assignment1,
							(assignment1.0 as isize + offx, assignment1.1 as isize),
						)
					} else {
						new.id_in_range_exclude_a1(
							assignment1,
							(assignment1.0 as isize + offx, assignment1.1 as isize - offy),
						)
					}
				} else if *fx == 0 {
					if *fy > 0 {
						new.id_in_range_exclude_a1(
							assignment1,
							(assignment1.0 as isize, assignment1.1 as isize + offy),
						)
					} else if *fy == 0 {
						new.id_in_range_exclude_a1(
							assignment1,
							(assignment1.0 as isize, assignment1.1 as isize),
						)
					} else {
						new.id_in_range_exclude_a1(
							assignment1,
							(assignment1.0 as isize, assignment1.1 as isize - offy),
						)
					}
				} else {
					if *fy > 0 {
						new.id_in_range_exclude_a1(
							assignment1,
							(assignment1.0 as isize - offx, assignment1.1 as isize + offy),
						)
					} else if *fy == 0 {
						new.id_in_range_exclude_a1(
							assignment1,
							(assignment1.0 as isize - offx, assignment1.1 as isize),
						)
					} else {
						new.id_in_range_exclude_a1(
							assignment1,
							(assignment1.0 as isize - offx, assignment1.1 as isize - offy),
						)
					}
				};
				// Improvement: Look across all candidates and find the one that minimizes force/energy for this node.
				if candidates.0.is_empty() && candidates.1.is_empty() {
					continue;
				}

				let mut min_cand = usize::MAX;
				let mut min_pos = (usize::MAX, usize::MAX);
				let mut min_sum = isize::MAX;
				for cand in &candidates.0 {
					let e_before = (new.energy(*idx1, connections_per_node) / 2
						+ new.energy(*cand, connections_per_node)) as isize
						/ 2;
					let e_after =
						(new.energy_if_mov(*idx1, new.assignment(*cand), connections_per_node) / 2
							+ new.energy_if_mov(*cand, new.assignment(*idx1), connections_per_node))
							as isize / 2;
					if e_after - e_before < min_sum {
						min_cand = *cand;
						min_sum = e_after - e_before;
					}
				}

				for cand in &candidates.1 {
					let e_diff = new.energy_if_mov(*idx1, *cand, connections_per_node) as isize
						- new.energy(*idx1, connections_per_node) as isize;
					if e_diff <= min_sum {
						min_pos = *cand;
						min_cand = usize::MAX;
						min_sum = e_diff;
					}
				}

				if min_cand == usize::MAX && min_pos.0 == usize::MAX {
					continue;
				}

				if min_cand != usize::MAX && min_cand != *idx1 {
					if new.density(new.assignment(min_cand)) < (max_density - 1).max(1) {
						new.mov(*idx1, new.assignment(min_cand));
					} else {
						new.swap(*idx1, min_cand);
					}
					continue;
				}

				if min_pos.0 != usize::MAX {
					new.mov(*idx1, min_pos);
				}
			}
		}
	}

	pub(crate) fn swap_random_energy_method(
		rng: &mut StdRng,
		_curr: &GlobalPlacement,
		new: &mut GlobalPlacement,
		connections_per_node: &Vec<Vec<(usize, usize)>>,
		_side_length: usize,
		_max_density: i32,
	) {
		let num_cells = new.num_cells();
		let swap_count = exponential_distr_sample(rng.random(), 0.05, num_cells) / 2 * 2;
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
			let e_before = new.energy(c1, connections_per_node) as f64
				+ new.energy(c2, connections_per_node) as f64;
			new.swap(c1, c2);
			let e_after = new.energy(c1, connections_per_node) as f64
				+ new.energy(c2, connections_per_node) as f64;
			if e_after > e_before {
				let acceptance = f64::exp(-(e_after - e_before) / e_before).min(1.0);
				if !rng.random_bool(acceptance) {
					new.swap(c1, c2);
				}
			}
		}
	}

	pub(crate) fn swap_local_energy_method(
		rng: &mut StdRng,
		_curr: &GlobalPlacement,
		new: &mut GlobalPlacement,
		connections_per_node: &Vec<Vec<(usize, usize)>>,
		side_length: usize,
		_max_density: i32,
	) {
		let num_cells = new.num_cells();
		let n_swap = exponential_distr_sample(rng.random(), 0.02, num_cells);
		let mut swap_cells = vec![];
		while swap_cells.len() != n_swap {
			let cell = rng.random_range(0..num_cells);
			if swap_cells.contains(&cell) {
				continue;
			}
			swap_cells.push(cell);
		}

		for c1 in swap_cells {
			let cxcy = new.assignment(c1);
			loop {
				let pick_x: i32 = rng.random_range(-1..=1);
				let pick_y: i32 = rng.random_range(-1..=1);
				let want_x = cxcy.0 as isize + pick_x as isize;
				let want_y = cxcy.1 as isize + pick_y as isize;
				if pick_x == 0 && pick_y == 0
					|| want_x < 0 || want_x >= side_length as isize
					|| want_y < 0 || want_y >= side_length as isize
				{
					continue;
				}
				let want_assignment = (want_x as usize, want_y as usize);
				let candidates = new.id_at(want_assignment).iter().collect_vec();
				if candidates.is_empty() {
					let e_before = new.energy(c1, connections_per_node);
					new.mov(c1, want_assignment);
					let e_after = new.energy(c1, connections_per_node);
					if e_after > e_before {
						new.mov(c1, cxcy);
					}
				} else {
					let c2 = *candidates[rng.random_range(0..candidates.len())];
					let e_before = new.energy(c1, connections_per_node) as f64
						+ new.energy(c2, connections_per_node) as f64;
					new.swap(c1, c2);
					let e_after = new.energy(c1, connections_per_node) as f64
						+ new.energy(c2, connections_per_node) as f64;
					if e_after > e_before {
						let acceptance = f64::exp(-(e_after - e_before) / e_before).min(1.0);
						if !rng.random_bool(acceptance) {
							new.swap(c1, c2);
						}
					}
				}
				break;
			}
		}
	}
}

#[cfg(test)]
mod test {
	use super::Placement;

	#[test]
	fn new() {
		let a = (0, 0);
		let b = (2, 2);
		let c = (4, 4);
		let plc = Placement::from_initialization(vec![a, b, c], 10);
		assert_eq!(plc.assignment(0), a);
		assert_eq!(plc.assignment(1), b);
		assert_eq!(plc.assignment(2), c);
		assert_eq!(plc.num_cells(), 3);
		assert_eq!(plc.side_length, 10);
		assert_eq!(plc.density(a), 1);
		assert_eq!(plc.density(b), 1);
		assert_eq!(plc.density(c), 1);
	}

	#[test]
	fn ops() {
		let a = (0, 0);
		let b = (2, 2);
		let c = (4, 4);
		let d = (6, 4);
		let mut plc = Placement::from_initialization(vec![a, b, c], 10);
		plc.swap(0, 1);
		assert_eq!(plc.assignment(1), a);
		assert_eq!(plc.assignment(0), b);
		assert_eq!(plc.assignment(2), c);

		plc.mov(0, c);
		assert_eq!(plc.density(c), 2);
		plc.slide(c, 10, false, true);
		assert_eq!(plc.density(d), 2);
	}
}

use std::collections::{BTreeSet, HashMap};

use itertools::Itertools;
use rand::{rngs::StdRng, Rng};

pub(crate) fn ripup_replace_method(
	rng: &mut StdRng,
	assignments: &Vec<(usize, usize)>,
	_block_counts: &Vec<Vec<i32>>,
	new_assignments: &mut Vec<(usize, usize)>,
	new_block_counts: &mut Vec<Vec<i32>>,
	_connections_per_node: &Vec<Vec<usize>>,
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

pub(crate) fn swap_local_method(
	rng: &mut StdRng,
	assignments: &Vec<(usize, usize)>,
	_block_counts: &Vec<Vec<i32>>,
	new_assignments: &mut Vec<(usize, usize)>,
	new_block_counts: &mut Vec<Vec<i32>>,
	_connections_per_node: &Vec<Vec<usize>>,
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

pub(crate) fn swap_random_method(
	rng: &mut StdRng,
	assignments: &Vec<(usize, usize)>,
	_block_counts: &Vec<Vec<i32>>,
	new_assignments: &mut Vec<(usize, usize)>,
	_new_block_counts: &mut Vec<Vec<i32>>,
	_connections_per_node: &Vec<Vec<usize>>,
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

pub(crate) fn ripup_range_method(
	rng: &mut StdRng,
	assignments: &Vec<(usize, usize)>,
	_block_counts: &Vec<Vec<i32>>,
	new_assignments: &mut Vec<(usize, usize)>,
	new_block_counts: &mut Vec<Vec<i32>>,
	_connections_per_node: &Vec<Vec<usize>>,
	side_length: usize,
	max_density: i32,
) {
	let num_cells = assignments.len();
	let region_w = rng.random_range(1..=10);
	let region_h = rng.random_range(1..=10);

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

pub(crate) fn crack_in_two_method(
	rng: &mut StdRng,
	assignments: &Vec<(usize, usize)>,
	_block_counts: &Vec<Vec<i32>>,
	new_assignments: &mut Vec<(usize, usize)>,
	new_block_counts: &mut Vec<Vec<i32>>,
	_connections_per_node: &Vec<Vec<usize>>,
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

pub(crate) fn slide_puzzle_method(
	rng: &mut StdRng,
	assignments: &Vec<(usize, usize)>,
	_block_counts: &Vec<Vec<i32>>,
	new_assignments: &mut Vec<(usize, usize)>,
	new_block_counts: &mut Vec<Vec<i32>>,
	_connections_per_node: &Vec<Vec<usize>>,
	side_length: usize,
	max_density: i32,
) {
	if side_length < 10 {
		return;
	}
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

pub(crate) fn slide_puzzle_method_worst_cells(
	rng: &mut StdRng,
	assignments: &Vec<(usize, usize)>,
	_block_counts: &Vec<Vec<i32>>,
	new_assignments: &mut Vec<(usize, usize)>,
	new_block_counts: &mut Vec<Vec<i32>>,
	_connections_per_node: &Vec<Vec<usize>>,
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

pub(crate) fn overflowing_cells_swap_local_method(
	rng: &mut StdRng,
	_assignments: &Vec<(usize, usize)>,
	_block_counts: &Vec<Vec<i32>>,
	new_assignments: &mut Vec<(usize, usize)>,
	new_block_counts: &mut Vec<Vec<i32>>,
	_connections_per_node: &Vec<Vec<usize>>,
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

pub(crate) fn simulated_spring_method(
	rng: &mut StdRng,
	_assignments: &Vec<(usize, usize)>,
	_block_counts: &Vec<Vec<i32>>,
	new_assignments: &mut Vec<(usize, usize)>,
	new_block_counts: &mut Vec<Vec<i32>>,
	connections_per_node: &Vec<Vec<usize>>,
	side_length: usize,
	max_density: i32,
) {
	let num_cells = new_assignments.len();
	let iters = rng.random_range(1..100);
	for _ in 0..iters {
		let mut force = new_assignments
			.iter()
			.enumerate()
			.map(|(idx, (x0, y0))| {
				(
					idx,
					connections_per_node[idx]
						.iter()
						.map(|idx2| {
							let (x1, y1) = new_assignments[*idx2];
							let dx = x1 as isize - *x0 as isize;
							let dy = y1 as isize - *y0 as isize;
							(dx * dx * dx.signum(), dy * dy * dy.signum())
						})
						.reduce(|(x0, y0), (x1, y1)| (x0 + x1, y0 + y1))
						.unwrap_or((0, 0)),
				)
			})
			.collect_vec();
		force.sort_by(|(_, fa), (_, fb)| {
			(fa.0 * fa.0 + fa.1 * fa.1)
				.cmp(&(fb.0 * fb.0 + fb.1 * fb.1))
				.reverse()
		});
		let mut pos_map: HashMap<(isize, isize), BTreeSet<usize>> = HashMap::new();
		for (idx, cxcy) in new_assignments.iter().enumerate() {
			match pos_map.entry((cxcy.0 as isize, cxcy.1 as isize)) {
				std::collections::hash_map::Entry::Occupied(mut occupied_entry) => {
					occupied_entry.get_mut().insert(idx);
				}
				std::collections::hash_map::Entry::Vacant(vacant_entry) => {
					vacant_entry.insert(BTreeSet::from_iter(vec![idx]));
				}
			};
		}
		let swap_count = rng.random_range(1..num_cells);
		for (idx1, f1) in force.iter().take(swap_count) {
			let (fx, fy) = f1;
			if *fx == 0 && *fy == 0 {
				continue;
			}
			let offset = if 2 * fx.abs() < fy.abs() {
				match *fy > 0 {
					true => (0, 1),
					false => (0, -1),
				}
			} else if 2 * fy.abs() < fx.abs() {
				match *fx > 0 {
					true => (2, 0),
					false => (-2, 0),
				}
			} else {
				match (*fx > 0, *fy > 0) {
					(true, true) => (2, 1),
					(true, false) => (2, -1),
					(false, true) => (-2, 1),
					(false, false) => (-2, -1),
				}
			};
			let (cx, cy) = new_assignments[*idx1];
			let want_assignment = (
				(cx as isize + offset.0) as usize,
				(cy as isize + offset.1) as usize,
			);
			let want_assignmentisize = ((cx as isize + offset.0), (cy as isize + offset.1));
			let mut got_want = false;
			let mut kicked_out = usize::MAX;
			match pos_map.entry(want_assignmentisize) {
				std::collections::hash_map::Entry::Vacant(vacant_entry) => {
					vacant_entry.insert(BTreeSet::new());
				}
				_ => {}
			};
			if want_assignmentisize.0 < 0
				|| want_assignmentisize.0 >= side_length as isize
				|| want_assignmentisize.1 < 0
				|| want_assignmentisize.1 >= side_length as isize
			{
				continue;
			}
			if new_block_counts[want_assignment.0][want_assignment.1] < (max_density - 1).max(1) {
				new_assignments[*idx1] = want_assignment;
				new_block_counts[cx][cy] -= 1;
				new_block_counts[want_assignment.0][want_assignment.1] += 1;
				got_want = true;
			} else {
				match pos_map.get_mut(&want_assignmentisize) {
					Some(idx2s) => {
						let mut min_force = isize::MAX;
						let f_incoming = f1.0 * f1.0 + f1.1 * f1.1;
						for idx2 in idx2s.iter() {
							let (_, f2) = force[*idx2];
							let f_current = f2.0 * f2.0 + f2.1 * f2.1;
							if f_incoming > f_current && f_current < min_force {
								new_assignments[*idx1] = want_assignment;
								got_want = true;
								kicked_out = *idx2;
								min_force = f_current;
							}
						}
						if (got_want) {
							new_assignments[kicked_out] = (cx, cy);
						}
					}
					None => {
						new_assignments[*idx1] = want_assignment;
						new_block_counts[cx][cy] -= 1;
						new_block_counts[want_assignment.0][want_assignment.1] += 1;
						got_want = true;
					}
				}
			}
			if got_want {
				pos_map
					.get_mut(&want_assignmentisize)
					.unwrap()
					.insert(*idx1);
				pos_map
					.get_mut(&(cx as isize, cy as isize))
					.unwrap()
					.remove(&idx1);
				if kicked_out != usize::MAX {
					pos_map
						.get_mut(&want_assignmentisize)
						.unwrap()
						.remove(&kicked_out);
					pos_map
						.get_mut(&(cx as isize, cy as isize))
						.unwrap()
						.insert(kicked_out);
				}
			}
		}
	}
}

pub(crate) fn slide_puzzle_method_on_violations(
	rng: &mut StdRng,
	assignments: &Vec<(usize, usize)>,
	_block_counts: &Vec<Vec<i32>>,
	new_assignments: &mut Vec<(usize, usize)>,
	new_block_counts: &mut Vec<Vec<i32>>,
	connections_per_node: &Vec<Vec<usize>>,
	side_length: usize,
	max_density: i32,
) {
	let num_cells = assignments.len();
	let iters = 1;
	for _ in 0..iters {
		let mut interesting_cells = BTreeSet::new();
		for (i, js) in connections_per_node.iter().enumerate() {
			for j in js {
				if *j < i {
					continue;
				}
				let (x_i, y_i) = new_assignments[i];
				let (x_j, y_j) = new_assignments[*j];
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
					let (x1, y1) = new_assignments[*idx2];
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

			if want_assignment == new_assignments[picked_cell] {
				continue;
			}

			if rng.random_bool(0.33) {
				let line_y = want_assignment.1;
				if rng.random_bool(0.5) {
					let mut ripup_cells = vec![];
					for cell in 0..num_cells {
						let (cx, cy) = new_assignments[cell];
						if cy == 0 {
							continue;
						}
						if cy <= line_y && cx == want_assignment.0 {
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
					let mut ripup_cells = vec![];
					for cell in 0..num_cells {
						let (cx, cy) = new_assignments[cell];
						if cy == side_length - 1 {
							continue;
						}
						if cy >= line_y && cx == want_assignment.0 {
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
			} else {
				let line_x = want_assignment.0;
				if rng.random_bool(0.5) {
					let mut ripup_cells = Vec::new();
					for cell in 0..num_cells {
						let (cx, cy) = new_assignments[cell];
						if cx == 0 {
							continue;
						}
						if cx >= line_x && cy == want_assignment.1 {
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
					let mut ripup_cells = Vec::new();
					for cell in 0..num_cells {
						let (cx, cy) = new_assignments[cell];
						if cx == (side_length - 1) / 2 * 2 {
							continue;
						}
						if cx <= line_x && cy == want_assignment.1 {
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
			let curr_assignment = new_assignments[picked_cell];
			if new_block_counts[want_assignment.0][want_assignment.1] < max_density {
				new_block_counts[want_assignment.0][want_assignment.1] += 1;
				new_block_counts[curr_assignment.0][curr_assignment.1] -= 1;
				new_assignments[picked_cell] = want_assignment;
			}
		}
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

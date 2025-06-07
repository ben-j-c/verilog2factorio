use std::fmt::format;

use metis::option::Opt;
use rand::{rngs::StdRng, Rng, SeedableRng};

use crate::{
	ndarr::Arr2,
	svg::SVG,
	util::{hash_set, HashS},
};

#[derive(Clone, Debug)]
struct BBox {
	x: f32,
	y: f32,
	w: f32,
	h: f32,
}

impl BBox {
	fn new(x: f32, y: f32, w: f32, h: f32) -> Self {
		Self { x, y, w, h }
	}

	/// https://www.desmos.com/calculator/wywzi2mula
	const fn overlap(&self, other: &BBox) -> f32 {
		let dx = {
			let dx1 = (other.x + other.w - self.x).max(0.0).min(self.w);
			let dx2 = (self.x + self.w - other.x).max(0.0).min(other.w);
			dx1.min(dx2)
		};
		let dy = {
			let dy1 = (other.y + other.h - self.y).max(0.0).min(self.h);
			let dy2 = (self.y + self.h - other.y).max(0.0).min(other.h);
			dy1.min(dy2)
		};
		dx * dy
	}

	const fn area(self) -> f32 {
		self.w * self.h
	}

	fn center_direction(&self, other: &BBox) -> ((f32, f32), f32) {
		let c1 = self.center();
		let c2 = other.center();
		let ds = (c2.0 - c1.0, c2.1 - c1.1);
		let mag = (ds.0 * ds.0 + ds.1 * ds.1).sqrt();
		if ds.0 == 0.0 && ds.1 == 0.0 {
			return ((0.0, 0.0), 0.0);
		}
		((ds.0 / mag, ds.1 / mag), mag)
	}

	const fn center(&self) -> (f32, f32) {
		(self.x + self.w / 2.0, self.y + self.h / 2.0)
	}

	const fn xy(&self) -> (f32, f32) {
		(self.x, self.y)
	}

	const fn xy_plus_wh(&self) -> (f32, f32) {
		(self.x + self.w, self.y + self.h)
	}

	fn map_xy<F>(&self, func: F) -> (f32, f32)
	where
		F: Fn(f32) -> f32,
	{
		(func(self.x), func(self.y))
	}

	const fn dir(&self, other: (f32, f32)) -> (f32, f32) {
		(other.0 - self.x, other.1 - self.y)
	}
}

#[derive(Clone, Debug)]
struct Cell {
	bbox: BBox,
	fixed: bool,
	max_distance: f32,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub(crate) struct PlacementId(u32);

#[derive(Debug, Clone)]
pub(crate) struct AnalyticalPlacement {
	cells: Vec<Cell>,
	side_length: f32,
	bucket_size: f32,
	buckets: Arr2<HashS<PlacementId>>,
	rng: StdRng,
}

impl AnalyticalPlacement {
	pub(crate) fn new(side_length: f32) -> Self {
		let dim = (side_length / 2.0).max(1.0) as usize;
		let ret = AnalyticalPlacement {
			cells: vec![],
			side_length,
			bucket_size: 2.0,
			buckets: Arr2::new([dim, dim]),
			rng: StdRng::seed_from_u64(0x1234ABCD),
		};
		ret
	}

	pub(crate) fn num_cells(&self) -> usize {
		self.cells.len()
	}

	pub(crate) fn add_cell(
		&mut self,
		pos: Option<(f32, f32)>,
		(w, h): (f32, f32),
		fixed: bool,
		max_distance: f32,
	) {
		let (x, y) = if let Some(xy) = pos {
			xy
		} else {
			(
				self.rng.random::<f32>() * (self.side_length - w),
				self.rng.random::<f32>() * (self.side_length - h),
			)
		};
		let id = PlacementId(self.cells.len() as u32);
		let cell = Cell {
			bbox: BBox { x, y, w, h },
			fixed,
			max_distance,
		};

		let b0 = self.bucket_idx((cell.bbox.x, cell.bbox.y));
		let b1 = self.bucket_idx((cell.bbox.x + cell.bbox.w, cell.bbox.y + cell.bbox.h));
		for x in b0.0..=b1.0 {
			for y in b0.1..=b1.1 {
				self.buckets[(x as usize, y as usize)].insert(id);
			}
		}
		self.cells.push(cell);
	}

	pub(crate) fn compute_cost(&self, connections: &Vec<(usize, usize)>) -> (f32, bool, i32) {
		let mut cost = 0.0;
		let mut sat = true;
		let mut sat_count = 0;
		for id in 0..self.cells.len() {
			let cell = &self.cells[id];
			if cell.fixed {
				continue;
			}
			let b0 = self.bucket_idx((cell.bbox.x, cell.bbox.y));
			let b1 = self.bucket_idx((cell.bbox.x + cell.bbox.w, cell.bbox.y + cell.bbox.h));
			let mut to_check = hash_set::<PlacementId>();
			for x in b0.0..=b1.0 {
				for y in b0.1..=b1.1 {
					to_check.extend(self.buckets[(x, y)].iter());
				}
			}
			to_check.remove(&PlacementId(id as u32));
			for id2 in to_check {
				let cell2 = &self.cells[id2.0 as usize];
				let overlap = cell.bbox.overlap(&cell2.bbox);
				if overlap > 0.0 {
					sat = false;
					sat_count += 1;
				}
				cost += overlap;
			}
		}
		for &(i, j) in connections {
			let (x_i, y_i) = self.cells[i].bbox.center();
			let (x_j, y_j) = self.cells[j].bbox.center();
			let dx = x_i - x_j;
			let dy = y_i - y_j;
			let r2distance = dx * dx + dy * dy;
			let r2_max = self.cells[i].max_distance.min(self.cells[j].max_distance);
			if r2distance > r2_max * r2_max {
				cost += r2distance.sqrt();
				sat_count += 1;
				sat = false;
			} else if dx.abs() / 2.0 + dy.abs() > 2.0 {
				cost += r2distance.sqrt() / 10.0;
			}
		}
		(cost, sat, sat_count)
	}

	pub(crate) fn calculate_overlap_force(&self) -> Vec<(f32, f32)> {
		let mut ret = vec![(0.0, 0.0); self.cells.len()];
		for id in 0..self.cells.len() {
			let cell = &self.cells[id];
			if cell.fixed {
				continue;
			}
			let b0 = self.bucket_idx((cell.bbox.x, cell.bbox.y));
			let b1 = self.bucket_idx((cell.bbox.x + cell.bbox.w, cell.bbox.y + cell.bbox.h));
			let mut to_check = hash_set::<PlacementId>();
			for x in b0.0..=b1.0 {
				for y in b0.1..=b1.1 {
					to_check.extend(self.buckets[(x, y)].iter());
				}
			}
			to_check.remove(&PlacementId(id as u32));
			for id2 in to_check {
				let cell2 = &self.cells[id2.0 as usize];
				let overlap = cell.bbox.overlap(&cell2.bbox);
				let (ds, _) = cell.bbox.center_direction(&cell2.bbox);
				ret[id].0 -= overlap * ds.0;
				ret[id].1 -= overlap * ds.1;
			}
		}
		ret
	}

	pub(crate) fn calculate_electrostatic_force(&self) -> Vec<(f32, f32)> {
		let mut ret = vec![(0.0, 0.0); self.cells.len()];
		for id1 in 0..self.cells.len() {
			let cell1 = &self.cells[id1];
			if cell1.fixed {
				continue;
			}
			for id2 in 0..self.cells.len() {
				let cell2 = &self.cells[id2];
				if cell2.fixed {
					continue;
				}
				let (ds, mag) = cell1.bbox.center_direction(&cell2.bbox);
				if mag == 0.0 {
					continue;
				}
				ret[id1].0 += ds.0 / (mag * mag);
				ret[id1].1 += ds.1 / (mag * mag);
			}
		}
		ret
	}

	pub(crate) fn calculate_spring_force(&self, connctions: &Vec<Vec<usize>>) -> Vec<(f32, f32)> {
		let mut ret = vec![(0.0, 0.0); self.cells.len()];
		for id in 0..self.cells.len() {
			let cell = &self.cells[id];
			if cell.fixed {
				continue;
			}
			let mut force = (0.0, 0.0);
			for id2 in &connctions[id] {
				let cell2 = &self.cells[*id2];
				let (ds, mag) = cell.bbox.center_direction(&cell2.bbox);
				if mag > 10.0 {
					force.0 += mag * mag * mag * ds.0 / 100.0;
					force.1 += mag * mag * mag * ds.1 / 100.0;
				} else {
					force.0 += mag * ds.0;
					force.1 += mag * ds.1;
				}
			}
			ret[id] = force;
		}
		ret
	}

	pub(crate) fn calculate_legalization_force(&self) -> Vec<(f32, f32)> {
		let mut ret = vec![(0.0, 0.0); self.cells.len()];
		for id in 0..self.cells.len() {
			let cell = &self.cells[id];
			if cell.fixed {
				continue;
			}
			let want = (
				(cell.bbox.x / cell.bbox.w).round() * cell.bbox.w,
				(cell.bbox.y / cell.bbox.h).round() * cell.bbox.h,
			);
			let dir = cell.bbox.dir(want);
			let mag = (dir.0 * dir.0 + dir.1 * dir.1).sqrt();
			if mag == 0.0 {
				continue;
			}
			ret[id] = (dir.0 / mag, dir.1 / mag);
		}
		ret
	}

	pub(crate) fn calculate_buckling_force(&self) -> Vec<(f32, f32)> {
		let mut ret = vec![(0.0, 0.0); self.cells.len()];
		for id in 0..self.cells.len() {
			let cell = &self.cells[id];
			if cell.fixed {
				continue;
			}
			let b0 = self.bucket_idx((cell.bbox.x, cell.bbox.y));
			let b1 = self.bucket_idx((cell.bbox.x + cell.bbox.w, cell.bbox.y + cell.bbox.h));
			let mut to_check = hash_set::<PlacementId>();
			for x in b0.0..=b1.0 {
				for y in b0.1..=b1.1 {
					to_check.extend(self.buckets[(x, y)].iter());
				}
			}
			to_check.remove(&PlacementId(id as u32));
			let mut max_dir = (0.0, 0.0);
			let mut max_force = 0.0;
			for id2 in to_check {
				let cell2 = &self.cells[id2.0 as usize];
				let overlap = cell.bbox.overlap(&cell2.bbox);
				let (ds, _) = cell.bbox.center_direction(&cell2.bbox);
				if overlap > max_force {
					max_dir = ds;
					max_force = overlap;
				}
			}
			ret[id].0 = -max_dir.1 * max_force;
			ret[id].1 = max_dir.0 * max_force;
		}
		ret
	}

	pub(crate) fn calculate_accessibility_force(
		&self,
		connctions: &Vec<Vec<(i32, usize, usize)>>,
	) -> Vec<(f32, f32)> {
		let mut ret = vec![(0.0, 0.0); self.cells.len()];
		let center = BBox::new(0.0, 0.0, self.side_length, self.side_length);
		for id in 0..self.cells.len() {
			let cell = &self.cells[id];
			if cell.fixed {
				continue;
			}
			if connctions[id].is_empty() {
				continue;
			}
			let (ds, mag) = center.center_direction(&cell.bbox);
			if mag == 0.0 {
				continue;
			}
			ret[id].0 = ds.0 / (mag * self.side_length).powi(2);
			ret[id].1 = ds.1 / (mag * self.side_length).powi(2);
		}
		ret
	}

	pub(crate) fn calculate_radial_force(&self) -> Vec<(f32, f32)> {
		let mut ret = vec![(0.0, 0.0); self.cells.len()];
		let mut center = (0.0, 0.0);
		for id in 0..self.cells.len() {
			center.0 += self.cells[id].bbox.x;
			center.1 += self.cells[id].bbox.y;
		}
		center.0 /= self.cells.len() as f32;
		center.1 /= self.cells.len() as f32;
		let center_bbox = BBox::new(center.0, center.1, 0.0, 0.0);
		for id1 in 0..self.cells.len() {
			let cell1 = &self.cells[id1];
			if cell1.fixed {
				continue;
			}
			let (ds, mag) = cell1.bbox.center_direction(&center_bbox);
			if mag == 0.0 {
				continue;
			}
			ret[id1].0 -= ds.0 / (mag + 2.0);
			ret[id1].1 -= ds.1 / (mag + 2.0);
		}
		ret
	}

	pub(crate) fn draw_placement(
		&self,
		connections: &Vec<(usize, usize)>,
		filename: &str,
	) -> Result<(), std::io::Error> {
		let scale = 25.0;
		let mut svg = SVG::new();
		svg.add_rect_ext(
			0,
			0,
			(self.side_length * scale) as i32,
			(self.side_length * scale) as i32,
			(255, 255, 255, 1.0),
			None,
			None,
		);
		for id in 0..self.cells.len() {
			let cell = &self.cells[id];
			svg.add_rect_ext(
				(cell.bbox.x * scale) as i32,
				(cell.bbox.y * scale) as i32,
				(cell.bbox.w * scale) as i32,
				(cell.bbox.h * scale) as i32,
				(100, 0, 0, 0.1),
				Some(format!("{id}")),
				None,
			);
		}
		for &(i, j) in connections {
			let cell_i = &self.cells[i];
			let cell_j = &self.cells[j];
			let (x1, y1) = cell_i.bbox.center();
			let (x2, y2) = cell_j.bbox.center();
			svg.add_line(
				(x1 * scale) as i32,
				(y1 * scale) as i32,
				(x2 * scale) as i32,
				(y2 * scale) as i32,
				None,
				None,
			);
		}
		svg.save(filename)
	}

	pub(crate) fn step_cells(&mut self, force: Vec<(f32, f32)>, factor: f32) {
		for id in 0..self.cells.len() {
			if self.cells[id].fixed {
				continue;
			}
			let mut bbox_new = self.cells[id].bbox.clone();
			let b0 = self.bucket_idx(bbox_new.xy());
			let b1 = self.bucket_idx(bbox_new.xy_plus_wh());
			for x in b0.0..=b1.0 {
				for y in b0.1..=b1.1 {
					self.buckets[(x, y)].remove(&PlacementId(id as u32));
				}
			}
			bbox_new.x += force[id].0 * factor;
			bbox_new.y += force[id].1 * factor;
			bbox_new.x = bbox_new.x.clamp(0.0, self.side_length - bbox_new.w);
			bbox_new.y = bbox_new.y.clamp(0.0, self.side_length - bbox_new.h);
			let b0 = self.bucket_idx(bbox_new.xy());
			let b1 = self.bucket_idx(bbox_new.xy_plus_wh());
			for x in b0.0..=b1.0 {
				for y in b0.1..=b1.1 {
					self.buckets[(x, y)].insert(PlacementId(id as u32));
				}
			}
			self.cells[id].bbox = bbox_new;
		}
	}

	pub(crate) fn legalized(&self) -> Vec<(usize, usize)> {
		let mut ret = vec![(0, 0); self.cells.len()];
		for id in 0..self.cells.len() {
			let cell = &self.cells[id];
			let tmp = cell.bbox.map_xy(f32::round);
			ret[id] = (tmp.0 as usize, tmp.1 as usize);
		}
		ret
	}

	pub(crate) fn apply_legalization(&mut self) {
		for id in 0..self.cells.len() {
			let cell = &mut self.cells[id];
			let tmp = cell.bbox.map_xy(f32::round);
			cell.bbox.x = tmp.0;
			cell.bbox.y = tmp.1;
		}
	}

	pub(crate) fn bucket_idx(&self, (x, y): (f32, f32)) -> (usize, usize) {
		let mut retx = (x / self.bucket_size) as usize;
		let mut rety = (y / self.bucket_size) as usize;
		if x < 0.0 {
			retx = 0;
		}
		if y < 0.0 {
			rety = 0;
		}
		if retx >= self.buckets.dims().0 {
			retx = self.buckets.dims().0 - 1;
		}
		if rety >= self.buckets.dims().1 {
			rety = self.buckets.dims().1 - 1;
		}

		(retx, rety)
	}
}

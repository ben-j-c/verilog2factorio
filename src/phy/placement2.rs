use metis::option::Opt;
use rand::{rngs::StdRng, Rng, SeedableRng};

use crate::{
	ndarr::Arr2,
	util::{hash_set, HashS},
};

#[derive(Clone, Debug)]
struct BBox {
	x: f64,
	y: f64,
	w: f64,
	h: f64,
}

impl BBox {
	fn new(x: f64, y: f64, w: f64, h: f64) -> Self {
		Self { x, y, w, h }
	}

	/// https://www.desmos.com/calculator/wywzi2mula
	const fn overlap(&self, other: &BBox) -> f64 {
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

	const fn area(self) -> f64 {
		self.w * self.h
	}

	fn center_direction(&self, other: &BBox) -> ((f64, f64), f64) {
		let c1 = self.center();
		let c2 = other.center();
		let ds = (c2.0 - c1.0, c2.1 - c1.1);
		let mag = (ds.0 * ds.0 + ds.1 * ds.1).sqrt();
		if ds.0 == 0.0 && ds.1 == 0.0 {
			return ((0.0, 0.0), 0.0);
		}
		((ds.0 / mag, ds.1 / mag), mag)
	}

	const fn center(&self) -> (f64, f64) {
		(self.x + self.w / 2.0, self.y + self.h / 2.0)
	}
}

#[derive(Clone, Debug)]
struct Cell {
	bbox: BBox,
	fixed: bool,
	max_distance: f64,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub(crate) struct PlacementId(u32);

#[derive(Debug)]
pub(crate) struct AnalyticalPlacement {
	cells: Vec<Cell>,
	side_length: f64,
	bucket_size: f64,
	buckets: Arr2<HashS<PlacementId>>,
	rng: StdRng,
}

impl AnalyticalPlacement {
	pub(crate) fn new(side_length: f64) -> Self {
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

	pub(crate) fn add_cell(
		&mut self,
		pos: Option<(f64, f64)>,
		(w, h): (f64, f64),
		fixed: bool,
		max_distance: f64,
	) {
		let (x, y) = if let Some(xy) = pos {
			xy
		} else {
			(self.rng.random::<f64>(), self.rng.random::<f64>())
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

	pub(crate) fn compute_cost(&self) -> (f64, bool) {
		let mut cost = 0.0;
		let mut sat = true;
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
				}
				cost += overlap;
			}
		}
		(cost, sat)
	}

	pub(crate) fn calculate_overlap_force(&self) -> Vec<(f64, f64)> {
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

	pub(crate) fn calcluate_spring_force(&self, connctions: Vec<Vec<usize>>) -> Vec<(f64, f64)> {
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
				force.0 += mag * ds.0;
				force.1 += mag * ds.1;
			}
			ret[id] = force;
		}
		ret
	}

	pub(crate) fn bucket_idx(&self, (x, y): (f64, f64)) -> (usize, usize) {
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

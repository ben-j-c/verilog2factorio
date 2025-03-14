use core::num;

use itertools::Itertools;
use nalgebra::DMatrix;
use nalgebra_lapack::SymmetricEigen;

pub(crate) fn kernighan_lin(
	nodes: &Vec<usize>,
	connectivity: &Vec<Vec<usize>>,
) -> (Vec<usize>, Vec<usize>) {
	let mut a_set: Vec<usize> = Vec::new();
	let mut b_set: Vec<usize> = Vec::new();
	for (i, &node) in nodes.iter().enumerate() {
		if i % 2 == 0 {
			a_set.push(node);
		} else {
			b_set.push(node);
		}
	}

	let mut improvement = true;
	while improvement {
		improvement = false;
		let mut best_gain = 0;
		let mut best_pair: Option<(usize, usize)> = None;
		for &a in &a_set {
			for &b in &b_set {
				let gain = compute_gain(a, b, &a_set, &b_set, connectivity);
				if gain > best_gain {
					best_gain = gain;
					best_pair = Some((a, b));
				}
			}
		}
		if let Some((a, b)) = best_pair {
			if best_gain > 0 {
				a_set.retain(|&x| x != a);
				b_set.retain(|&x| x != b);
				a_set.push(b);
				b_set.push(a);
				improvement = true;
			}
		}
	}
	(a_set, b_set)
}

fn compute_gain(
	a: usize,
	b: usize,
	a_set: &Vec<usize>,
	b_set: &Vec<usize>,
	connectivity: &Vec<Vec<usize>>,
) -> isize {
	let ext_a = b_set
		.iter()
		.filter(|&&node| connectivity[a].contains(&node))
		.count() as isize;
	let int_a = a_set
		.iter()
		.filter(|&&node| node != a && connectivity[a].contains(&node))
		.count() as isize;
	let ext_b = a_set
		.iter()
		.filter(|&&node| connectivity[b].contains(&node))
		.count() as isize;
	let int_b = b_set
		.iter()
		.filter(|&&node| node != b && connectivity[b].contains(&node))
		.count() as isize;
	let connection = if connectivity[a].contains(&b) { 1 } else { 0 };

	(ext_a - int_a) + (ext_b - int_b) - 2 * connection
}

pub(crate) fn spectral(connectivity: &Vec<Vec<usize>>) -> (Vec<bool>, Vec<bool>) {
	let num_nodes = connectivity.len();
	let mut laplacian: DMatrix<f64> = DMatrix::zeros(num_nodes, num_nodes);
	for (i, connections) in connectivity.iter().enumerate() {
		laplacian[(i, i)] = connections.len() as f64;
		for j in connections {
			laplacian[(i, *j)] = -1.0;
		}
	}
	let ev = SymmetricEigen::new(laplacian);
	let values = (0..num_nodes)
		.map(|idx| (idx, ev.eigenvalues[(idx, 0)]))
		.sorted_by(|(_, val1), (_, val2)| val1.total_cmp(val2))
		.collect_vec();
	let mut by_sign = Vec::with_capacity(num_nodes);
	let mut by_median = Vec::with_capacity(num_nodes);
	let idx_pick = values[1].0;
	let ev_sorted = ev
		.eigenvectors
		.column(idx_pick)
		.into_iter()
		.sorted_by(|a, b| a.total_cmp(b))
		.collect_vec();
	let median = if num_nodes % 2 == 0 {
		(ev_sorted[num_nodes / 2] + ev_sorted[num_nodes / 2 + 1]) / 2.0
	} else {
		*ev_sorted[num_nodes / 2]
	};
	for i in 0..num_nodes {
		by_sign.push(ev.eigenvectors[(i, idx_pick)] >= 0.0);
		by_median.push(ev.eigenvectors[(i, idx_pick)] >= median);
	}
	(by_median, by_sign)
}

#[cfg(test)]
mod test {
	use crate::physical_design::PhysicalDesign;

	use super::*;

	#[test]
	fn spectral_simple() {
		let connectivity = vec![
			vec![1, 2],
			vec![0, 2],
			vec![0, 1, 3],
			vec![2, 4, 5],
			vec![3, 5],
			vec![3, 4],
		];
		let (by_median, by_sign) = spectral(&connectivity);
		assert_eq!(by_sign.len(), 6);
		assert_eq!(by_sign[0], by_sign[1]);
		assert_eq!(by_sign[0], by_sign[2]);
		assert_ne!(by_sign[0], by_sign[3]);
		assert_ne!(by_sign[0], by_sign[4]);
		assert_ne!(by_sign[0], by_sign[5]);
		println!("{:?}", by_median);
		println!("{:?}", by_sign);
	}

	#[test]
	fn spectral_n_synthetic() {
		let l = crate::logical_design::get_large_logical_design(4000);
		let mut p = PhysicalDesign::new();
		p.extract_combs(&l);
		let connectvity = p.get_connectivity_as_vec_usize(&l);
		let (by_median, by_sign) = spectral(&connectvity);
		println!(
			"Median: {:?}",
			by_median
				.iter()
				.map(|b| if *b { 1 } else { 0 })
				.collect_vec()
		);
		println!(
			"Sign: {:?}",
			by_sign.iter().map(|b| if *b { 1 } else { 0 }).collect_vec()
		);
	}
}

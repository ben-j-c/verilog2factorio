use itertools::Itertools;
use nalgebra::{DMatrix, SymmetricEigen};
use rand::{rngs::StdRng, seq::IndexedRandom, SeedableRng};

#[allow(dead_code)]
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

#[allow(dead_code)]
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

fn simple_kmeans_balance_partitions(data: &DMatrix<f64>, k: usize) -> Vec<i32> {
	let (n_rows, n_cols) = (data.nrows(), data.ncols());
	let mut rng = StdRng::seed_from_u64(0xB1FFB0FF);

	// Initialize centroids randomly from existing points
	let mut centroids: Vec<Vec<f64>> = (0..n_rows)
		.collect_vec()
		.choose_multiple(&mut rng, k)
		.map(|&idx| data.row(idx).iter().cloned().collect())
		.collect();

	let mut assignments = vec![0; n_rows];

	for _ in 0..20 {
		for i in 0..n_rows {
			let row = data.row(i);
			let best_cluster = centroids
				.iter()
				.enumerate()
				.map(|(idx, c)| {
					let d2: f64 = row.iter().zip(c).map(|(a, b)| (a - b).powi(2)).sum();
					(idx, d2)
				})
				.min_by(|a, b| a.1.total_cmp(&b.1))
				.unwrap();

			assignments[i] = best_cluster.0;
		}

		let mut new_centroids = vec![vec![0.0; n_cols]; k];
		let mut counts = vec![0; k];
		for i in 0..n_rows {
			let c = assignments[i];
			counts[c] += 1;
			for j in 0..n_cols {
				new_centroids[c][j] += data[(i, j)];
			}
		}

		for c in 0..k {
			if counts[c] > 0 {
				for j in 0..n_cols {
					centroids[c][j] = new_centroids[c][j] / counts[c] as f64;
				}
			}
		}
	}

	let mut cluster_usage = vec![0; k];
	let max_usage = n_rows / k * 11 / 10;
	let mut assignments = vec![-1i32; n_rows];

	let min_distance = (0..n_rows)
		.map(|i| {
			let row = data.row(i);
			let best_cluster = centroids
				.iter()
				.enumerate()
				.map(|(idx, c)| {
					let d2: f64 = row.iter().zip(c).map(|(a, b)| (a - b).powi(2)).sum();
					(idx, d2)
				})
				.min_by(|a, b| a.1.total_cmp(&b.1))
				.unwrap();

			(i, best_cluster.1)
		})
		.sorted_by(|a, b| a.1.partial_cmp(&b.1).unwrap())
		.map(|(i, _)| i);

	for i in min_distance {
		let row = data.row(i);
		let best_cluster = centroids
			.iter()
			.enumerate()
			.map(|(idx, c)| {
				if cluster_usage[idx] > max_usage {
					return (idx, f64::MAX);
				}
				let d2: f64 = row.iter().zip(c).map(|(a, b)| (a - b).powi(2)).sum();
				(idx, d2)
			})
			.min_by(|a, b| a.1.total_cmp(&b.1))
			.unwrap();

		assignments[i] = best_cluster.0 as i32;
		cluster_usage[best_cluster.0] += 1;
	}

	assignments
}

pub(crate) fn spectral_k_way(connectivity: &Vec<Vec<usize>>, k: i32) -> (Vec<i32>, i32) {
	let num_nodes = connectivity.len();
	let mut laplacian: DMatrix<f64> = DMatrix::zeros(num_nodes, num_nodes);
	for (i, connections) in connectivity.iter().enumerate() {
		laplacian[(i, i)] = connections.len() as f64;
		for j in connections {
			laplacian[(i, *j)] = -1.0;
		}
	}
	let ev = SymmetricEigen::new(laplacian);
	let sorted_indices = (0..num_nodes)
		.map(|idx| (idx, ev.eigenvalues[idx]))
		.sorted_by(|(_, a), (_, b)| a.total_cmp(b))
		.skip(1)
		.take(k as usize)
		.map(|(idx, _)| idx)
		.collect_vec();

	let mut embedding = DMatrix::zeros(num_nodes, k as usize);
	for (col_idx, &ev_idx) in sorted_indices.iter().enumerate() {
		let column = ev.eigenvectors.column(ev_idx);
		for row_idx in 0..num_nodes {
			embedding[(row_idx, col_idx)] = column[row_idx];
		}
	}

	// 5. Row Normalization (Crucial for k-way clustering)
	for mut row in embedding.row_iter_mut() {
		let norm = row.norm();
		if norm > 1e-9 {
			row /= norm;
		}
	}

	let assignments = simple_kmeans_balance_partitions(&embedding, k as usize);
	let mut n_cuts = 0;
	for i in 0..num_nodes {
		let assignment = assignments[i];
		for j in &connectivity[i] {
			if assignments[*j] != assignment {
				n_cuts += 1;
			}
		}
	}
	(assignments, n_cuts)
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn metis(connectivity: &Vec<Vec<usize>>, n_parts: i32) -> (Vec<i32>, i32) {
	let (adj, idx_adj) = crate::util::convert_connectivity_to_csr(connectivity);
	let graph = metis::Graph::new(1, n_parts, &idx_adj, &adj).unwrap();
	let mut partition = vec![0; connectivity.len()];
	let ret = graph.part_recursive(&mut partition).unwrap();
	(partition, ret)
}

pub(crate) fn report_partition_quality(
	part: &Vec<i32>,
	n_cuts: i32,
	connectivity: &Vec<Vec<usize>>,
	n_parts: i32,
) {
	let n_edges: usize = connectivity.iter().map(|conn| conn.len()).sum();
	println!("Partition quality:");
	println!(
		"Edges cut: {n_cuts} ({:.3}%)",
		n_cuts as f64 / n_edges as f64 * 100.0
	);
	println!("Edge cuts per partition: {})", n_cuts * 2 / n_parts,);
	println!("N edges: {n_edges}");
	println!("N nodes: {}", connectivity.len());
	println!("N partitions: {}", n_parts);
	let mut sizes = vec![0; n_parts as usize];
	for x in part {
		sizes[*x as usize] += 1;
	}
	let min_size = sizes.iter().min().unwrap();
	let max_size = sizes.iter().max().unwrap();
	let mut histogram = vec![0; (max_size - min_size + 1) as usize];
	for x in &sizes {
		histogram[x - min_size] += 1;
	}
	println!("Histogram of partition sizes:");
	for (i, x) in histogram.iter().enumerate() {
		println!("{:4}: {:4}", i + min_size, x);
	}
}

#[cfg(not(target_arch = "wasm32"))]
#[cfg(test)]
mod test {
	use crate::phy::PhysicalDesign;

	use super::*;

	#[test]
	fn csr() {
		let conn = vec![vec![1], vec![0]];
		let (adj, idx_adj) = crate::util::convert_connectivity_to_csr(&conn);
		assert_eq!(adj, vec![1, 0]);
		assert_eq!(idx_adj, vec![0, 1, 2]);
		println!("{:?}, {:?}", adj, idx_adj);
	}

	#[test]
	fn csr2() {
		let conn = vec![
			vec![1, 4],
			vec![0, 2, 4],
			vec![1, 3],
			vec![2, 4, 5],
			vec![0, 1, 3],
			vec![3],
		];
		let (adj, idx_adj) = crate::util::convert_connectivity_to_csr(&conn);
		assert_eq!(adj, vec![1, 4, 0, 2, 4, 1, 3, 2, 4, 5, 0, 1, 3, 3]);
		assert_eq!(idx_adj, vec![0, 2, 5, 7, 10, 13, 14]);
		println!("{:?}, {:?}", adj, idx_adj);
	}

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
		let l = crate::tests::logical_design_tests::get_large_logical_design(40);
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

	#[test]
	fn metis_simple() {
		let connectivity = vec![
			vec![1, 2],
			vec![0, 2],
			vec![0, 1, 3],
			vec![2, 4, 5],
			vec![3, 5],
			vec![3, 4],
		];
		let (part, _ncuts) = metis(&connectivity, 2);
		assert_eq!(part.len(), 6);
		assert_eq!(part[0], part[1]);
		assert_eq!(part[0], part[2]);
		assert_ne!(part[0], part[3]);
		assert_ne!(part[0], part[4]);
		assert_ne!(part[0], part[5]);
		println!("{:?}", part);
	}

	#[test]
	fn metis_n_synthetic() {
		let n = 30_000;
		let n_parts = n / 1000;
		let l = crate::tests::logical_design_tests::get_large_logical_design(n as usize);
		let mut p = PhysicalDesign::new();
		p.extract_combs(&l);
		let connectivity = p.get_connectivity_as_vec_usize(&l);
		let (part, n_cuts) = metis(&connectivity, n_parts);
		report_partition_quality(&part, n_cuts, &connectivity, n_parts);
	}

	#[test]
	fn metis_2d_synthetic() {
		let n = 500;
		let n_parts = n * n / 1000;
		let l = crate::tests::logical_design_tests::get_large_logical_design_2d(n as usize);
		let mut p = PhysicalDesign::new();
		p.extract_combs(&l);
		let connectivity = p.get_connectivity_as_vec_usize(&l);
		let (part, n_cuts) = metis(&connectivity, n_parts);
		report_partition_quality(&part, n_cuts, &connectivity, n_parts);
	}
}

use std::{
	collections::{HashMap, HashSet},
	fs,
	hash::BuildHasherDefault,
	path::Path,
};

use hashers::fnv::FNV1aHasher64;
use itertools::Itertools;

pub(crate) fn hash_set<K>() -> HashSet<K, BuildHasherDefault<FNV1aHasher64>> {
	HashSet::default()
}

pub(crate) fn hash_map<K, V>() -> HashMap<K, V, BuildHasherDefault<FNV1aHasher64>> {
	HashMap::default()
}

pub(crate) type HashM<K, V> = HashMap<K, V, BuildHasherDefault<FNV1aHasher64>>;
pub(crate) type HashS<K> = HashSet<K, BuildHasherDefault<FNV1aHasher64>>;

#[allow(dead_code)]
pub(crate) fn construct_bidirectional_join<S1, S2>(
	lhs: &[S1],
	rhs: &[S2],
) -> (Vec<usize>, Vec<usize>)
where
	S1: AsRef<str>,
	S2: AsRef<str>,
{
	assert_eq!(lhs.len(), rhs.len());
	let lhs_index =
		lhs.iter()
			.enumerate()
			.fold(hash_map::<&str, usize>(), |mut map, (idx, the_str)| {
				map.insert(the_str.as_ref(), idx);
				map
			});
	let rhs_index =
		rhs.iter()
			.enumerate()
			.fold(hash_map::<&str, usize>(), |mut map, (idx, the_str)| {
				map.insert(the_str.as_ref(), idx);
				map
			});
	let forward = lhs
		.iter()
		.map(|the_str| {
			rhs_index
				.get(the_str.as_ref())
				.copied()
				.unwrap_or(usize::MAX)
		})
		.collect_vec();
	let reverse = rhs
		.iter()
		.map(|the_str| {
			lhs_index
				.get(the_str.as_ref())
				.copied()
				.unwrap_or(usize::MAX)
		})
		.collect_vec();

	(forward, reverse)
}

pub(crate) fn index_of(strings: &[&str], target: &str) -> Option<usize> {
	let mut i = 0;
	for s in strings {
		if *s == target {
			return Some(i);
		}
		i += 1;
	}
	None
}

pub(crate) fn convert_connectivity_to_csr(conn: &Vec<Vec<usize>>) -> (Vec<i32>, Vec<i32>) {
	let mut idx_adj = vec![0];
	let mut adj = vec![];
	let mut idx = 0;
	for neighbors in conn.iter() {
		for neigh in neighbors.iter().sorted() {
			adj.push(*neigh as i32);
			idx += 1;
		}
		idx_adj.push(idx);
	}
	(adj, idx_adj)
}

pub(crate) fn load_hex_file<P: AsRef<Path>>(path: P) -> Vec<i32> {
	let path = path.as_ref();
	let data = fs::read_to_string(path).expect("Failed to read file");
	data.lines()
		.map(|x| i32::from_str_radix(x, 16).unwrap())
		.collect_vec()
}

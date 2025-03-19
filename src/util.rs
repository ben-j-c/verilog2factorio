use std::{
	collections::{HashMap, HashSet},
	hash::BuildHasherDefault,
};

use hashers::fnv::FNV1aHasher64;

pub(crate) fn hash_set<K>() -> HashSet<K, BuildHasherDefault<FNV1aHasher64>> {
	HashSet::default()
}

pub(crate) fn hash_map<K, V>() -> HashMap<K, V, BuildHasherDefault<FNV1aHasher64>> {
	HashMap::default()
}

pub(crate) type HashM<K, V> = HashMap<K, V, BuildHasherDefault<FNV1aHasher64>>;
pub(crate) type HashS<K> = HashSet<K, BuildHasherDefault<FNV1aHasher64>>;

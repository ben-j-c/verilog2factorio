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

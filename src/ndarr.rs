use std::ops::{Index, IndexMut};

pub struct Arr2<T> {
	data: Vec<T>,
	dims: [usize; 2],
}

impl<T> Index<usize> for Arr2<T> {
	type Output = [T];

	fn index(&self, index: usize) -> &Self::Output {
		&self.data[index * self.dims[1]..(index + 1) * self.dims[1]]
	}
}

impl<T> IndexMut<usize> for Arr2<T> {
	fn index_mut(&mut self, index: usize) -> &mut Self::Output {
		&mut self.data[index * self.dims[1]..(index + 1) * self.dims[1]]
	}
}

impl<T> Arr2<T>
where
	T: Default + Clone,
{
	pub fn new(dims: [usize; 2]) -> Self {
		let total_size: usize = dims.iter().product();
		Self {
			data: vec![T::default(); total_size],
			dims: dims,
		}
	}
}

#[cfg(test)]
mod test {
	use super::*;
	#[test]
	fn new() {
		let x = Arr2::new([2, 2]);
		let _yy: i32 = x[0][0];
		let mut x = Arr2::new([2, 2]);
		x[0][1] = 4;
	}
}

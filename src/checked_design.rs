use crate::mapped_design::MappedDesign;

#[derive(Debug)]
pub struct CheckedDesign {}

impl CheckedDesign {
	pub fn new() -> Self {
		Self {}
	}

	pub fn build_from(&mut self, mapped_design: &MappedDesign) {}
}

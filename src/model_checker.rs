use std::{cell::RefCell, rc::Rc};

use crate::{
	logical_design::{LogicalDesign, NodeId},
	ndarr::Arr2,
	signal_lookup_table,
};

struct SimState {
	output_red: Arr2<i32>,
	output_green: Arr2<i32>,
	logd: Rc<RefCell<LogicalDesign>>,
}

impl SimState {
	fn new(logd: Rc<RefCell<LogicalDesign>>) -> Self {
		let n_nodes = logd.borrow().nodes.len();
		Self {
			output_red: Arr2::new([n_nodes, signal_lookup_table::n_ids() as usize]),
			output_green: Arr2::new([n_nodes, signal_lookup_table::n_ids() as usize]),
			logd,
		}
	}

	fn probe_red_output(&self, id: NodeId) -> &[i32] {
		&self.output_red[id.0]
	}

	fn probe_green_output(&self, id: NodeId) -> &[i32] {
		&self.output_red[id.0]
	}

	fn step(&mut self, steps: usize) {}
}

#[cfg(test)]
mod test {
	use std::{cell::RefCell, rc::Rc};

	use super::SimState;
	use crate::logical_design::LogicalDesign;

	#[test]
	fn new() {
		let logd = Rc::new(RefCell::new(LogicalDesign::new()));
		let nop = logd.borrow_mut().add_nop_simple();
		let mut sim = SimState::new(logd.clone());
		sim.probe_green_output(nop);
		logd.borrow_mut().nodes[nop.0].description = Some("The description".to_owned());
		sim.step(10);
		logd.borrow_mut().nodes[nop.0].description = Some("The description2".to_owned());
		sim.step(10);
	}
}

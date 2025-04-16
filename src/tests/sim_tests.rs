use std::{cell::RefCell, rc::Rc};

use crate::{
	logical_design::{LogicalDesign, Signal},
	sim::SimState,
};

#[test]
fn delay_correctness() {
	let logd = Rc::new(RefCell::new(LogicalDesign::new()));
	let (c1, nop) = {
		let mut logd = logd.borrow_mut();
		let c1 = logd.add_constant_comb(vec![Signal::Id(10)], vec![1]);
		let mut nops = vec![];
		nops.push(logd.add_nop_simple());
		logd.add_wire_red_simple(c1, nops[0]);
		for i in 1..100 {
			nops.push(logd.add_nop_simple());
			logd.add_wire_red_simple(nops[i - 1], nops[i]);
		}
		logd.add_wire_red_simple(nops[99], nops[0]);
		(c1, nops[0])
	};
	let mut sim = SimState::new(logd.clone());
	assert_eq!(sim.probe_red_out_sparse(c1), vec![]);
	assert_eq!(sim.probe_red_out_sparse(nop), vec![]);
	sim.step(1);
	{
		let mut logd = logd.borrow_mut();
		logd.set_ith_output_count(c1, 0, 0);
	}
	assert_eq!(sim.probe_red_out_sparse(c1), vec![(10, 1)]);
	assert_eq!(sim.probe_red_out_sparse(nop), vec![]);
	sim.step(1);
	assert_eq!(sim.probe_red_out_sparse(c1), vec![]);
	assert_eq!(sim.probe_red_out_sparse(nop), vec![(10, 1)]);
	sim.step(1);
	assert_eq!(sim.probe_red_out_sparse(c1), vec![]);
	assert_eq!(sim.probe_red_out_sparse(nop), vec![]);
	sim.step(99);
	assert_eq!(sim.probe_red_out_sparse(c1), vec![]);
	assert_eq!(sim.probe_red_out_sparse(nop), vec![(10, 1)]);
	for _ in 0..10 {
		sim.step(1);
		assert_eq!(sim.probe_red_out_sparse(c1), vec![]);
		assert_eq!(sim.probe_red_out_sparse(nop), vec![]);
		sim.step(98);
		assert_eq!(sim.probe_red_out_sparse(c1), vec![]);
		assert_eq!(sim.probe_red_out_sparse(nop), vec![]);
		sim.step(1);
		assert_eq!(sim.probe_red_out_sparse(c1), vec![]);
		assert_eq!(sim.probe_red_out_sparse(nop), vec![(10, 1)]);
	}
}

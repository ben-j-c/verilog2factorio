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
	assert_eq!(sim.probe_red_out(c1), vec![]);
	assert_eq!(sim.probe_red_out(nop), vec![]);
	sim.step(1);
	{
		let mut logd = logd.borrow_mut();
		logd.set_ith_output_count(c1, 0, 0);
	}
	assert_eq!(sim.probe_red_out(c1), vec![(10, 1)]);
	assert_eq!(sim.probe_red_out(nop), vec![]);
	sim.step(1);
	assert_eq!(sim.probe_red_out(c1), vec![]);
	assert_eq!(sim.probe_red_out(nop), vec![(10, 1)]);
	sim.step(1);
	assert_eq!(sim.probe_red_out(c1), vec![]);
	assert_eq!(sim.probe_red_out(nop), vec![]);
	sim.step(99);
	assert_eq!(sim.probe_red_out(c1), vec![]);
	assert_eq!(sim.probe_red_out(nop), vec![(10, 1)]);
	for _ in 0..100 {
		sim.step(1);
		assert_eq!(sim.probe_red_out(c1), vec![]);
		assert_eq!(sim.probe_red_out(nop), vec![]);
		sim.step(98);
		assert_eq!(sim.probe_red_out(c1), vec![]);
		assert_eq!(sim.probe_red_out(nop), vec![]);
		sim.step(1);
		assert_eq!(sim.probe_red_out(c1), vec![]);
		assert_eq!(sim.probe_red_out(nop), vec![(10, 1)]);
	}
}

use crate::logical_design::{
	DeciderOperator, DeciderRowConjDisj, NET_GREEN, NET_RED, NET_RED_GREEN,
};

#[test]
fn new() {
	let logd = Rc::new(RefCell::new(LogicalDesign::new()));
	let nop = logd.borrow_mut().add_nop_simple();
	let mut sim = SimState::new(logd.clone());
	sim.probe_green_out(nop);
	logd.borrow_mut().nodes[nop.0].description = Some("The description".to_owned());
	sim.step(10);
	logd.borrow_mut().nodes[nop.0].description = Some("The description2".to_owned());
	sim.step(10);
}

#[test]
fn constant_through_nop() {
	let logd = Rc::new(RefCell::new(LogicalDesign::new()));
	let (c1, nop, wire) = {
		let mut logd = logd.borrow_mut();
		let c1 = logd.add_constant_comb(vec![Signal::Id(10)], vec![1234]);
		let nop = logd.add_nop_simple();
		let wire = logd.add_wire_red_simple(c1, nop);
		println!("{:?}", logd);
		(c1, nop, wire)
	};
	let mut sim = SimState::new(logd.clone());
	sim.print();
	for (id, count) in sim.probe_red_out(c1) {
		assert_eq!(id, 0);
	}
	for x in sim.probe_red_out(nop) {
		assert_eq!(x.1, 0);
	}
	for x in sim.probe_red_out(wire) {
		assert_eq!(x.1, 0);
	}

	sim.step(1);
	sim.print();
	for (id, count) in sim.probe_red_out(c1) {
		if id == 10 {
			assert_eq!(count, 1234);
		} else {
			assert_eq!(count, 0);
		}
	}
	for x in sim.probe_red_out(nop) {
		assert_eq!(x.1, 0);
	}
	for (id, x) in sim.probe_red_out(wire) {
		if id == 10 {
			assert_eq!(x, 1234);
		} else {
			assert_eq!(x, 0);
		}
	}

	sim.step(1);
	sim.print();
	for (id, x) in sim.probe_red_out(c1).iter() {
		if *id == 10 {
			assert_eq!(*x, 1234);
		} else {
			assert_eq!(*x, 0);
		}
	}
	for (id, x) in sim.probe_red_out(nop).iter() {
		if *id == 10 {
			assert_eq!(*x, 1234);
		} else {
			assert_eq!(*x, 0);
		}
	}
	for (id, x) in sim.probe_red_out(wire).iter() {
		if *id == 10 {
			assert_eq!(*x, 1234);
		} else {
			assert_eq!(*x, 0);
		}
	}
}

#[test]
fn decider_simple() {
	let logd = Rc::new(RefCell::new(LogicalDesign::new()));
	let (c1, d1, wire) = {
		let mut logd = logd.borrow_mut();
		let c1 = logd.add_constant_comb(vec![Signal::Id(10)], vec![1234]);
		let d1 = logd.add_decider_comb();
		let wire = logd.add_wire_red_simple(c1, d1);
		logd.add_decider_comb_input(
			d1,
			(
				Signal::Id(10),
				DeciderOperator::GreaterThan,
				Signal::Constant(1233),
			),
			DeciderRowConjDisj::FirstRow,
			NET_RED_GREEN,
			NET_RED_GREEN,
		);
		logd.add_decider_out_constant(d1, Signal::Id(20), 1, NET_RED_GREEN);
		println!("{}", logd);
		(c1, d1, wire)
	};
	let mut sim = SimState::new(logd.clone());
	sim.step(1);
	assert_eq!(sim.probe_red_out(wire), vec![(10, 1234)]);
	assert_eq!(sim.probe_red_out(c1), vec![(10, 1234)]);
	sim.step(1);
	sim.print();
	assert_eq!(sim.probe_red_out(wire), vec![(10, 1234)]);
	assert_eq!(sim.probe_red_out(d1), vec![(20, 1)]);
}

#[test]
fn decider_simple2() {
	let logd = Rc::new(RefCell::new(LogicalDesign::new()));
	let (c1, d1, wire) = {
		let mut logd = logd.borrow_mut();
		let c1 = logd.add_constant_comb(vec![Signal::Id(10)], vec![1234]);
		let d1 = logd.add_decider_comb();
		let wire = logd.add_wire_red_simple(c1, d1);
		logd.add_decider_comb_input(
			d1,
			(
				Signal::Id(10),
				DeciderOperator::GreaterThan,
				Signal::Constant(1233),
			),
			DeciderRowConjDisj::FirstRow,
			NET_RED_GREEN,
			NET_RED_GREEN,
		);
		logd.add_decider_out_constant(d1, Signal::Id(20), 1, NET_RED_GREEN);
		logd.add_decider_out_constant(d1, Signal::Id(20), 1, NET_RED_GREEN);
		println!("{}", logd);
		(c1, d1, wire)
	};
	let mut sim = SimState::new(logd.clone());
	sim.step(1);
	assert_eq!(sim.probe_red_out(wire), vec![(10, 1234)]);
	assert_eq!(sim.probe_red_out(c1), vec![(10, 1234)]);
	sim.step(1);
	sim.print();
	assert_eq!(sim.probe_red_out(wire), vec![(10, 1234)]);
	assert_eq!(sim.probe_red_out(d1), vec![(20, 2)]);
}

#[test]
fn decider_simple3() {
	let logd = Rc::new(RefCell::new(LogicalDesign::new()));
	let (c1, d1, wire) = {
		let mut logd = logd.borrow_mut();
		let c1 = logd.add_constant_comb(vec![Signal::Id(10)], vec![1234]);
		let d1 = logd.add_decider_comb();
		let wire = logd.add_wire_red_simple(c1, d1);
		logd.add_decider_comb_input(
			d1,
			(
				Signal::Id(10),
				DeciderOperator::GreaterThan,
				Signal::Constant(1233),
			),
			DeciderRowConjDisj::FirstRow,
			NET_RED_GREEN,
			NET_RED_GREEN,
		);
		logd.add_decider_comb_input(
			d1,
			(
				Signal::Id(10),
				DeciderOperator::LessThan,
				Signal::Constant(1235),
			),
			DeciderRowConjDisj::And,
			NET_RED_GREEN,
			NET_RED_GREEN,
		);
		logd.add_decider_out_constant(d1, Signal::Id(20), 1, NET_RED_GREEN);
		logd.add_decider_out_constant(d1, Signal::Id(20), 1, NET_RED_GREEN);
		println!("{}", logd);
		(c1, d1, wire)
	};
	let mut sim = SimState::new(logd.clone());
	sim.step(1);
	assert_eq!(sim.probe_red_out(wire), vec![(10, 1234)]);
	assert_eq!(sim.probe_red_out(c1), vec![(10, 1234)]);
	sim.step(1);
	sim.print();
	assert_eq!(sim.probe_red_out(wire), vec![(10, 1234)]);
	assert_eq!(sim.probe_red_out(d1), vec![(20, 2)]);
}

#[test]
fn decider_simple4() {
	let logd = Rc::new(RefCell::new(LogicalDesign::new()));
	let (c1, _, _, wire, wire2) = {
		let mut logd = logd.borrow_mut();
		let c1 = logd.add_constant_comb(vec![Signal::Id(10)], vec![1234]);
		let d1 = logd.add_decider_comb();
		let d2 = logd.add_decider_comb();
		let wire = logd.add_wire_red(vec![c1], vec![d1, d2]);
		let wire2 = logd.add_wire_red(vec![d1, d2], vec![]);
		logd.add_decider_comb_input(
			d1,
			(
				Signal::Id(10),
				DeciderOperator::GreaterThan,
				Signal::Constant(1233),
			),
			DeciderRowConjDisj::FirstRow,
			NET_RED_GREEN,
			NET_RED_GREEN,
		);
		logd.add_decider_comb_input(
			d2,
			(
				Signal::Id(10),
				DeciderOperator::LessThan,
				Signal::Constant(1235),
			),
			DeciderRowConjDisj::And,
			NET_RED_GREEN,
			NET_RED_GREEN,
		);
		logd.add_decider_out_constant(d1, Signal::Id(20), 1, NET_RED_GREEN);
		logd.add_decider_out_constant(d2, Signal::Id(20), 1, NET_RED_GREEN);
		println!("{}", logd);
		(c1, d1, d2, wire, wire2)
	};
	let mut sim = SimState::new(logd.clone());
	sim.step(1);
	assert_eq!(sim.probe_red_out(wire), vec![(10, 1234)]);
	assert_eq!(sim.probe_red_out(c1), vec![(10, 1234)]);
	sim.step(1);
	sim.print();
	assert_eq!(sim.probe_red_out(wire), vec![(10, 1234)]);
	assert_eq!(sim.probe_red_out(wire2), vec![(20, 2)]);
}

#[test]
fn decider_each1() {
	let logd = Rc::new(RefCell::new(LogicalDesign::new()));
	let (c1, c2, d1, d2) = {
		let mut logd = logd.borrow_mut();
		let c1 = logd.add_constant_comb(vec![Signal::Id(10)], vec![1]);
		let c2 = logd.add_constant_comb(vec![Signal::Id(10)], vec![2]);
		let d1 = logd.add_decider_comb();
		let d2 = logd.add_decider_comb();
		logd.add_wire_red(vec![c1], vec![d1]);
		logd.add_wire_red(vec![c1], vec![d2]);
		logd.add_wire_green(vec![c2], vec![d1]);
		logd.add_wire_green(vec![c2], vec![d2]);
		logd.add_decider_comb_input(
			d1,
			(Signal::Each, DeciderOperator::Equal, Signal::Constant(1)),
			DeciderRowConjDisj::FirstRow,
			NET_RED, // Only accept on c1
			NET_RED_GREEN,
		);
		logd.add_decider_out_input_count(d1, Signal::Each, NET_RED_GREEN);
		logd.add_decider_comb_input(
			d2,
			(Signal::Each, DeciderOperator::Equal, Signal::Constant(1)),
			DeciderRowConjDisj::FirstRow,
			NET_RED, // Only accept on c1
			NET_RED_GREEN,
		);
		logd.add_decider_out_input_count(d2, Signal::Each, NET_GREEN);
		(c1, c2, d1, d2)
	};
	let mut sim = SimState::new(logd.clone());
	assert_eq!(sim.probe_red_out(c1), vec![]);
	assert_eq!(sim.probe_red_out(c2), vec![]);
	assert_eq!(sim.probe_red_out(d1), vec![]);
	assert_eq!(sim.probe_red_out(d2), vec![]);
	sim.step(1);
	assert_eq!(sim.probe_red_out(c1), vec![(10, 1)]);
	assert_eq!(sim.probe_red_out(c2), vec![(10, 2)]);
	assert_eq!(sim.probe_red_out(d1), vec![]);
	assert_eq!(sim.probe_red_out(d2), vec![]);
	sim.step(1);
	sim.print();
	assert_eq!(sim.probe_red_out(c1), vec![(10, 1)]);
	assert_eq!(sim.probe_red_out(c2), vec![(10, 2)]);
	assert_eq!(sim.probe_red_out(d1), vec![(10, 3)]); // Even though only c1 is seen, c2 is summed too.
	assert_eq!(sim.probe_red_out(d2), vec![(10, 2)]); // d2 sees c1, but forwards c2
}

#[test]
fn decider_each2() {
	let logd = Rc::new(RefCell::new(LogicalDesign::new()));
	let (c1, c2, d1, _, _) = {
		let mut logd = logd.borrow_mut();
		let c1 = logd.add_constant_comb(vec![Signal::Id(10)], vec![1]);
		let c2 = logd.add_constant_comb(vec![Signal::Id(11)], vec![2]);
		let d1 = logd.add_decider_comb();
		let wire1 = logd.add_wire_red(vec![c1], vec![d1]);
		let wire2 = logd.add_wire_green(vec![c2], vec![d1]);
		logd.add_decider_comb_input(
			d1,
			(Signal::Each, DeciderOperator::Equal, Signal::Constant(1)),
			DeciderRowConjDisj::FirstRow,
			NET_RED,
			NET_RED_GREEN,
		);
		logd.add_decider_out_input_count(d1, Signal::Each, NET_RED_GREEN);
		(c1, c2, d1, wire1, wire2)
	};
	let mut sim = SimState::new(logd.clone());
	assert_eq!(sim.probe_red_out(c1), vec![]);
	assert_eq!(sim.probe_red_out(c2), vec![]);
	assert_eq!(sim.probe_red_out(d1), vec![]);
	sim.step(1);
	assert_eq!(sim.probe_red_out(c1), vec![(10, 1)]);
	assert_eq!(sim.probe_red_out(c2), vec![(11, 2)]);
	assert_eq!(sim.probe_red_out(d1), vec![]);
	sim.step(1);
	sim.print();
	assert_eq!(sim.probe_red_out(c1), vec![(10, 1)]); // This should be seen by the Each.
	assert_eq!(sim.probe_red_out(c2), vec![(11, 2)]); // This one shouldn't be seen by the Each.
	assert_eq!(sim.probe_red_out(d1), vec![(10, 1)]); // So we only get c1
}

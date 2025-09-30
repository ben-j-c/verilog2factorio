use std::{
	fs::File,
	io::{BufRead, BufReader},
	sync::{Arc, RwLock},
};

use itertools::Itertools;

use crate::{
	checked_design::CheckedDesign,
	logical_design::{LogicalDesign, Signal},
	mapped_design::MappedDesign,
	phy::PhysicalDesign,
	serializable_design::SerializableDesign,
	signal_lookup_table,
	sim::SimState,
};

#[test]
fn delay_correctness() {
	let logd = Arc::new(RwLock::new(LogicalDesign::new()));
	let (c1, nop) = {
		let mut logd = logd.write().unwrap();
		let c1 = logd.add_constant(vec![Signal::Id(10)], vec![1]);
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
		let mut logd = logd.write().unwrap();
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
	let logd = Arc::new(RwLock::new(LogicalDesign::new()));
	let nop = logd.write().unwrap().add_nop_simple();
	let mut sim = SimState::new(logd.clone());
	sim.probe_green_out(nop);
	logd.write().unwrap().nodes[nop.0].description = Some("The description".to_owned());
	sim.step(10);
	logd.write().unwrap().nodes[nop.0].description = Some("The description2".to_owned());
	sim.step(10);
}

#[test]
fn constant_through_nop() {
	let logd = Arc::new(RwLock::new(LogicalDesign::new()));
	let (c1, nop, wire) = {
		let mut logd = logd.write().unwrap();
		let c1 = logd.add_constant(vec![Signal::Id(10)], vec![1234]);
		let nop = logd.add_nop_simple();
		let wire = logd.add_wire_red_simple(c1, nop);
		println!("{:?}", logd);
		(c1, nop, wire)
	};
	let mut sim = SimState::new(logd.clone());
	sim.print();
	for (id, _count) in sim.probe_red_out(c1) {
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
	let logd = Arc::new(RwLock::new(LogicalDesign::new()));
	let (c1, d1, wire) = {
		let mut logd = logd.write().unwrap();
		let c1 = logd.add_constant(vec![Signal::Id(10)], vec![1234]);
		let d1 = logd.add_decider();
		let wire = logd.add_wire_red_simple(c1, d1);
		logd.add_decider_input(
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
	let logd = Arc::new(RwLock::new(LogicalDesign::new()));
	let (c1, d1, wire) = {
		let mut logd = logd.write().unwrap();
		let c1 = logd.add_constant(vec![Signal::Id(10)], vec![1234]);
		let d1 = logd.add_decider();
		let wire = logd.add_wire_red_simple(c1, d1);
		logd.add_decider_input(
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
	let logd = Arc::new(RwLock::new(LogicalDesign::new()));
	let (c1, d1, wire) = {
		let mut logd = logd.write().unwrap();
		let c1 = logd.add_constant(vec![Signal::Id(10)], vec![1234]);
		let d1 = logd.add_decider();
		let wire = logd.add_wire_red_simple(c1, d1);
		logd.add_decider_input(
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
		logd.add_decider_input(
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
	let logd = Arc::new(RwLock::new(LogicalDesign::new()));
	let (c1, _, _, wire, wire2) = {
		let mut logd = logd.write().unwrap();
		let c1 = logd.add_constant(vec![Signal::Id(10)], vec![1234]);
		let d1 = logd.add_decider();
		let d2 = logd.add_decider();
		let wire = logd.add_wire_red(vec![c1], vec![d1, d2]);
		let wire2 = logd.add_wire_red(vec![d1, d2], vec![]);
		logd.add_decider_input(
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
		logd.add_decider_input(
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
	let logd = Arc::new(RwLock::new(LogicalDesign::new()));
	let (c1, c2, d1, d2) = {
		let mut logd = logd.write().unwrap();
		let c1 = logd.add_constant(vec![Signal::Id(10)], vec![1]);
		let c2 = logd.add_constant(vec![Signal::Id(10)], vec![2]);
		let d1 = logd.add_decider();
		let d2 = logd.add_decider();
		logd.add_wire_red(vec![c1], vec![d1]);
		logd.add_wire_red(vec![c1], vec![d2]);
		logd.add_wire_green(vec![c2], vec![d1]);
		logd.add_wire_green(vec![c2], vec![d2]);
		logd.add_decider_input(
			d1,
			(Signal::Each, DeciderOperator::Equal, Signal::Constant(1)),
			DeciderRowConjDisj::FirstRow,
			NET_RED, // Only accept on c1
			NET_RED_GREEN,
		);
		logd.add_decider_out_input_count(d1, Signal::Each, NET_RED_GREEN);
		logd.add_decider_input(
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
	let logd = Arc::new(RwLock::new(LogicalDesign::new()));
	let (c1, c2, d1, _, _) = {
		let mut logd = logd.write().unwrap();
		let c1 = logd.add_constant(vec![Signal::Id(10)], vec![1]);
		let c2 = logd.add_constant(vec![Signal::Id(11)], vec![2]);
		let d1 = logd.add_decider();
		let wire1 = logd.add_wire_red(vec![c1], vec![d1]);
		let wire2 = logd.add_wire_green(vec![c2], vec![d1]);
		logd.add_decider_input(
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

#[test]
fn test10_multiport_rom() {
	let period = 4;
	let data_len = 64;
	let mut data = vec![];
	{
		let data_file = File::open("./test_designs/test8_memory_hex.txt").unwrap();
		let reader = BufReader::new(data_file);
		for line in reader.lines() {
			let hex_str = line.unwrap();
			let value = u32::from_str_radix(&hex_str, 16).unwrap();
			data.push(value as i32);
		}
	}

	let file = File::open("./test_designs/output/test10.json").unwrap();
	let reader = BufReader::new(file);
	let mapped_design: MappedDesign = serde_json::from_reader(reader).unwrap();
	let mut checked_design = CheckedDesign::new();
	let mut logd = LogicalDesign::new();
	checked_design.build_from(&mapped_design);
	logd.build_from(&checked_design, &mapped_design);
	let mut phy = PhysicalDesign::new();
	phy.build_from(&logd);
	let mut serializable_design = SerializableDesign::new();
	serializable_design.build_from(&phy, &logd);
	let blueprint_json = serde_json::to_string(&serializable_design).unwrap();
	println!("{blueprint_json}");
	let in_1 = logd.get_port_node("signal_1_1").unwrap();
	let in_2 = logd.get_port_node("signal_1_2").unwrap();
	let out_1 = logd.get_port_node("signal_2_1").unwrap();
	let out_2 = logd.get_port_node("signal_2_2").unwrap();

	let signal_2 = signal_lookup_table::lookup_id("signal_2").unwrap();

	let logd = Arc::new(RwLock::new(logd));
	let mut sim = SimState::new(logd.clone());
	sim.add_trace(in_1);
	sim.add_trace(in_2);

	for idx in 0..data_len {
		let idx1 = idx;
		let idx2 = ((idx + data_len / 2) % data_len) as usize;
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(in_1, 0, idx1 as i32);
			logd.set_ith_output_count(in_2, 0, idx2 as i32);
		}
		for _ in 0..period {
			let logd = logd.read().unwrap();
			sim.step(1);
			let _ = phy.save_svg_full(Some(&sim), &logd, "svg/test10_phy_sim.svg");
		}
		assert_eq!(
			sim.probe_input(out_1, NET_RED_GREEN),
			vec![(signal_2, data[idx])]
		);
		assert_eq!(
			sim.probe_input(out_2, NET_RED_GREEN),
			vec![(signal_2, data[idx2])]
		);
	}
}

#[test]
fn pmux() {
	let mut logd = LogicalDesign::new();
	let a = signal_lookup_table::lookup_sig("signal_A");
	let b0 = signal_lookup_table::lookup_sig("signal_0");
	let b1 = signal_lookup_table::lookup_sig("signal_1");
	let s0 = signal_lookup_table::lookup_sig("signal_S");
	let s1 = signal_lookup_table::lookup_sig("signal_T");
	let y = signal_lookup_table::lookup_sig("signal_Y");
	let input_a = logd.add_constant(vec![a], vec![777]);
	let input_b0 = logd.add_constant(vec![b0], vec![888]);
	let input_b1 = logd.add_constant(vec![b1], vec![999]);
	let input_s0 = logd.add_constant(vec![s0], vec![1]);
	let input_s1 = logd.add_constant(vec![s1], vec![1]);
	let output_y = logd.add_lamp((y, DeciderOperator::NotEqual, Signal::Constant(0)));
	logd.set_constant_enabled(input_s0, true);
	logd.set_constant_enabled(input_s1, false);

	{
		let (wire_a_opt, wires_b, wires_s, comb_y) =
			logd.add_pmux(Some(a), &[b0, b1], &[s0, s1], y, None);
		logd.connect_red(input_a, wire_a_opt.unwrap());
		logd.connect_red(input_b0, wires_b[0]);
		logd.connect_red(input_b1, wires_b[1]);
		logd.connect_red(input_s0, wires_s[0]);
		logd.connect_red(input_s1, wires_s[1]);
		logd.add_wire_red(vec![comb_y], vec![output_y]);
	}

	let logd = Arc::new(RwLock::new(logd));
	let mut sim = SimState::new(logd.clone());
	sim.step(3);
	assert_eq!(
		sim.probe_input(output_y, NET_RED_GREEN),
		vec![(y.id(), 888)]
	);

	{
		let mut logd = logd.write().unwrap();
		logd.set_constant_enabled(input_s0, false);
		logd.set_constant_enabled(input_s1, true);
	}
	sim.step(3);
	assert_eq!(
		sim.probe_input(output_y, NET_RED_GREEN),
		vec![(y.id(), 999)]
	);

	{
		let mut logd = logd.write().unwrap();
		logd.set_constant_enabled(input_s0, false);
		logd.set_constant_enabled(input_s1, false);
	}
	sim.step(3);
	assert_eq!(
		sim.probe_input(output_y, NET_RED_GREEN),
		vec![(y.id(), 777)]
	);
}

#[test]
fn pmux_n() {
	let n = 16;
	let mut logd = LogicalDesign::new();
	let a = signal_lookup_table::lookup_sig("signal_A");
	let y = signal_lookup_table::lookup_sig("signal_Y");
	let input_a = logd.add_constant(vec![a], vec![777]);
	logd.set_description_node(input_a, format!("input_a"));
	let b_sigs = (0..n).map(|i| Signal::Id(i)).collect_vec();
	let s_sigs = (0..n).map(|i| Signal::Id(i + 100)).collect_vec();
	let mut input_b = vec![];
	let mut input_s = vec![];
	for i in 0..n {
		let b = logd.add_constant(vec![Signal::Id(i)], vec![i + 500]);
		logd.set_description_node(b, format!("b{i}"));
		input_b.push(b);
	}
	for i in 0..n {
		let s = logd.add_constant(vec![Signal::Id(i + 100)], vec![1]);
		input_s.push(s);
		logd.set_description_node(s, format!("s{i}"));
		logd.set_constant_enabled(*input_s.last().unwrap(), false);
	}
	let output_y = logd.add_lamp((y, DeciderOperator::NotEqual, Signal::Constant(0)));
	logd.set_description_node(output_y, "output_y");

	{
		let (wire_a_opt, wires_b, wires_s, comb_y) =
			logd.add_pmux(Some(a), &b_sigs, &s_sigs, y, None);
		logd.connect_red(input_a, wire_a_opt.unwrap());
		for i in 0..n as usize {
			logd.connect_red(input_b[i], wires_b[i]);
			logd.connect_red(input_s[i], wires_s[i]);
		}
		logd.add_wire_red(vec![comb_y], vec![output_y]);
	}

	{
		let mut pd = PhysicalDesign::new();
		pd.build_from(&logd);
		let mut sd = SerializableDesign::new();
		sd.build_from(&pd, &logd);
		let blueprint_json = serde_json::to_string(&sd).unwrap();
		println!("{}", blueprint_json);
	}

	let logd = Arc::new(RwLock::new(logd));
	let mut sim = SimState::new(logd.clone());
	sim.step(3);
	assert_eq!(
		sim.probe_input(output_y, NET_RED_GREEN),
		vec![(y.id(), 777)]
	);

	for i in 0..n as usize {
		let count = i as i32 + 500;
		{
			let mut logd = logd.write().unwrap();
			logd.set_constant_enabled(input_s[i], true);
		}

		sim.step(3);
		assert_eq!(
			sim.probe_input(output_y, NET_RED_GREEN),
			vec![(y.id(), count)]
		);
		{
			let mut logd = logd.write().unwrap();
			logd.set_constant_enabled(input_s[i], false);
		}
	}
}

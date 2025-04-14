use crate::logical_design::*;

#[cfg(test)]
pub(crate) fn get_simple_logical_design() -> LogicalDesign {
	use ArithmeticOperator as Aop;
	use DeciderOperator as Dop;
	use Signal as Sig;
	let mut d = LogicalDesign::new();
	let constant1 = d.add_constant_comb(vec![Sig::Id(0)], vec![100]);
	let constant2 = d.add_constant_comb(vec![Sig::Id(1)], vec![4]);
	let mult = d.add_arithmetic_comb((Sig::Id(1), Aop::Mult, Sig::Id(0)), Sig::Id(10));
	let lamp = d.add_lamp((Sig::Id(10), Dop::Equal, Sig::Constant(400)));
	let _wire_pre_mult = d.add_wire_red(vec![constant1, constant2], vec![mult]);
	let _wire_post_mult = d.add_wire_red(vec![mult], vec![lamp]);
	d
}

#[cfg(test)]
pub(crate) fn get_complex_40_logical_design() -> LogicalDesign {
	use ArithmeticOperator as Aop;
	use DeciderOperator as Dop;
	use Signal as Sig;
	let mut d = LogicalDesign::new();
	let mut constants = vec![];
	for i in 0..20 {
		constants.push(d.add_constant_comb(vec![(Sig::Id(i))], vec![i + 1]));
	}
	let mut mults = vec![];
	for i in 0..10 {
		mults.push(d.add_arithmetic_comb(
			(Sig::Id(i * 2), Aop::Mult, Sig::Id(i * 2 + 1)),
			Sig::Id(20 + i),
		));
	}
	let mut lamps = vec![];
	for i in 0..10 {
		lamps.push(d.add_lamp((
			Sig::Id(20 + i),
			Dop::Equal,
			Sig::Constant((i * 2 + 1) * (i * 2 + 2)),
		)));
	}
	for i in 0..10 {
		d.add_wire_red(vec![constants[i * 2], constants[i * 2 + 1]], vec![mults[i]]);
		d.add_wire_red(vec![mults[i]], vec![lamps[i]]);
	}
	d
}

#[cfg(test)]
pub(crate) fn get_large_logical_design(n: usize) -> LogicalDesign {
	use crate::logical_design;

	let mut l = LogicalDesign::new();
	for _ in 0..n {
		let id = l.add_nop(logical_design::Signal::Id(0), logical_design::Signal::Id(0));
		l.set_description_node(id, format!("{}", id.0));
	}
	for i in 0..20 {
		for j in (20..n).step_by(20) {
			let left = NodeId(j + i - 20);
			let right = NodeId(j + i);
			l.add_wire_green(vec![left], vec![right]);
		}
	}
	for i in (1..n).step_by(2) {
		let left = NodeId(i - 1);
		let right = NodeId(i);
		l.add_wire_red(vec![left], vec![right]);
	}
	for i in (6..n).step_by(7) {
		let left = NodeId(i - 3);
		let right = NodeId(i);
		l.add_wire_red(vec![left], vec![right]);
	}
	for i in (10..n).step_by(11) {
		let left = NodeId(i - 7);
		let right = NodeId(i);
		l.add_wire_red(vec![left], vec![right]);
	}
	l
}

#[cfg(test)]
pub(crate) fn get_large_logical_design_2d(n: usize) -> LogicalDesign {
	use crate::logical_design;

	let mut l = LogicalDesign::new();
	let mut cells = vec![vec![]; n];
	cells[0].push(l.add_nop(logical_design::Signal::Id(0), logical_design::Signal::Id(0)));
	for i in 0..n {
		for j in 0..n {
			if i == 0 && j == 0 {
				continue;
			}
			let c = l.add_nop(logical_design::Signal::Id(0), logical_design::Signal::Id(0));
			if i != 0 {
				l.add_wire_red(vec![cells[i - 1][j]], vec![c]);
			}
			if j != 0 {
				l.add_wire_green(vec![cells[i][j - 1]], vec![c]);
			}
			cells[i].push(c);
		}
	}
	l
}

#[cfg(test)]
pub(crate) fn get_large_memory_test_design(n: usize) -> LogicalDesign {
	let data = vec![0; n];
	let mut d = LogicalDesign::new();
	let mpf_arr = d.add_rom(
		vec![MemoryReadPort {
			addr: Signal::Id(0),
			data: Signal::Id(1),
			clk: None,
			en: None,
			rst: ResetSpec::Disabled,
			transparent: false,
		}],
		data,
		None,
	);
	assert_eq!(mpf_arr.len(), 1);
	let port = mpf_arr.first().unwrap();
	let addr = d.add_constant_comb(vec![Signal::Id(0)], vec![6]);
	let data = d.add_lamp((
		Signal::Id(1),
		DeciderOperator::Equal,
		Signal::Constant(2000),
	));
	d.connect_red(addr, port.addr_wire);
	d.add_wire_red(vec![port.data], vec![data]);
	d
}

#[cfg(test)]
pub(crate) fn get_large_dense_memory_test_design(n: usize) -> LogicalDesign {
	let mut data = vec![0; n];
	for x in 0..n {
		data[x] = x as i32;
	}
	let mut d = LogicalDesign::new();
	let mpf_arr = d.add_rom(
		vec![MemoryReadPort {
			addr: Signal::Id(0),
			data: Signal::Id(1),
			clk: None,
			en: None,
			rst: ResetSpec::Disabled,
			transparent: false,
		}],
		data,
		Some(256),
	);
	assert_eq!(mpf_arr.len(), 1);
	let port = mpf_arr.first().unwrap();
	let addr = d.add_constant_comb(vec![Signal::Id(0)], vec![6]);
	let data = d.add_lamp((
		Signal::Id(1),
		DeciderOperator::Equal,
		Signal::Constant(2000),
	));
	d.connect_red(addr, port.addr_wire);
	d.add_wire_red(vec![port.data], vec![data]);
	d
}

#[cfg(test)]
mod test {
	use std::{cell::RefCell, rc::Rc};

	use crate::{
		phy::PhysicalDesign, serializable_design::SerializableDesign, signal_lookup_table,
		sim::SimState,
	};

	use super::*;
	#[allow(unused)]
	use ArithmeticOperator as Aop;
	use DeciderOperator as Dop;
	use Signal as Sig;

	#[test]
	fn new() {
		let d = LogicalDesign::new();
		assert_eq!(d.nodes.len(), 0);
	}

	#[test]
	fn single_combinator() {
		let mut d = LogicalDesign::new();
		let lamp = d.add_lamp((Sig::Id(0), Dop::Equal, Sig::Id(1)));
		assert_eq!(d.nodes.len(), 1);
		assert_eq!(lamp.0, 0);
		assert_eq!(d.nodes[lamp.0].id, lamp);
	}

	#[test]
	fn output_to_input() {
		let mut d = LogicalDesign::new();
		let constant = d.add_constant_comb(vec![Sig::Id(2)], vec![100]);
		let lamp = d.add_lamp((Sig::Id(2), Dop::Equal, Sig::Constant(100)));
		let wire = d.add_wire_red(vec![constant], vec![lamp]);
		assert_eq!(d.nodes.len(), 3);

		assert_eq!(constant.0, 0);
		assert_eq!(lamp.0, 1);

		assert_eq!(d.nodes[constant.0].id, constant);
		assert_eq!(d.nodes[lamp.0].id, lamp);

		assert!(d.nodes[constant.0].fanout_red.contains(&wire));
		assert!(d.nodes[lamp.0].fanin_red.contains(&wire));

		assert_eq!(d.nodes[constant.0].fanout_red.len(), 1);
		assert_eq!(d.nodes[lamp.0].fanin_red.len(), 1);

		assert_eq!(d.get_node(constant).output.len(), 1);
	}

	#[test]
	fn loopback() {
		let mut d = LogicalDesign::new();
		let counter = d.add_arithmetic_comb((Sig::Id(0), Aop::Add, Sig::Constant(0)), Sig::Id(0));
		let filter_pre =
			d.add_arithmetic_comb((Sig::Id(0), Aop::Mult, Sig::Constant(1)), Sig::Id(0));
		let filter_post =
			d.add_arithmetic_comb((Sig::Id(0), Aop::Mult, Sig::Constant(1)), Sig::Id(0));
		d.add_wire_red(vec![counter, filter_pre], vec![counter, filter_post]);
		d.for_all(|_, x| println!("{:?}", x));
		let mut p = PhysicalDesign::new();
		let mut s = SerializableDesign::new();
		p.build_from(&d);
		s.build_from(&p, &d)
	}

	#[test]
	fn two_wire_sum() {
		let mut d = LogicalDesign::new();
		let a = d.add_arithmetic_comb((Sig::Id(10), Aop::Add, Sig::Constant(0)), Sig::Id(10));
		let b = d.add_arithmetic_comb((Sig::Id(10), Aop::Add, Sig::Constant(0)), Sig::Id(10));
		let c = d.add_arithmetic_comb((Sig::Id(10), Aop::Add, Sig::Constant(0)), Sig::Id(10));
		d.add_wire_red(vec![a], vec![c]);
		d.add_wire_red(vec![b], vec![c]);

		let mut p = PhysicalDesign::new();
		let mut s = SerializableDesign::new();
		p.build_from(&d);
		s.build_from(&p, &d)
	}

	#[test]
	fn red_green_wires() {
		let mut d = LogicalDesign::new();
		let a = d.add_arithmetic_comb((Sig::Id(10), Aop::Add, Sig::Constant(0)), Sig::Id(10));
		let b = d.add_arithmetic_comb((Sig::Id(10), Aop::Add, Sig::Constant(0)), Sig::Id(10));
		let c = d.add_arithmetic_comb((Sig::Id(10), Aop::Add, Sig::Constant(0)), Sig::Id(10));
		d.add_wire_red(vec![a], vec![c]);
		d.add_wire_green(vec![b], vec![c]);

		let mut p = PhysicalDesign::new();
		let mut s = SerializableDesign::new();
		p.build_from(&d);
		s.build_from(&p, &d);
		let blueprint_json = serde_json::to_string(&s).unwrap();
		println!("{}", blueprint_json);
	}

	#[test]
	fn dff() {
		let mut d = LogicalDesign::new();
		let (wire_data, wire_clk, comb_out) =
			d.add_dff(Signal::Id(0), Signal::Id(1), Signal::Id(2));
		let c1 = d.add_constant_comb(vec![Signal::Id(0)], vec![0]);
		let c2 = d.add_constant_comb(vec![Signal::Id(1)], vec![1]);
		let l1 = d.add_lamp((Signal::Id(2), Dop::NotEqual, Signal::Constant(0)));
		d.connect_red(c1, wire_data);
		d.connect_red(c2, wire_clk);
		d.add_wire_red(vec![comb_out], vec![l1]);

		let mut p = PhysicalDesign::new();
		let mut s = SerializableDesign::new();
		p.build_from(&d);
		s.build_from(&p, &d);
		let blueprint_json = serde_json::to_string(&s).unwrap();
		println!("{}", blueprint_json);
	}

	#[test]
	fn latch() {
		let mut d = LogicalDesign::new();
		let (wire_data, wire_clk, comb_out) = d.add_latch(Signal::Id(0), Signal::Id(1));
		let c1 = d.add_constant_comb(vec![Signal::Id(0)], vec![0]);
		let c2 = d.add_constant_comb(vec![Signal::Id(1)], vec![1]);
		let l1 = d.add_lamp((Signal::Id(2), Dop::NotEqual, Signal::Constant(0)));
		d.connect_red(c1, wire_data);
		d.connect_red(c2, wire_clk);
		d.add_wire_red(vec![comb_out], vec![l1]);

		let mut p = PhysicalDesign::new();
		let mut s = SerializableDesign::new();
		p.build_from(&d);
		s.build_from(&p, &d);
		let blueprint_json = serde_json::to_string(&s).unwrap();
		println!("{}", blueprint_json);
	}

	#[test]
	fn output_to_output() {
		let mut d = LogicalDesign::new();
		let nop1 = d.add_nop(Signal::Each, Signal::Each);
		let nop2 = d.add_nop(Signal::Each, Signal::Each);
		d.add_wire_red(vec![nop1, nop2], vec![]);

		let mut p = PhysicalDesign::new();
		let mut s = SerializableDesign::new();
		p.build_from(&d);
		s.build_from(&p, &d);
		let blueprint_json = serde_json::to_string(&s).unwrap();
		println!("\n{}\n", blueprint_json);
	}

	#[test]
	fn input_to_input() {
		let mut d = LogicalDesign::new();
		let nop0 = d.add_nop(Signal::Each, Signal::Each);
		let nop1 = d.add_nop(Signal::Each, Signal::Each);
		let nop2 = d.add_nop(Signal::Each, Signal::Each);
		let nop3 = d.add_nop(Signal::Each, Signal::Each);
		d.add_wire_red(vec![], vec![nop1, nop2]);
		d.add_wire_red(vec![nop0], vec![nop1]);
		d.add_wire_red(vec![nop1], vec![nop3]);

		for x in &d.nodes {
			println!("{:?}", x);
		}

		let mut p = PhysicalDesign::new();
		let mut s = SerializableDesign::new();
		p.build_from(&d);
		s.build_from(&p, &d);
		let blueprint_json = serde_json::to_string(&s).unwrap();
		println!("\n{}\n", blueprint_json);
	}

	#[test]
	fn rom_simple() {
		let data = vec![555, 666, 777, 888, 999, 1000, 2000, 3000, 4000, 5000];
		let mut d = LogicalDesign::new();
		let mpf_arr = d.add_rom(
			vec![MemoryReadPort {
				addr: Sig::Id(0),
				data: Sig::Id(1),
				clk: None,
				en: None,
				rst: ResetSpec::Disabled,
				transparent: false,
			}],
			data,
			None,
		);
		assert_eq!(mpf_arr.len(), 1);
		let port = mpf_arr.first().unwrap();
		let addr = d.add_constant_comb(vec![Sig::Id(0)], vec![6]);
		let data = d.add_lamp((Sig::Id(1), Dop::Equal, Sig::Constant(2000)));
		d.connect_red(addr, port.addr_wire);
		d.add_wire_red(vec![port.data], vec![data]);

		let mut p = PhysicalDesign::new();
		let mut s = SerializableDesign::new();
		p.build_from(&d);
		s.build_from(&p, &d);
		let blueprint_json = serde_json::to_string(&s).unwrap();
		println!("\n{}\n", blueprint_json);
	}

	#[test]
	fn rom_dense_simple() {
		let data = vec![555, 666, 777, 888, 999, 1000, 2000, 3000, 4000, 5000];
		let mut d = LogicalDesign::new();
		let mpf_arr = d.add_rom(
			vec![MemoryReadPort {
				addr: Sig::Id(0),
				data: Sig::Id(1),
				clk: None,
				en: None,
				rst: ResetSpec::Disabled,
				transparent: false,
			}],
			data,
			Some(10),
		);
		assert_eq!(mpf_arr.len(), 1);
		let port = mpf_arr.first().unwrap();
		let addr_in = d.add_constant_comb(vec![Sig::Id(0)], vec![6]);
		let data_lamp = d.add_lamp((Sig::Id(1), Dop::Equal, Sig::Constant(2000)));
		d.connect_red(addr_in, port.addr_wire);
		d.add_wire_red(vec![port.data], vec![data_lamp]);

		let mut p = PhysicalDesign::new();
		let mut s = SerializableDesign::new();
		p.build_from(&d);
		s.build_from(&p, &d);
		let blueprint_json = serde_json::to_string(&s).unwrap();
		println!("\n{}\n", blueprint_json);
	}

	#[test]
	fn rom_dense_large() {
		let mut data = vec![0, 1];
		for i in 2..47 {
			data.push(data[i - 1] + data[i - 2]);
		}

		let mut d = LogicalDesign::new();
		let mpf_arr = d.add_rom(
			vec![MemoryReadPort {
				addr: Sig::Id(0),
				data: Sig::Id(1),
				clk: None,
				en: None,
				rst: ResetSpec::Disabled,
				transparent: false,
			}],
			data,
			Some(10),
		);
		assert_eq!(mpf_arr.len(), 1);
		let port = mpf_arr.first().unwrap();
		let addr_in = d.add_constant_comb(vec![Sig::Id(0)], vec![6]);
		let data_lamp = d.add_lamp((Sig::Id(1), Dop::NotEqual, Sig::Constant(0)));
		d.connect_red(addr_in, port.addr_wire);
		d.add_wire_red(vec![port.data], vec![data_lamp]);

		let mut p = PhysicalDesign::new();
		let mut s = SerializableDesign::new();
		p.build_from(&d);
		s.build_from(&p, &d);
		let blueprint_json = serde_json::to_string(&s).unwrap();
		println!("\n{}\n", blueprint_json);
	}

	#[test]
	fn get_connected_combs() {
		// (_0_)----(_1_)----(_3_)
		//           |
		//          (_2_)
		let mut d = LogicalDesign::new();
		let nop0 = d.add_nop(Signal::Each, Signal::Each);
		let nop1 = d.add_nop(Signal::Each, Signal::Each);
		let nop2 = d.add_nop(Signal::Each, Signal::Each);
		let nop3 = d.add_nop(Signal::Each, Signal::Each);
		d.add_wire_red(vec![], vec![nop1, nop2]);
		d.add_wire_red(vec![nop0], vec![nop1]);
		d.add_wire_red(vec![nop1], vec![nop3]);
		let c0 = d.get_connected_combs(nop0);
		let c1 = d.get_connected_combs(nop1);
		let c2 = d.get_connected_combs(nop2);
		let c3 = d.get_connected_combs(nop3);
		assert_eq!(c0, vec![nop1]);
		assert!(c1.contains(&nop0) && c1.contains(&nop2) && c1.contains(&nop3) && c1.len() == 3);
		assert_eq!(c2, vec![nop1]);
		assert_eq!(c3, vec![nop1]);
	}

	#[test]
	fn named_signals() {
		let mut d = LogicalDesign::new();
		for i in 0..signal_lookup_table::n_ids() {
			let id = d.add_nop(Signal::Id(i), Signal::Id(i));
			d.set_description_node(id, format!("Sigid: {i}"));
		}
		let mut p = PhysicalDesign::new();
		let mut s = SerializableDesign::new();
		p.build_from(&d);
		s.build_from(&p, &d);
		let blueprint_json = serde_json::to_string(&s).unwrap();
		println!("\n{}\n", blueprint_json);
	}

	#[test]
	fn ram() {
		let mut d = LogicalDesign::new();
		let sig_red = Sig::Id(signal_lookup_table::lookup_id("signal-red").unwrap());
		let (rd_ports, wr_ports) = d.add_ram(
			vec![MemoryReadPort {
				addr: Sig::Id(0),
				data: Sig::Id(1),
				clk: Some(Sig::Id(2)),
				en: Some(Sig::Id(3)),
				rst: ResetSpec::Disabled,
				transparent: false,
			}],
			vec![
				MemoryWritePort {
					addr: Sig::Id(4),
					data: Sig::Id(5),
					clk: sig_red,
					en: Some(Sig::Id(6)),
				},
				MemoryWritePort {
					addr: Sig::Id(7),
					data: Sig::Id(8),
					clk: sig_red,
					en: Some(Sig::Id(9)),
				},
			],
			1,
		);

		// Setup read side.
		let read_port = d.add_constant_comb(
			vec![Sig::Id(0), Sig::Id(1), Sig::Id(2), Sig::Id(3)],
			vec![0, 0, 0, 0],
		);
		d.set_description_node(read_port, "Read port".to_owned());
		let read_lamp = d.add_lamp((Sig::Id(1), DeciderOperator::NotEqual, Sig::Constant(0)));
		d.connect_red(read_port, rd_ports[0].addr_wire);
		d.connect_red(read_port, rd_ports[0].en_wire.unwrap());
		d.connect_red(read_port, rd_ports[0].clk_wire.unwrap());
		d.add_wire_red_simple(rd_ports[0].data, read_lamp);

		// Setup write0 side.
		let write0_port =
			d.add_constant_comb(vec![Sig::Id(4), Sig::Id(5), Sig::Id(6)], vec![0, 0, 0]);
		d.connect_red(write0_port, wr_ports[0].addr_wire);

		// Setup write1 side.
		let write1_port =
			d.add_constant_comb(vec![Sig::Id(8), Sig::Id(9), Sig::Id(10)], vec![0, 0, 0]);

		d.connect_red(write1_port, wr_ports[0].addr_wire);

		// CLK
		let clk = d.add_constant_comb(vec![sig_red], vec![1]);
		d.connect_red(clk, wr_ports[0].clk_wire.unwrap());

		let mut p = PhysicalDesign::new();
		let mut s = SerializableDesign::new();
		p.build_from(&d);
		s.build_from(&p, &d);
		let blueprint_json = serde_json::to_string(&s).unwrap();
		println!("\n{}\n", blueprint_json);
	}

	#[test]
	fn nop_to_lamp() {
		let mut d = LogicalDesign::new();
		let nop = d.add_nop(Sig::Id(0), Sig::Id(0));
		let lamp = d.add_lamp((Sig::None, DeciderOperator::Equal, Sig::None));
		d.add_wire_red_simple(nop, lamp);
		let mut p = PhysicalDesign::new();
		let mut s = SerializableDesign::new();
		p.build_from(&d);
		s.build_from(&p, &d);
		let blueprint_json = serde_json::to_string(&s).unwrap();
		println!("\n{}\n", blueprint_json);
	}

	#[test]
	fn sim_dff() {
		const STEPS: usize = 5;
		let logd = Rc::new(RefCell::new(LogicalDesign::new()));

		let sig_data = signal_lookup_table::lookup_sig("signal-D");
		let sig_clk = signal_lookup_table::lookup_sig("signal-C");
		let sig_q = signal_lookup_table::lookup_sig("signal-Q");

		let (data_c, clock_c, comb_out) = {
			let mut logd = logd.borrow_mut();
			let (wire_data, wire_clk, comb_out) = logd.add_dff(sig_data, sig_clk, sig_q);
			let c1 = logd.add_constant_comb(vec![sig_data], vec![0]);
			let c2 = logd.add_constant_comb(vec![sig_clk], vec![0]);
			logd.connect_red(c1, wire_data);
			logd.connect_red(c2, wire_clk);
			logd.set_description_node(comb_out, "dff_q".to_owned());
			logd.set_description_node(c1, "data".to_owned());
			logd.set_description_node(c2, "clock".to_owned());
			(c1, c2, comb_out)
		};
		let mut sim = SimState::new(logd.clone());
		sim.add_trace(data_c);
		sim.add_trace(clock_c);
		sim.add_trace(comb_out);
		assert_eq!(sim.probe_red_out_sparse(data_c), vec![]);
		assert_eq!(sim.probe_red_out_sparse(clock_c), vec![]);
		assert_eq!(sim.probe_red_out_sparse(comb_out), vec![]);
		{
			let mut logd = logd.borrow_mut();
			logd.set_ith_output_count(data_c, 0, 100);
		}
		sim.step(STEPS);
		assert_eq!(sim.probe_red_out_sparse(data_c), vec![(sig_data.id(), 100)]);
		assert_eq!(sim.probe_red_out_sparse(clock_c), vec![]);
		assert_eq!(sim.probe_red_out_sparse(comb_out), vec![]);
		{
			let mut logd = logd.borrow_mut();
			logd.set_ith_output_count(clock_c, 0, 1);
		}
		sim.step(STEPS);
		assert_eq!(sim.probe_red_out_sparse(data_c), vec![(sig_data.id(), 100)]);
		assert_eq!(sim.probe_red_out_sparse(clock_c), vec![(sig_clk.id(), 1)]);
		assert_eq!(sim.probe_red_out_sparse(comb_out), vec![(sig_q.id(), 100)]);
		{
			let mut logd = logd.borrow_mut();
			logd.set_ith_output_count(data_c, 0, 200);
			logd.set_ith_output_count(clock_c, 0, 0);
		}
		sim.step(STEPS);
		assert_eq!(sim.probe_red_out_sparse(data_c), vec![(sig_data.id(), 200)]);
		assert_eq!(sim.probe_red_out_sparse(clock_c), vec![]);
		assert_eq!(sim.probe_red_out_sparse(comb_out), vec![(sig_q.id(), 100)]);
		{
			let mut logd = logd.borrow_mut();
			logd.set_ith_output_count(data_c, 0, 300);
		}
		sim.step(STEPS);
		assert_eq!(sim.probe_red_out_sparse(data_c), vec![(sig_data.id(), 300)]);
		assert_eq!(sim.probe_red_out_sparse(clock_c), vec![]);
		assert_eq!(sim.probe_red_out_sparse(comb_out), vec![(sig_q.id(), 100)]);
		{
			let mut logd = logd.borrow_mut();
			logd.set_ith_output_count(clock_c, 0, 1);
		}
		sim.step(STEPS);
		assert_eq!(sim.probe_red_out_sparse(data_c), vec![(sig_data.id(), 300)]);
		assert_eq!(sim.probe_red_out_sparse(clock_c), vec![(sig_clk.id(), 1)]);
		assert_eq!(sim.probe_red_out_sparse(comb_out), vec![(sig_q.id(), 300)]);
		let traces = sim.render_traces();
		traces.save("svg/sim_dff_traces.svg").unwrap();
	}

	#[test]
	fn sim_dffe() {
		const STEPS: usize = 6;
		let logd = Rc::new(RefCell::new(LogicalDesign::new()));

		let sig_data = signal_lookup_table::lookup_sig("signal-D");
		let sig_clk = signal_lookup_table::lookup_sig("signal-C");
		let sig_en = signal_lookup_table::lookup_sig("signal-E");
		let sig_q = signal_lookup_table::lookup_sig("signal-Q");

		let (data_c, clock_c, en_c, comb_out) = {
			let mut logd = logd.borrow_mut();
			let (wire_data, wire_clk, wire_en, comb_out) =
				logd.add_dffe(sig_data, sig_clk, sig_en, sig_q);
			let c1 = logd.add_constant_comb(vec![sig_data], vec![0]);
			let c2 = logd.add_constant_comb(vec![sig_clk], vec![0]);
			let c3 = logd.add_constant_comb(vec![sig_en], vec![0]);
			logd.connect_red(c1, wire_data);
			logd.connect_red(c2, wire_clk);
			logd.connect_red(c3, wire_en);
			logd.set_description_node(comb_out, "dff_q".to_owned());
			logd.set_description_node(c1, "data".to_owned());
			logd.set_description_node(c2, "clock".to_owned());
			logd.set_description_node(c3, "enable".to_owned());
			(c1, c2, c3, comb_out)
		};
		let mut sim = SimState::new(logd.clone());
		sim.add_trace(data_c);
		sim.add_trace(clock_c);
		sim.add_trace(en_c);
		sim.add_trace(comb_out);
		assert_eq!(sim.probe_red_out_sparse(data_c), vec![]);
		assert_eq!(sim.probe_red_out_sparse(clock_c), vec![]);
		assert_eq!(sim.probe_red_out_sparse(en_c), vec![]);
		assert_eq!(sim.probe_red_out_sparse(comb_out), vec![]);
		{
			let mut logd = logd.borrow_mut();
			logd.set_ith_output_count(data_c, 0, 100);
			logd.set_ith_output_count(en_c, 0, 1);
		}
		sim.step(STEPS);
		assert_eq!(sim.probe_red_out_sparse(data_c), vec![(sig_data.id(), 100)]);
		assert_eq!(sim.probe_red_out_sparse(clock_c), vec![]);
		assert_eq!(sim.probe_red_out_sparse(comb_out), vec![]);
		{
			let mut logd = logd.borrow_mut();
			logd.set_ith_output_count(clock_c, 0, 1);
		}
		sim.step(STEPS);
		assert_eq!(sim.probe_red_out_sparse(data_c), vec![(sig_data.id(), 100)]);
		assert_eq!(sim.probe_red_out_sparse(clock_c), vec![(sig_clk.id(), 1)]);
		assert_eq!(sim.probe_red_out_sparse(comb_out), vec![(sig_q.id(), 100)]);
		{
			let mut logd = logd.borrow_mut();
			logd.set_ith_output_count(data_c, 0, 200);
			logd.set_ith_output_count(clock_c, 0, 0);
		}
		sim.step(STEPS);
		assert_eq!(sim.probe_red_out_sparse(data_c), vec![(sig_data.id(), 200)]);
		assert_eq!(sim.probe_red_out_sparse(clock_c), vec![]);
		assert_eq!(sim.probe_red_out_sparse(comb_out), vec![(sig_q.id(), 100)]);
		{
			let mut logd = logd.borrow_mut();
			logd.set_ith_output_count(data_c, 0, 300);
		}
		sim.step(STEPS);
		assert_eq!(sim.probe_red_out_sparse(data_c), vec![(sig_data.id(), 300)]);
		assert_eq!(sim.probe_red_out_sparse(clock_c), vec![]);
		assert_eq!(sim.probe_red_out_sparse(comb_out), vec![(sig_q.id(), 100)]);
		{
			let mut logd = logd.borrow_mut();
			logd.set_ith_output_count(clock_c, 0, 1);
		}
		sim.step(STEPS);
		assert_eq!(sim.probe_red_out_sparse(data_c), vec![(sig_data.id(), 300)]);
		assert_eq!(sim.probe_red_out_sparse(clock_c), vec![(sig_clk.id(), 1)]);
		assert_eq!(sim.probe_red_out_sparse(comb_out), vec![(sig_q.id(), 300)]);
		// Now for disabled
		{
			let mut logd = logd.borrow_mut();
			logd.set_ith_output_count(data_c, 0, 100);
			logd.set_ith_output_count(en_c, 0, 0);
			logd.set_ith_output_count(clock_c, 0, 0);
		}
		sim.step(STEPS);
		assert_eq!(sim.probe_red_out_sparse(data_c), vec![(sig_data.id(), 100)]);
		assert_eq!(sim.probe_red_out_sparse(clock_c), vec![]);
		assert_eq!(sim.probe_red_out_sparse(comb_out), vec![(sig_q.id(), 300)]);
		{
			let mut logd = logd.borrow_mut();
			logd.set_ith_output_count(clock_c, 0, 1);
		}
		sim.step(STEPS);
		assert_eq!(sim.probe_red_out_sparse(data_c), vec![(sig_data.id(), 100)]);
		assert_eq!(sim.probe_red_out_sparse(clock_c), vec![(sig_clk.id(), 1)]);
		assert_eq!(sim.probe_red_out_sparse(comb_out), vec![(sig_q.id(), 300)]);
		{
			let mut logd = logd.borrow_mut();
			logd.set_ith_output_count(data_c, 0, 200);
			logd.set_ith_output_count(clock_c, 0, 0);
		}
		sim.step(STEPS);
		assert_eq!(sim.probe_red_out_sparse(data_c), vec![(sig_data.id(), 200)]);
		assert_eq!(sim.probe_red_out_sparse(clock_c), vec![]);
		assert_eq!(sim.probe_red_out_sparse(comb_out), vec![(sig_q.id(), 300)]);
		{
			let mut logd = logd.borrow_mut();
			logd.set_ith_output_count(data_c, 0, 300);
		}
		sim.step(STEPS);
		assert_eq!(sim.probe_red_out_sparse(data_c), vec![(sig_data.id(), 300)]);
		assert_eq!(sim.probe_red_out_sparse(clock_c), vec![]);
		assert_eq!(sim.probe_red_out_sparse(comb_out), vec![(sig_q.id(), 300)]);
		{
			let mut logd = logd.borrow_mut();
			logd.set_ith_output_count(clock_c, 0, 1);
		}
		sim.step(STEPS);
		assert_eq!(sim.probe_red_out_sparse(data_c), vec![(sig_data.id(), 300)]);
		assert_eq!(sim.probe_red_out_sparse(clock_c), vec![(sig_clk.id(), 1)]);
		assert_eq!(sim.probe_red_out_sparse(comb_out), vec![(sig_q.id(), 300)]);
		let traces = sim.render_traces();
		traces.save("svg/sim_dffe_traces.svg").unwrap();
	}
}

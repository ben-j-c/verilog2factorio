use std::{
	fs::File,
	io::BufReader,
	ops::Rem,
	sync::{Arc, RwLock},
};

use itertools::Itertools;

use crate::{
	checked_design::CheckedDesign, connected_design::CoarseExpr, logical_design::*,
	mapped_design::MappedDesign, phy::PhysicalDesign, serializable_design::SerializableDesign,
	signal_lookup_table, sim::SimState,
};

#[cfg(test)]
pub(crate) fn get_simple_logical_design() -> LogicalDesign {
	use ArithmeticOperator as Aop;
	use DeciderOperator as Dop;
	use Signal as Sig;
	let mut d = LogicalDesign::new();
	let constant1 = d.add_constant(vec![Sig::Id(0)], vec![100]);
	let constant2 = d.add_constant(vec![Sig::Id(1)], vec![4]);
	let mult = d.add_arithmetic((Sig::Id(1), Aop::Mult, Sig::Id(0)), Sig::Id(10));
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
		constants.push(d.add_constant(vec![(Sig::Id(i))], vec![i + 1]));
	}
	let mut mults = vec![];
	for i in 0..10 {
		mults.push(d.add_arithmetic(
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
			clk_polarity: Polarity::Positive,
		}],
		data,
		None,
	);
	assert_eq!(mpf_arr.len(), 1);
	let port = mpf_arr.first().unwrap();
	let addr = d.add_constant(vec![Signal::Id(0)], vec![6]);
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
			clk_polarity: Polarity::Positive,
		}],
		data,
		Some(256),
	);
	assert_eq!(mpf_arr.len(), 1);
	let port = mpf_arr.first().unwrap();
	let addr = d.add_constant(vec![Signal::Id(0)], vec![6]);
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
	use std::sync::{Arc, RwLock};

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
		let constant = d.add_constant(vec![Sig::Id(2)], vec![100]);
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
		let counter = d.add_arithmetic((Sig::Id(0), Aop::Add, Sig::Constant(0)), Sig::Id(0));
		let filter_pre = d.add_arithmetic((Sig::Id(0), Aop::Mult, Sig::Constant(1)), Sig::Id(0));
		let filter_post = d.add_arithmetic((Sig::Id(0), Aop::Mult, Sig::Constant(1)), Sig::Id(0));
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
		let a = d.add_arithmetic((Sig::Id(10), Aop::Add, Sig::Constant(0)), Sig::Id(10));
		let b = d.add_arithmetic((Sig::Id(10), Aop::Add, Sig::Constant(0)), Sig::Id(10));
		let c = d.add_arithmetic((Sig::Id(10), Aop::Add, Sig::Constant(0)), Sig::Id(10));
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
		let a = d.add_arithmetic((Sig::Id(10), Aop::Add, Sig::Constant(0)), Sig::Id(10));
		let b = d.add_arithmetic((Sig::Id(10), Aop::Add, Sig::Constant(0)), Sig::Id(10));
		let c = d.add_arithmetic((Sig::Id(10), Aop::Add, Sig::Constant(0)), Sig::Id(10));
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
		let c1 = d.add_constant(vec![Signal::Id(0)], vec![0]);
		let c2 = d.add_constant(vec![Signal::Id(1)], vec![1]);
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
		let c1 = d.add_constant(vec![Signal::Id(0)], vec![0]);
		let c2 = d.add_constant(vec![Signal::Id(1)], vec![1]);
		let l1 = d.add_lamp((Signal::Id(2), Dop::NotEqual, Signal::Constant(0)));
		d.connect_red(c1, wire_data);
		d.connect_green(c2, wire_clk);
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
				clk_polarity: Polarity::Positive,
			}],
			data,
			None,
		);
		assert_eq!(mpf_arr.len(), 1);
		let port = mpf_arr.first().unwrap();
		let addr = d.add_constant(vec![Sig::Id(0)], vec![6]);
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
				clk_polarity: Polarity::Positive,
			}],
			data,
			Some(10),
		);
		assert_eq!(mpf_arr.len(), 1);
		let port = mpf_arr.first().unwrap();
		let addr_in = d.add_constant(vec![Sig::Id(0)], vec![6]);
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
				clk_polarity: Polarity::Positive,
			}],
			data,
			Some(10),
		);
		assert_eq!(mpf_arr.len(), 1);
		let port = mpf_arr.first().unwrap();
		let addr_in = d.add_constant(vec![Sig::Id(0)], vec![6]);
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
				clk_polarity: Polarity::Positive,
			}],
			vec![
				MemoryWritePort {
					addr: Sig::Id(4),
					data: Sig::Id(5),
					clk: sig_red,
					en: Some(Sig::Id(6)),
					clk_polarity: Polarity::Positive,
				},
				MemoryWritePort {
					addr: Sig::Id(7),
					data: Sig::Id(8),
					clk: sig_red,
					en: Some(Sig::Id(9)),
					clk_polarity: Polarity::Positive,
				},
			],
			1,
		);

		// Setup read side.
		let read_port = d.add_constant(
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
		let write0_port = d.add_constant(vec![Sig::Id(4), Sig::Id(5), Sig::Id(6)], vec![0, 0, 0]);
		d.connect_red(write0_port, wr_ports[0].addr_wire);

		// Setup write1 side.
		let write1_port = d.add_constant(vec![Sig::Id(8), Sig::Id(9), Sig::Id(10)], vec![0, 0, 0]);

		d.connect_red(write1_port, wr_ports[0].addr_wire);

		// CLK
		let clk = d.add_constant(vec![sig_red], vec![1]);
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
		let nop = d.add_nop(signal_lookup_table::lookup_sig("signal-Y"), Sig::Id(0));
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
		const STEPS: u32 = 5;
		let logd = Arc::new(RwLock::new(LogicalDesign::new()));

		let sig_data = signal_lookup_table::lookup_sig("signal-D");
		let sig_clk = signal_lookup_table::lookup_sig("signal-C");
		let sig_q = signal_lookup_table::lookup_sig("signal-Q");

		let (data_c, clock_c, comb_out) = {
			let mut logd = logd.write().unwrap();
			let (wire_data, wire_clk, comb_out) = logd.add_dff(sig_data, sig_clk, sig_q);
			let c1 = logd.add_constant(vec![sig_data], vec![0]);
			let c2 = logd.add_constant(vec![sig_clk], vec![0]);
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
		assert_eq!(sim.probe_red_out(data_c), vec![]);
		assert_eq!(sim.probe_red_out(clock_c), vec![]);
		assert_eq!(sim.probe_red_out(comb_out), vec![]);
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(data_c, 0, 100);
		}
		sim.step(STEPS);
		assert_eq!(sim.probe_red_out(data_c), vec![(sig_data.id(), 100)]);
		assert_eq!(sim.probe_red_out(clock_c), vec![]);
		assert_eq!(sim.probe_red_out(comb_out), vec![]);
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(clock_c, 0, 1);
		}
		sim.step(STEPS);
		assert_eq!(sim.probe_red_out(data_c), vec![(sig_data.id(), 100)]);
		assert_eq!(sim.probe_red_out(clock_c), vec![(sig_clk.id(), 1)]);
		assert_eq!(sim.probe_red_out(comb_out), vec![(sig_q.id(), 100)]);
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(data_c, 0, 200);
			logd.set_ith_output_count(clock_c, 0, 0);
		}
		sim.step(STEPS);
		assert_eq!(sim.probe_red_out(data_c), vec![(sig_data.id(), 200)]);
		assert_eq!(sim.probe_red_out(clock_c), vec![]);
		assert_eq!(sim.probe_red_out(comb_out), vec![(sig_q.id(), 100)]);
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(data_c, 0, 300);
		}
		sim.step(STEPS);
		assert_eq!(sim.probe_red_out(data_c), vec![(sig_data.id(), 300)]);
		assert_eq!(sim.probe_red_out(clock_c), vec![]);
		assert_eq!(sim.probe_red_out(comb_out), vec![(sig_q.id(), 100)]);
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(clock_c, 0, 1);
		}
		sim.step(STEPS);
		assert_eq!(sim.probe_red_out(data_c), vec![(sig_data.id(), 300)]);
		assert_eq!(sim.probe_red_out(clock_c), vec![(sig_clk.id(), 1)]);
		assert_eq!(sim.probe_red_out(comb_out), vec![(sig_q.id(), 300)]);
		let traces = sim.render_traces();
		traces.save("svg/sim_dff_traces.svg").unwrap();
	}

	#[test]
	fn sim_dffe() {
		const STEPS: u32 = 6;
		let logd = Arc::new(RwLock::new(LogicalDesign::new()));

		let sig_data = signal_lookup_table::lookup_sig("signal-D");
		let sig_clk = signal_lookup_table::lookup_sig("signal-C");
		let sig_en = signal_lookup_table::lookup_sig("signal-E");
		let sig_q = signal_lookup_table::lookup_sig("signal-Q");

		let (data_c, clock_c, en_c, comb_out) = {
			let mut logd = logd.write().unwrap();
			let (wire_data, wire_clk, wire_en, comb_out) =
				logd.add_dffe(sig_data, sig_clk, sig_en, sig_q);
			let c1 = logd.add_constant(vec![sig_data], vec![0]);
			let c2 = logd.add_constant(vec![sig_clk], vec![0]);
			let c3 = logd.add_constant(vec![sig_en], vec![0]);
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
		assert_eq!(sim.probe_red_out(data_c), vec![]);
		assert_eq!(sim.probe_red_out(clock_c), vec![]);
		assert_eq!(sim.probe_red_out(en_c), vec![]);
		assert_eq!(sim.probe_red_out(comb_out), vec![]);
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(data_c, 0, 100);
			logd.set_ith_output_count(en_c, 0, 1);
		}
		sim.step(STEPS);
		assert_eq!(sim.probe_red_out(data_c), vec![(sig_data.id(), 100)]);
		assert_eq!(sim.probe_red_out(clock_c), vec![]);
		assert_eq!(sim.probe_red_out(comb_out), vec![]);
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(clock_c, 0, 1);
		}
		sim.step(STEPS);
		assert_eq!(sim.probe_red_out(data_c), vec![(sig_data.id(), 100)]);
		assert_eq!(sim.probe_red_out(clock_c), vec![(sig_clk.id(), 1)]);
		assert_eq!(sim.probe_red_out(comb_out), vec![(sig_q.id(), 100)]);
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(data_c, 0, 200);
			logd.set_ith_output_count(clock_c, 0, 0);
		}
		sim.step(STEPS);
		assert_eq!(sim.probe_red_out(data_c), vec![(sig_data.id(), 200)]);
		assert_eq!(sim.probe_red_out(clock_c), vec![]);
		assert_eq!(sim.probe_red_out(comb_out), vec![(sig_q.id(), 100)]);
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(data_c, 0, 300);
		}
		sim.step(STEPS);
		assert_eq!(sim.probe_red_out(data_c), vec![(sig_data.id(), 300)]);
		assert_eq!(sim.probe_red_out(clock_c), vec![]);
		assert_eq!(sim.probe_red_out(comb_out), vec![(sig_q.id(), 100)]);
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(clock_c, 0, 1);
		}
		sim.step(STEPS);
		assert_eq!(sim.probe_red_out(data_c), vec![(sig_data.id(), 300)]);
		assert_eq!(sim.probe_red_out(clock_c), vec![(sig_clk.id(), 1)]);
		assert_eq!(sim.probe_red_out(comb_out), vec![(sig_q.id(), 300)]);
		// Now for disabled
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(data_c, 0, 100);
			logd.set_ith_output_count(en_c, 0, 0);
			logd.set_ith_output_count(clock_c, 0, 0);
		}
		sim.step(STEPS);
		assert_eq!(sim.probe_red_out(data_c), vec![(sig_data.id(), 100)]);
		assert_eq!(sim.probe_red_out(clock_c), vec![]);
		assert_eq!(sim.probe_red_out(comb_out), vec![(sig_q.id(), 300)]);
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(clock_c, 0, 1);
		}
		sim.step(STEPS);
		assert_eq!(sim.probe_red_out(data_c), vec![(sig_data.id(), 100)]);
		assert_eq!(sim.probe_red_out(clock_c), vec![(sig_clk.id(), 1)]);
		assert_eq!(sim.probe_red_out(comb_out), vec![(sig_q.id(), 300)]);
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(data_c, 0, 200);
			logd.set_ith_output_count(clock_c, 0, 0);
		}
		sim.step(STEPS);
		assert_eq!(sim.probe_red_out(data_c), vec![(sig_data.id(), 200)]);
		assert_eq!(sim.probe_red_out(clock_c), vec![]);
		assert_eq!(sim.probe_red_out(comb_out), vec![(sig_q.id(), 300)]);
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(data_c, 0, 300);
		}
		sim.step(STEPS);
		assert_eq!(sim.probe_red_out(data_c), vec![(sig_data.id(), 300)]);
		assert_eq!(sim.probe_red_out(clock_c), vec![]);
		assert_eq!(sim.probe_red_out(comb_out), vec![(sig_q.id(), 300)]);
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(clock_c, 0, 1);
		}
		sim.step(STEPS);
		assert_eq!(sim.probe_red_out(data_c), vec![(sig_data.id(), 300)]);
		assert_eq!(sim.probe_red_out(clock_c), vec![(sig_clk.id(), 1)]);
		assert_eq!(sim.probe_red_out(comb_out), vec![(sig_q.id(), 300)]);
		let traces = sim.render_traces();
		traces.save("svg/sim_dffe_traces.svg").unwrap();
	}
}

#[test]
fn sim_dffe() {
	const NO_SIGNAL: Vec<(i32, i32)> = vec![];
	const STEPS: u32 = 10;
	let logd = Arc::new(RwLock::new(LogicalDesign::new()));

	let sig_data = signal_lookup_table::lookup_sig("signal-D");
	let sig_clk = signal_lookup_table::lookup_sig("signal-C");
	let sig_rst = signal_lookup_table::lookup_sig("signal-R");
	let sig_en = signal_lookup_table::lookup_sig("signal-E");
	let sig_q = signal_lookup_table::lookup_sig("signal-Q");

	let (data_c, clock_c, rst_c, en_c, q_out) = {
		let mut logd = logd.write().unwrap();
		let (wire_data, wire_clk, wire_rst, wire_en, comb_out) =
			logd.add_sdffe(sig_data, sig_clk, sig_rst, sig_en, sig_q);
		let c1 = logd.add_constant(vec![sig_data], vec![0]);
		let c2 = logd.add_constant(vec![sig_clk], vec![0]);
		let c3 = logd.add_constant(vec![sig_rst], vec![0]);
		let c4 = logd.add_constant(vec![sig_en], vec![0]);
		logd.connect_red(c1, wire_data);
		logd.connect_red(c2, wire_clk);
		logd.connect_red(c3, wire_rst);
		logd.connect_red(c4, wire_en);
		logd.set_description_node(comb_out, "dff_q".to_owned());
		logd.set_description_node(c1, "data".to_owned());
		logd.set_description_node(c2, "clock".to_owned());
		logd.set_description_node(c2, "sync_reset".to_owned());
		logd.set_description_node(c4, "enable".to_owned());
		(c1, c2, c3, c4, comb_out)
	};
	let mut sim = SimState::new(logd.clone());
	sim.add_trace(data_c);
	sim.add_trace(clock_c);
	sim.add_trace(rst_c);
	sim.add_trace(en_c);
	sim.add_trace(q_out);
	assert_eq!(sim.probe_red_out(q_out), NO_SIGNAL);
	{
		let mut logd = logd.write().unwrap();
		logd.set_ith_output_count(data_c, 0, 100);
		logd.set_ith_output_count(en_c, 0, 1);
	}
	sim.step(STEPS);
	assert_eq!(sim.probe_red_out(q_out), NO_SIGNAL);
	{
		let mut logd = logd.write().unwrap();
		logd.set_ith_output_count(clock_c, 0, 1);
	}
	sim.step(STEPS);
	assert_eq!(sim.probe_red_out(q_out), vec![(sig_q.id(), 100)]);
	{
		let mut logd = logd.write().unwrap();
		logd.set_ith_output_count(data_c, 0, 200);
		logd.set_ith_output_count(clock_c, 0, 0);
	}
	sim.step(STEPS);
	assert_eq!(sim.probe_red_out(q_out), vec![(sig_q.id(), 100)]);
	{
		let mut logd = logd.write().unwrap();
		logd.set_ith_output_count(data_c, 0, 300);
	}
	sim.step(STEPS);
	assert_eq!(sim.probe_red_out(q_out), vec![(sig_q.id(), 100)]);
	{
		let mut logd = logd.write().unwrap();
		logd.set_ith_output_count(clock_c, 0, 1);
	}
	sim.step(STEPS);
	assert_eq!(sim.probe_red_out(q_out), vec![(sig_q.id(), 300)]);

	// Now for disabled

	{
		let mut logd = logd.write().unwrap();
		logd.set_ith_output_count(data_c, 0, 100);
		logd.set_ith_output_count(en_c, 0, 0);
		logd.set_ith_output_count(clock_c, 0, 0);
	}
	sim.step(STEPS);
	assert_eq!(sim.probe_red_out(q_out), vec![(sig_q.id(), 300)]);
	{
		let mut logd = logd.write().unwrap();
		logd.set_ith_output_count(clock_c, 0, 1);
	}
	sim.step(STEPS);
	assert_eq!(sim.probe_red_out(q_out), vec![(sig_q.id(), 300)]);
	{
		let mut logd = logd.write().unwrap();
		logd.set_ith_output_count(data_c, 0, 200);
		logd.set_ith_output_count(clock_c, 0, 0);
	}
	sim.step(STEPS);
	assert_eq!(sim.probe_red_out(q_out), vec![(sig_q.id(), 300)]);
	{
		let mut logd = logd.write().unwrap();
		logd.set_ith_output_count(data_c, 0, 300);
	}
	sim.step(STEPS);
	assert_eq!(sim.probe_red_out(q_out), vec![(sig_q.id(), 300)]);
	{
		let mut logd = logd.write().unwrap();
		logd.set_ith_output_count(clock_c, 0, 1);
	}
	sim.step(STEPS);
	assert_eq!(sim.probe_red_out(q_out), vec![(sig_q.id(), 300)]);

	// Now with reset
	{
		let mut logd = logd.write().unwrap();
		logd.set_ith_output_count(clock_c, 0, 0);
		logd.set_ith_output_count(rst_c, 0, 1); // Set reset
		logd.set_ith_output_count(data_c, 0, 555);
	}
	sim.step(STEPS);
	assert_eq!(sim.probe_red_out(q_out), vec![(sig_q.id(), 300)]);
	{
		let mut logd = logd.write().unwrap();
		logd.set_ith_output_count(clock_c, 0, 1);
	}
	sim.step(STEPS);
	assert_eq!(sim.probe_red_out(q_out), NO_SIGNAL);
	{
		let mut logd = logd.write().unwrap();
		logd.set_ith_output_count(clock_c, 0, 0);
		logd.set_ith_output_count(en_c, 0, 1);
	}
	sim.step(STEPS);
	assert_eq!(sim.probe_red_out(q_out), NO_SIGNAL);
	{
		let mut logd = logd.write().unwrap();
		logd.set_ith_output_count(clock_c, 0, 1);
	}
	sim.step(STEPS);
	assert_eq!(sim.probe_red_out(q_out), NO_SIGNAL);

	// Now turn off reset
	{
		let mut logd = logd.write().unwrap();
		logd.set_ith_output_count(clock_c, 0, 0);
		logd.set_ith_output_count(rst_c, 0, 0); // Clear reset
		logd.set_ith_output_count(data_c, 0, 1234);
	}
	sim.step(STEPS);
	assert_eq!(sim.probe_red_out(q_out), NO_SIGNAL);
	{
		let mut logd = logd.write().unwrap();
		logd.set_ith_output_count(clock_c, 0, 1);
	}
	sim.step(STEPS);
	assert_eq!(sim.probe_red_out(q_out), vec![(sig_q.id(), 1234)]);
	{
		let mut logd = logd.write().unwrap();
		logd.set_ith_output_count(clock_c, 0, 0);
		logd.set_ith_output_count(rst_c, 0, 1); // Set reset
	}
	sim.step(STEPS);
	assert_eq!(sim.probe_red_out(q_out), vec![(sig_q.id(), 1234)]);
	{
		let mut logd = logd.write().unwrap();
		logd.set_ith_output_count(clock_c, 0, 1);
	}
	sim.step(STEPS);
	assert_eq!(sim.probe_red_out(q_out), NO_SIGNAL);

	let traces = sim.render_traces();
	traces.save("svg/sim_sdffe_traces.svg").unwrap();
}

#[test]
fn test10() {
	let file = File::open("./test_designs/output/test10.json").unwrap();
	let reader = BufReader::new(file);
	let mapped_design: MappedDesign = serde_json::from_reader(reader).unwrap();
	let mut checked_design = CheckedDesign::new();
	let mut logical_design = LogicalDesign::new();
	let mut physical_design = PhysicalDesign::new();
	let mut serializable_design = SerializableDesign::new();
	checked_design.build_from(&mapped_design);
	logical_design.build_from(&checked_design, &mapped_design);
	physical_design.build_from(&logical_design);
	let _ = physical_design.save_svg(&logical_design, "svg/test10.svg");
	serializable_design.build_from(&physical_design, &logical_design);
	let blueprint_json = serde_json::to_string(&serializable_design).unwrap();
	println!("\n{}", blueprint_json);
}

#[test]
fn sim_srl() {
	let mut logd = LogicalDesign::new();
	let a = logd.add_constant(vec![Signal::Id(0)], vec![-1]);
	let b = logd.add_constant(vec![Signal::Id(1)], vec![16]);
	let (ab_wire, y) = logd.add_srl(&[Signal::Id(0), Signal::Id(1)], Signal::Id(2));
	let lamp = logd.add_lamp((
		Signal::Id(2),
		DeciderOperator::Equal,
		Signal::Constant(0xffff),
	));
	logd.connect_red(a, ab_wire[0]);
	logd.connect_red(b, ab_wire[1]);
	let outp_wire = logd.add_wire_red_simple(y, lamp);
	let logd = Arc::new(RwLock::new(logd));
	let mut sim = SimState::new(logd.clone());
	sim.step(3);
	let outp = sim.probe_lamp_state(lamp);
	assert_eq!(outp, Some(true));
	assert_eq!(sim.probe_red_out(outp_wire), vec![(2, 0xffff)]);
}

#[test]
/// An example test from the a riscv decoder
fn swizzle_riscv_branch() {
	let opcode: i32 = 1910063075;
	let excted_closed_form = (opcode & 0b111_1111) + ((opcode >> 5) & 0b11_1000_0000);
	let expected = 483;
	assert_eq!(excted_closed_form, expected);
	let mut logd = LogicalDesign::new();
	let instruction = logd.add_constant(vec![Signal::Id(0)], vec![opcode]);
	let (swizzle_wires, swizzle_comb) = logd.add_swizzle(
		vec![Signal::Id(0), Signal::Id(0)],
		vec![
			Some(CoarseExpr::DriverChunk {
				driver_ioid: 0,
				shift: 0,
				bit_start: 0,
				bit_end: 7,
			}),
			Some(CoarseExpr::DriverChunk {
				driver_ioid: 0,
				shift: -5,
				bit_start: 7,
				bit_end: 10,
			}),
		],
		Signal::Id(1),
	);
	let lamp = logd.add_lamp((
		Signal::Id(1),
		DeciderOperator::Equal,
		Signal::Constant(expected),
	));
	logd.connect_red(instruction, swizzle_wires[0]);
	logd.connect_red(instruction, swizzle_wires[1]);
	let outp_wire = logd.add_wire_red_simple(swizzle_comb, lamp);

	let mut sim = SimState::new(Arc::new(RwLock::new(logd)));
	sim.step(3);
	let outp = sim.probe_lamp_state(lamp);
	assert_eq!(sim.probe_red_out(outp_wire), vec![(1, expected)]);
	assert_eq!(outp, Some(true));
}

#[test]
fn simple_counter() {
	let mut logd = LogicalDesign::new();
	let sig_arst = Signal::Id(0);
	let sig_clk = Signal::Id(1);
	let sig_q = Signal::Id(3);
	let sig_data = Signal::Id(4);
	let reset = logd.add_constant(vec![sig_arst], vec![0]);
	let clock = logd.add_constant(vec![sig_clk], vec![0]);
	let lamp_q = logd.add_lamp((sig_q, DeciderOperator::NotEqual, Signal::Constant(-1)));

	let adder = logd.add_arithmetic(
		(sig_q, ArithmeticOperator::Add, Signal::Constant(1)),
		sig_data,
	);

	let (wire_data, wire_clk, wire_arst, comb_q, _loopback) =
		logd.add_adff_isolated(sig_data, sig_clk, sig_arst, sig_q, 0, false, false);

	logd.connect_red(adder, wire_data);
	logd.connect_red(clock, wire_clk);
	logd.connect_red(reset, wire_arst);
	logd.add_wire_red_simple(comb_q, lamp_q);
	logd.add_wire_red_simple(comb_q, adder);

	let logd = Arc::new(RwLock::new(logd));
	let mut sim = SimState::new(logd.clone());
	let del = 4;
	sim.step(del);
	for i in 0..100 {
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(clock, 0, 1);
		}
		sim.step(del);
		assert_eq!(sim.probe_lamp_state(lamp_q), Some(true));
		assert_eq!(sim.probe_red_out(comb_q), vec![(sig_q.id(), i + 1)]);
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(clock, 0, 0);
		}

		sim.step(del);
	}
	{
		let mut logd = logd.write().unwrap();
		logd.set_ith_output_count(reset, 0, 1);
	}
	sim.step(del);
	assert_eq!(sim.probe_red_out(comb_q), vec![]);
	{
		let mut logd = logd.write().unwrap();
		logd.set_ith_output_count(reset, 0, 0);
	}
	sim.step(del);
	for i in 0..100 {
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(clock, 0, 1);
		}
		sim.step(del);
		assert_eq!(sim.probe_lamp_state(lamp_q), Some(true));
		assert_eq!(sim.probe_red_out(comb_q), vec![(sig_q.id(), i + 1)]);
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(clock, 0, 0);
		}

		sim.step(del);
	}
}

#[test]
fn simple_counter_adffe() {
	let mut logd = LogicalDesign::new();
	let sig_arst = Signal::Id(0);
	let sig_clk = Signal::Id(1);
	let sig_en = Signal::Id(2);
	let sig_q = Signal::Id(3);
	let sig_data = Signal::Id(4);
	let reset = logd.add_constant(vec![sig_arst], vec![0]);
	let clock = logd.add_constant(vec![sig_clk], vec![0]);
	let en = logd.add_constant(vec![sig_en], vec![1]);
	let lamp_q = logd.add_lamp((sig_q, DeciderOperator::NotEqual, Signal::Constant(-1)));

	let adder = logd.add_arithmetic(
		(sig_q, ArithmeticOperator::Add, Signal::Constant(1)),
		sig_data,
	);

	let (wire_data, wire_clk, wire_en, wire_arst, comb_q) = logd.add_adffe(
		sig_data, sig_clk, sig_en, sig_arst, sig_q, 0, false, false, false,
	);

	logd.connect_red(adder, wire_data);
	logd.connect_red(clock, wire_clk);
	logd.connect_red(en, wire_en);
	logd.connect_red(reset, wire_arst);
	logd.add_wire_red_simple(comb_q, lamp_q);
	logd.add_wire_red_simple(comb_q, adder);

	let logd = Arc::new(RwLock::new(logd));
	let mut sim = SimState::new(logd.clone());
	let del = 5;
	sim.step(del);
	for i in 0..100 {
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(clock, 0, 1);
		}
		sim.step(del);
		assert_eq!(sim.probe_lamp_state(lamp_q), Some(true));
		assert_eq!(sim.probe_red_out(comb_q), vec![(sig_q.id(), i + 1)]);
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(clock, 0, 0);
		}

		sim.step(del);
	}
	{
		let mut logd = logd.write().unwrap();
		logd.set_ith_output_count(reset, 0, 1);
	}
	sim.step(del);
	assert_eq!(sim.probe_red_out(comb_q), vec![]);
	{
		let mut logd = logd.write().unwrap();
		logd.set_ith_output_count(reset, 0, 0);
	}
	sim.step(del);
	for i in 0..100 {
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(clock, 0, 1);
		}
		sim.step(del);
		assert_eq!(sim.probe_lamp_state(lamp_q), Some(true));
		assert_eq!(sim.probe_red_out(comb_q), vec![(sig_q.id(), i + 1)]);
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(clock, 0, 0);
		}

		sim.step(del);
	}
	{
		let mut logd = logd.write().unwrap();
		logd.set_ith_output_count(reset, 0, 1);
	}
	sim.step(del);
	{
		let mut logd = logd.write().unwrap();
		logd.set_ith_output_count(reset, 0, 0);
	}
	sim.step(del);
	for i in 0..100 {
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(clock, 0, 1);
		}
		sim.step(del);
		assert_eq!(sim.probe_lamp_state(lamp_q), Some(true));
		assert_eq!(sim.probe_red_out(comb_q), vec![(sig_q.id(), i / 2 + 1)]);
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(clock, 0, 0);
			logd.set_ith_output_count(en, 0, i.rem(2));
		}

		sim.step(del);
	}
	{
		let mut logd = logd.write().unwrap();
		logd.set_ith_output_count(reset, 0, 1);
	}
	sim.step(del);
	assert_eq!(sim.probe_red_out(comb_q), vec![]);
	{
		let mut logd = logd.write().unwrap();
		logd.set_ith_output_count(reset, 0, 0);
	}
	sim.step(del);
	for i in 0..100 {
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(clock, 0, 1);
		}
		sim.step(del);
		assert_eq!(sim.probe_lamp_state(lamp_q), Some(true));
		assert_eq!(sim.probe_red_out(comb_q), vec![(sig_q.id(), i + 1)]);
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(clock, 0, 0);
		}

		sim.step(del);
	}
}

#[test]
fn byte_select() {
	let mut logd = LogicalDesign::new();

	let d1 = logd.add_constant(vec![Signal::Id(0)], vec![0x11223344]);
	let d0 = logd.add_constant(vec![Signal::Id(1)], vec![0xAABBCCDDu32 as i32]);
	let select = logd.add_constant(vec![Signal::Id(2)], vec![0]);
	let lamp = logd.add_lamp((
		Signal::Id(0),
		DeciderOperator::NotEqual,
		Signal::Constant(0),
	));

	let (i1_comb, i0_comb, sel, data_comb) = logd.add_byte_select_write(Signal::Id(2));
	logd.add_wire_red_simple(d1, i1_comb);
	logd.add_wire_red_simple(d0, i0_comb);
	logd.add_wire_red_simple(select, sel);
	logd.add_wire_red_simple(data_comb, lamp);

	{
		let mut phy = PhysicalDesign::new();
		phy.build_from(&logd);
		let mut serd = SerializableDesign::new();
		serd.build_from(&phy, &logd);
		let blueprint_json = serde_json::to_string(&serd).unwrap();
		println!("\n{}", blueprint_json);
	}

	let logd = Arc::new(RwLock::new(logd));
	let mut sim = SimState::new(logd.clone());

	for i in 0..16 {
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(select, 0, i);
		}
		sim.step(5);
		sim.step(5);
		let val = sim.probe_input(lamp, NET_RED_GREEN);
		assert_eq!(val.len(), 1);
		assert_eq!(val[0].0, 0);
		let val = val[0].1;
		let b0 = val & 0xff;
		let b1 = (val & 0xff00) >> 8;
		let b2 = (val & 0xff0000) >> 16;
		let b3 = (val as u32 & 0xff000000) >> 24;
		if i & 1 > 0 {
			assert_eq!(b0, 0x44);
		} else {
			assert_eq!(b0, 0xDD);
		}
		if i & 2 > 0 {
			assert_eq!(b1, 0x33);
		} else {
			assert_eq!(b1, 0xCC);
		}
		if i & 4 > 0 {
			assert_eq!(b2, 0x22);
		} else {
			assert_eq!(b2, 0xBB);
		}
		if i & 8 > 0 {
			assert_eq!(b3, 0x11);
		} else {
			assert_eq!(b3, 0xAA);
		}
	}
}

#[test]
fn print() {
	println!("{}", LogicalDesign::new());
	println!("{:?}", Signal::Id(123));
	println!("{}", Signal::Constant(123));
	println!("{}", Signal::Anything);
	println!("{}", Signal::Each);
	println!("{}", Signal::Everything);
	println!("{}", Signal::None);
}

#[test]
fn parser1() {
	let mut logd = LogicalDesign::new();
	let decider = logd.add_decider();
	logd.set_decider_inputs(
		decider,
		"
		signal-a[] != signal-b[G]
		|| red-wire[RG] > 0
		|| green-wire < 0
				&& signal-0 >= Id(25)
				&& signal-k <= signal-dot",
	);
	let node = logd.get_node(decider);
	assert!(node.is_decider());
	let func = node.function.unwrap_decider();
	let a = signal_lookup_table::lookup_sig("signal-a");
	let b = signal_lookup_table::lookup_sig("signal-b");
	let red_wire = signal_lookup_table::lookup_sig("red-wire");
	let green_wire = signal_lookup_table::lookup_sig("green-wire");
	let signal_0 = signal_lookup_table::lookup_sig("signal-0");
	let signal_k = signal_lookup_table::lookup_sig("signal-k");
	let signal_dot = signal_lookup_table::lookup_sig("signal-dot");
	use DeciderOperator::*;
	assert_eq!(
		func.0,
		&vec![
			(a, NotEqual, b),
			(red_wire, GreaterThan, Signal::Constant(0)),
			(green_wire, LessThan, Signal::Constant(0)),
			(signal_0, GreaterThanEqual, Signal::Id(25)),
			(signal_k, LessThanEqual, signal_dot),
		]
	);
	use DeciderRowConjDisj::*;
	assert_eq!(func.1, &vec![FirstRow, Or, Or, And, And]);
}

#[test]
fn demux() {
	const DENSITY: i32 = 4;
	let mut logd = LogicalDesign::new();
	let addr = signal_lookup_table::lookup_sig("signal-a");
	let byte_select = signal_lookup_table::lookup_sig("signal-b");

	let lamp = logd.add_lamp((
		Signal::Anything,
		DeciderOperator::NotEqual,
		Signal::Constant(0),
	));
	let addr_const = logd.add_constant(vec![addr], vec![0]);
	let byte_select_const = logd.add_constant(vec![byte_select], vec![15]);
	let v_data = 0x77AABBCC;
	let data_const = logd.add_constant(vec![Signal::Id(0)], vec![v_data]);

	let demux = logd.add_demux_memory_with_byte_select(addr, byte_select, DENSITY as usize);
	println!("{:?}", demux);
	logd.add_wire_red_simple(demux.row_in, lamp);

	let row_out = logd.add_constant(
		(0..DENSITY).map(|i| Signal::Id(i)).collect_vec(),
		(0..DENSITY).map(|i| i + 10).collect_vec(),
	);
	logd.connect_red(row_out, demux.mux1_out_wire);

	logd.connect_green(addr_const, demux.addr_low_wire_g);
	logd.connect_red(byte_select_const, demux.byte_select_wire);
	logd.connect_red(data_const, demux.data_in_wire);

	{
		let mut phy = PhysicalDesign::new();
		phy.build_from(&logd);
		let mut serd = SerializableDesign::new();
		serd.build_from(&phy, &logd);
		let blueprint_json = serde_json::to_string(&serd).unwrap();
		println!("\n{}", blueprint_json);
	}

	let logd = Arc::new(RwLock::new(logd));
	let mut sim = SimState::new(logd.clone());
	sim.step(10);

	let masks: [u32; 16] = [
		0x00000000, 0x000000ff, 0x0000ff00, 0x0000ffff, 0x00ff0000, 0x00ff00ff, 0x00ffff00,
		0x00ffffff, 0xff000000, 0xff0000ff, 0xff00ff00, 0xff00ffff, 0xffff0000, 0xffff00ff,
		0xffffff00, 0xffffffff,
	];

	for addr_sel in 0..DENSITY {
		for b_sel in 0..16 {
			{
				let mut logd = logd.write().unwrap();
				logd.set_ith_output_count(addr_const, 0, addr_sel);
				logd.set_ith_output_count(byte_select_const, 0, b_sel);
			}
			sim.step(10);
			let val = sim.probe_input(lamp, NET_RED_GREEN);
			let v0 = (addr_sel + 10) as u32 & (masks[b_sel as usize] ^ 0xFFFFFFFF);
			let v1 = v_data as u32 & masks[b_sel as usize];
			assert_eq!(val.len(), DENSITY as usize);
			for (id, val_id) in val {
				if id == addr_sel {
					assert_eq!(val_id, (v0 | v1) as i32);
				} else {
					assert_eq!(val_id, id + 10);
				}
			}
		}
	}
}

//#[test]
fn ram_resetable_dense() {
	const DENSITY: i32 = 4;
	let mut logd = LogicalDesign::new();
	let arst_sig = signal_lookup_table::lookup_sig("signal-R");
	let clk_sig = signal_lookup_table::lookup_sig("signal-C");
	let bs_sig = signal_lookup_table::lookup_sig("signal-S");
	let rd_port = vec![MemoryReadPort {
		addr: todo!(),
		data: todo!(),
		clk: todo!(),
		clk_polarity: todo!(),
		en: todo!(),
		rst: todo!(),
		transparent: todo!(),
	}];
	let wr_port = MemoryWritePort {
		addr: todo!(),
		data: todo!(),
		clk: todo!(),
		en: todo!(),
		clk_polarity: todo!(),
	};
	let mem = logd.add_ram_resetable_dense(
		arst_sig,
		clk_sig,
		bs_sig,
		rd_port.clone(),
		wr_port,
		DENSITY as usize,
		(0..(DENSITY * 4)).collect_vec(),
	);
}

#[test]
fn adffe_program_mem_cell() {
	const DENSITY: i32 = 4;
	let mut logd = LogicalDesign::new();
	let clk_sig = signal_lookup_table::lookup_sig("signal-C");
	let en_sig = signal_lookup_table::lookup_sig("signal-E");
	let arst_sig = signal_lookup_table::lookup_sig("signal-R");

	use DeciderOperator as Dop;
	use Signal as Sig;
	let lamp = logd.add_lamp((Signal::Anything, Dop::NotEqual, Signal::Constant(0)));
	let data = logd.add_constant(
		vec![Sig::Id(0), Sig::Id(1), Sig::Id(2), Sig::Id(3)],
		vec![1, 2, 3, 4],
	);

	let clk = logd.add_constant(vec![clk_sig], vec![0]);
	let en = logd.add_constant(vec![en_sig], vec![0]);
	let arst = logd.add_constant(vec![arst_sig], vec![0]);

	let mcell = logd.add_adffe_program_mem_cell(
		clk_sig,
		en_sig,
		arst_sig,
		&(10..10 + DENSITY).collect_vec(),
		DENSITY as usize,
	);

	logd.add_wire_red_simple(mcell.row_out, lamp);
	logd.add_wire_red_simple(data, mcell.row_in_comb);

	{
		let clk_pos = logd.add_nop(clk_sig, clk_sig);
		let clk_neg = logd.add_neg(clk_sig, clk_sig);
		let arst_pos = logd.add_nop(arst_sig, arst_sig);
		let arst_neg = logd.add_nop(arst_sig, arst_sig);
		logd.add_wire_red(vec![clk], vec![clk_pos, clk_neg]);
		logd.add_wire_red(vec![arst], vec![arst_pos, arst_neg]);
		logd.add_wire_green(vec![arst_pos, clk_pos], vec![]);
		logd.add_wire_green(vec![arst_neg, clk_neg], vec![]);
		logd.add_wire_green_simple(clk_pos, mcell.latch_pos.in_comb);
		logd.add_wire_green_simple(clk_neg, mcell.latch_neg.in_comb);
		logd.connect_green(en, mcell.en_wire);
	}

	{
		let mut phy = PhysicalDesign::new();
		phy.build_from(&logd);
		let mut serd = SerializableDesign::new();
		serd.build_from(&phy, &logd);
		let blueprint_json = serde_json::to_string(&serd).unwrap();
		println!("\n{}", blueprint_json);
	}

	let logd = Arc::new(RwLock::new(logd));
	let mut sim = SimState::new(logd.clone());
	sim.step(10);

	let init_val = sim.probe_input(lamp, NET_RED_GREEN);
	assert_eq!(init_val.len(), 0);
	{
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(arst, 0, 1);
		}
		sim.step(3);
		let mut val = sim.probe_input(lamp, NET_RED_GREEN);
		val.sort();
		assert_eq!(val.len(), 4);
		assert_eq!(val, (0..DENSITY).map(|i| (i, i + 10)).collect_vec());
	}

	{
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(arst, 0, 0);
		}
		sim.step(3);
		let mut val = sim.probe_input(lamp, NET_RED_GREEN);
		val.sort();
		assert_eq!(val.len(), 4);
		assert_eq!(val, (0..DENSITY).map(|i| (i, i + 10)).collect_vec());
	}

	{
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(clk, 0, 1);
		}
		sim.step(3);
		let mut val = sim.probe_input(lamp, NET_RED_GREEN);
		val.sort();
		assert_eq!(val.len(), 4);
		assert_eq!(val, (0..DENSITY).map(|i| (i, i + 10)).collect_vec());
	}

	{
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(clk, 0, 0);
		}
		sim.step(3);
		let mut val = sim.probe_input(lamp, NET_RED_GREEN);
		val.sort();
		assert_eq!(val.len(), 4);
		assert_eq!(val, (0..DENSITY).map(|i| (i, i + 10)).collect_vec());
	}

	{
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(en, 0, 1);
		}
		sim.step(3);
		let mut val = sim.probe_input(lamp, NET_RED_GREEN);
		val.sort();
		assert_eq!(val.len(), 4);
		assert_eq!(val, (0..DENSITY).map(|i| (i, i + 10)).collect_vec());
	}

	{
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(clk, 0, 1);
		}
		sim.step(3);
		let mut val = sim.probe_input(lamp, NET_RED_GREEN);
		val.sort();
		assert_eq!(val.len(), 4);
		assert_eq!(val, (0..DENSITY).map(|i| (i, i + 1)).collect_vec());
	}

	{
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(clk, 0, 0);
		}
		sim.step(3);
		let mut val = sim.probe_input(lamp, NET_RED_GREEN);
		val.sort();
		assert_eq!(val.len(), 4);
		assert_eq!(val, (0..DENSITY).map(|i| (i, i + 1)).collect_vec());
	}

	{
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(en, 0, 0);
		}
		sim.step(3);
		let mut val = sim.probe_input(lamp, NET_RED_GREEN);
		val.sort();
		assert_eq!(val.len(), 4);
		assert_eq!(val, (0..DENSITY).map(|i| (i, i + 1)).collect_vec());
	}

	{
		{
			let mut logd = logd.write().unwrap();
			logd.set_ith_output_count(arst, 0, 1);
		}
		sim.step(3);
		let mut val = sim.probe_input(lamp, NET_RED_GREEN);
		val.sort();
		assert_eq!(val.len(), 4);
		assert_eq!(val, (0..DENSITY).map(|i| (i, i + 10)).collect_vec());
	}
}

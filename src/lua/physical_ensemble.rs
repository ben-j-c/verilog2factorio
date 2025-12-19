use std::{
	panic::{catch_unwind, AssertUnwindSafe},
	sync::RwLock,
};

use graphviz_rust::attributes::root;
use mlua::prelude::*;

use crate::{
	logical_design::{NodeId, Signal, WireColour},
	lua::{
		runtime_err, Arithmetic, ArithmeticExpression, Constant, Decider, DeciderExpression, Lamp,
		PhysicalDesignAPI, SimRef, SimStateAPI, Terminal, TerminalSide,
	},
	mapped_design::Direction,
	phy::PhyId,
	sim::SimState,
	util::HashM,
};

pub struct PhysicalEnsembleAPI {
	pub(crate) root: PhysicalDesignAPI,
	pub(crate) offsets_log: HashM<usize, (NodeId, usize)>,
	pub(crate) offsets_phy: HashM<usize, (PhyId, usize)>,
}

impl PhysicalEnsembleAPI {
	fn resolve_log_id(&self, id: NodeId, log_id: usize) -> Result<NodeId, ()> {
		if self.root.log_id == log_id {
			return Ok(id);
		}
		if self.offsets_log.contains_key(&log_id) {
			if id.0 < self.offsets_log[&log_id].1 {
				let offset = self.offsets_log[&log_id].0 .0;
				return Ok(NodeId(id.0 + offset));
			}
		}
		Err(())
	}

	fn resolve_phy_id(&self, id: PhyId, log_id: usize) -> Result<PhyId, ()> {
		if self.root.log_id == log_id {
			return Ok(id);
		}
		if self.offsets_log.contains_key(&log_id) {
			if id.0 < self.offsets_phy[&log_id].1 {
				let offset = self.offsets_phy[&log_id].0 .0;
				return Ok(PhyId(id.0 + offset));
			}
		}
		Err(())
	}

	fn connect(&mut self, lhs: &Terminal, rhs: &TerminalSide) -> Result<(), mlua::Error> {
		let (log_id, this_id, _this_logd, this_colour) = lhs.get();
		let (log_id_other, other_id, _other_logd) = rhs.get();
		let this_id = self
			.resolve_log_id(this_id, log_id)
			.map_err(|_| LuaError::runtime("Node isn't part of this design."))?;
		let other_id = self
			.resolve_log_id(other_id, log_id_other)
			.map_err(|_| LuaError::runtime("Node isn't part of this design."))?;
		let (res, this_dir, other_dir) = {
			let mut fanin = vec![];
			let mut fanout = vec![];
			let this_dir = match lhs.0 {
				TerminalSide::Input(..) => {
					fanout.push(this_id);
					Direction::Input
				},
				TerminalSide::Output(..) => {
					fanin.push(this_id);
					Direction::Output
				},
			};
			let other_dir = match *rhs {
				TerminalSide::Input(..) => {
					fanout.push(other_id);
					Direction::Input
				},
				TerminalSide::Output(..) => {
					fanin.push(other_id);
					Direction::Output
				},
			};
			(
				match this_colour {
					WireColour::Red => self
						.root
						.logd
						.write()
						.unwrap()
						.add_wire_red_safe(fanin, fanout),
					WireColour::Green => self
						.root
						.logd
						.write()
						.unwrap()
						.add_wire_green_safe(fanin, fanout),
				},
				this_dir,
				other_dir,
			)
		};
		res.map_err(|_| LuaError::runtime("Invalid connection."))?;
		let mut phyd = self.root.phyd.write().unwrap();
		phyd.route_single_net(this_id, this_dir, other_id, other_dir)
			.map_err(|_| LuaError::runtime("Failed to route new connection."))
	}
}

impl LuaUserData for PhysicalEnsembleAPI {
	fn add_methods<M: LuaUserDataMethods<Self>>(methods: &mut M) {
		methods.add_method_mut(
			"freeze_and_place",
			|_, this, (name, other, x, y): (String, PhysicalDesignAPI, usize, usize)| {
				let mut logd = this
					.root
					.logd
					.write()
					.map_err(|_| LuaError::runtime("Can only have one writer"))?;
				let mut phyd = this
					.root
					.phyd
					.write()
					.map_err(|_| LuaError::runtime("Can only have one writer"))?;
				let other_logd = other
					.logd
					.read()
					.map_err(|_| LuaError::runtime("Design being written to"))?;
				let other_phyd = other
					.phyd
					.read()
					.map_err(|_| LuaError::runtime("Design being written to"))?;
				let lim_logd = other_logd.nodes.len();
				let lim_phyd = other_phyd.n_nodes();
				let (offset_logd, offset_ports) = logd.extend(other_logd.clone(), &name);
				let offset_phy = phyd.extend(other_phyd.clone(), offset_logd, (x, y));
				let offset_phy = match offset_phy {
					Ok(v) => v,
					Err(_) => {
						logd.truncate(offset_logd, offset_ports);
						return runtime_err("Design overlaps with existing structure");
					},
				};
				this.offsets_log
					.insert(other.log_id, (NodeId(offset_logd), lim_logd));
				this.offsets_phy
					.insert(other.log_id, (PhyId(offset_phy), lim_phyd));
				Ok(())
			},
		);
		methods.add_method_mut(
			"connect",
			|_, this, args: (LuaAnyUserData, LuaAnyUserData)| {
				let lhs = args.0.borrow::<Terminal>()?;
				let rhs = args.1.borrow::<TerminalSide>()?;
				this.connect(&lhs, &rhs)?;
				Ok(())
			},
		);
		methods.add_method_mut("save_svg", |_, this, name: String| {
			this.root
				.phyd
				.read()
				.unwrap()
				.save_svg(&this.root.logd.read().unwrap(), name)?;
			Ok(())
		});
		methods.add_method("new_simulation", |_, this, _: ()| {
			Ok(SimStateAPI {
				log_id: this.root.log_id,
				logd: this.root.logd.clone(),
				sim: SimRef::new(RwLock::new(SimState::new(this.root.logd.clone()))),
			})
		});
		methods.add_method("find_out_port", |_, this, name: String| {
			Ok(this
				.root
				.logd
				.read()
				.unwrap()
				.get_out_port_node(name)
				.map(|id| Lamp {
					log_id: this.root.log_id,
					id,
					logd: this.root.logd.clone(),
				}))
		});
		methods.add_method("find_in_port", |_, this, name: String| {
			Ok(this
				.root
				.logd
				.read()
				.unwrap()
				.get_in_port_node(name)
				.map(|id| Constant {
					log_id: this.root.log_id,
					id,
					logd: this.root.logd.clone(),
				}))
		});
		methods.add_method("add_decider", |_, this, (x, y): (usize, usize)| {
			if !this
				.root
				.phyd
				.write()
				.unwrap()
				.check_free_make_space(x, y, 2, 1)
			{
				return Ok(None);
			}
			let mut logd = this.root.logd.write().unwrap();
			let id = logd.add_decider();
			this.root
				.phyd
				.write()
				.unwrap()
				.place_new_comb((x, y), &logd, id)
				.unwrap();
			Ok(Some(Decider {
				log_id: this.root.log_id,
				id,
				logd: this.root.logd.clone(),
			}))
		});
		methods.add_method(
			"add_arithmetic",
			|_,
			 this,
			 (x, y, expr, out, net_left_v, net_right_v): (
				usize,
				usize,
				LuaAnyUserData,
				Signal,
				i32,
				i32,
			)| {
				if !this
					.root
					.phyd
					.write()
					.unwrap()
					.check_free_make_space(x, y, 2, 1)
				{
					return Ok(None);
				}
				let expr = expr.borrow::<ArithmeticExpression>()?;
				let net_left = (net_left_v & 1 > 0, net_left_v & 2 > 0);
				let net_right = (net_right_v & 1 > 0, net_right_v & 2 > 0);
				let mut logd = this.root.logd.write().unwrap();
				let id = logd.add_arithmetic_with_net(
					(expr.0, expr.1, expr.2),
					out,
					net_left,
					net_right,
				);
				this.root
					.phyd
					.write()
					.unwrap()
					.place_new_comb((x, y), &logd, id)
					.unwrap();
				Ok(Some(Arithmetic {
					log_id: this.root.log_id,
					id,
					logd: this.root.logd.clone(),
				}))
			},
		);
		methods.add_method(
			"add_lamp",
			|_, this, args: (usize, usize, LuaAnyUserData)| {
				let (x, y) = (args.0, args.1);
				if !this
					.root
					.phyd
					.write()
					.unwrap()
					.check_free_make_space(x, y, 1, 1)
				{
					return Ok(None);
				}
				let expr = args.2.borrow::<DeciderExpression>()?;
				let mut logd = this.root.logd.write().unwrap();
				let id = logd.add_lamp((expr.0, expr.1, expr.2));
				this.root
					.phyd
					.write()
					.unwrap()
					.place_new_comb((x, y), &logd, id)
					.unwrap();
				Ok(Some(Lamp {
					log_id: this.root.log_id,
					id,
					logd: this.root.logd.clone(),
				}))
			},
		);
		methods.add_method(
			"add_constant",
			|_, this, (x, y, sigs, counts): (usize, usize, Vec<Signal>, Vec<i32>)| {
				if sigs.len() != counts.len() {
					return runtime_err("Mismatched length.");
				}
				if !this
					.root
					.phyd
					.write()
					.unwrap()
					.check_free_make_space(x, y, 1, 1)
				{
					return Ok(None);
				}
				let mut logd = this.root.logd.write().unwrap();
				let id = logd.add_constant(sigs, counts);
				this.root
					.phyd
					.write()
					.unwrap()
					.place_new_comb((x, y), &logd, id)
					.unwrap();
				Ok(Some(Constant {
					log_id: this.root.log_id,
					id,
					logd: this.root.logd.clone(),
				}))
			},
		);
	}
}

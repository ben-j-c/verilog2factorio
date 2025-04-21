use std::{
	cell::RefCell,
	panic::{catch_unwind, AssertUnwindSafe},
	rc::Rc,
};

use mlua::{AnyUserData, Error, Lua, MetaMethod, UserData, UserDataFields, UserDataMethods};

use crate::{
	logical_design::{
		ArithmeticOperator, DeciderOperator, DeciderRowConjDisj, LogicalDesign, NodeId, Signal,
		WireColour,
	},
	signal_lookup_table,
};

struct LuaLogicalDesign {
	logd: Rc<RefCell<LogicalDesign>>,
}

#[derive(Clone, Debug)]
enum TerminalSide {
	Input(NodeId, Rc<RefCell<LogicalDesign>>),
	Output(NodeId, Rc<RefCell<LogicalDesign>>),
}

struct DeciderExpression(Signal, DeciderOperator, Signal);

impl UserData for DeciderExpression {}

struct ArithmeticExpression(Signal, ArithmeticOperator, Signal);

impl UserData for ArithmeticExpression {}

impl TerminalSide {
	fn get(&self) -> (NodeId, Rc<RefCell<LogicalDesign>>) {
		match self {
			TerminalSide::Input(node_id, ref_cell) => (*node_id, ref_cell.clone()),
			TerminalSide::Output(node_id, ref_cell) => (*node_id, ref_cell.clone()),
		}
	}
}

impl UserData for TerminalSide {
	fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
		fields.add_field_method_get("green", |_, this| {
			Ok(Terminal(this.clone(), WireColour::Green))
		});
		fields.add_field_method_get("red", |_, this| Ok(Terminal(this.clone(), WireColour::Red)));
	}
}

impl UserData for WireColour {}

impl UserData for Signal {
	fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
		methods.add_meta_method(MetaMethod::Eq, |_, lhs, rhs: AnyUserData| {
			let rhs = rhs.borrow()?;
			Ok(DeciderExpression(*lhs, DeciderOperator::Equal, *rhs))
		});
		methods.add_meta_method(MetaMethod::Le, |_, lhs, rhs: AnyUserData| {
			let rhs = rhs.borrow()?;
			if lhs.is_constant() {
				Ok(DeciderExpression(
					*rhs,
					DeciderOperator::GreaterThanEqual,
					*lhs,
				))
			} else {
				Ok(DeciderExpression(
					*lhs,
					DeciderOperator::LessThanEqual,
					*rhs,
				))
			}
		});
		methods.add_meta_method(MetaMethod::Lt, |_, lhs, rhs: AnyUserData| {
			let rhs = rhs.borrow()?;
			if lhs.is_constant() {
				Ok(DeciderExpression(*rhs, DeciderOperator::GreaterThan, *lhs))
			} else {
				Ok(DeciderExpression(*lhs, DeciderOperator::LessThan, *rhs))
			}
		});
		methods.add_method(MetaMethod::Add, |_, lhs, rhs: AnyUserData| {
			let rhs = rhs.borrow()?;
			Ok(ArithmeticExpression(*lhs, ArithmeticOperator::Add, *rhs))
		});
		methods.add_method(MetaMethod::Sub, |_, lhs, rhs: AnyUserData| {
			let rhs = rhs.borrow()?;
			Ok(ArithmeticExpression(*lhs, ArithmeticOperator::Sub, *rhs))
		});
		methods.add_method(MetaMethod::Mul, |_, lhs, rhs: AnyUserData| {
			let rhs = rhs.borrow()?;
			Ok(ArithmeticExpression(*lhs, ArithmeticOperator::Mult, *rhs))
		});
		methods.add_method(MetaMethod::Div, |_, lhs, rhs: AnyUserData| {
			let rhs = rhs.borrow()?;
			Ok(ArithmeticExpression(*lhs, ArithmeticOperator::Div, *rhs))
		});
		methods.add_method(MetaMethod::Mod, |_, lhs, rhs: AnyUserData| {
			let rhs = rhs.borrow()?;
			Ok(ArithmeticExpression(*lhs, ArithmeticOperator::Mod, *rhs))
		});
		methods.add_method(MetaMethod::Pow, |_, lhs, rhs: AnyUserData| {
			let rhs = rhs.borrow()?;
			Ok(ArithmeticExpression(*lhs, ArithmeticOperator::Exp, *rhs))
		});
		methods.add_method(MetaMethod::Shr, |_, lhs, rhs: AnyUserData| {
			let rhs = rhs.borrow()?;
			Ok(ArithmeticExpression(*lhs, ArithmeticOperator::Srl, *rhs))
		});
		methods.add_method(MetaMethod::Shl, |_, lhs, rhs: AnyUserData| {
			let rhs = rhs.borrow()?;
			Ok(ArithmeticExpression(*lhs, ArithmeticOperator::Sll, *rhs))
		});
		methods.add_method(MetaMethod::BAnd, |_, lhs, rhs: AnyUserData| {
			let rhs = rhs.borrow()?;
			Ok(ArithmeticExpression(*lhs, ArithmeticOperator::And, *rhs))
		});
		methods.add_method(MetaMethod::BOr, |_, lhs, rhs: AnyUserData| {
			let rhs = rhs.borrow()?;
			Ok(ArithmeticExpression(*lhs, ArithmeticOperator::Or, *rhs))
		});
		methods.add_method(MetaMethod::BXor, |_, lhs, rhs: AnyUserData| {
			let rhs = rhs.borrow()?;
			Ok(ArithmeticExpression(*lhs, ArithmeticOperator::Xor, *rhs))
		});
	}
}

#[derive(Debug, Clone)]
struct Terminal(TerminalSide, WireColour);

impl Terminal {
	fn get(&self) -> (NodeId, Rc<RefCell<LogicalDesign>>, WireColour) {
		let inner = self.0.get();
		(inner.0, inner.1, self.1)
	}
}

impl UserData for Terminal {
	fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
		methods.add_method("connect", |_, this, other: AnyUserData| {
			let other = other.borrow::<Terminal>()?;
			let (this_id, this_logd, this_colour) = this.get();
			let (other_id, other_logd, other_colour) = other.get();
			if this_logd.as_ptr() != other_logd.as_ptr() {
				return Err(Error::RuntimeError(
					"Can't connect two logical designs together".to_owned(),
				));
			}
			if this_colour != other_colour {
				return Err(Error::RuntimeError(
					"Differently coloured terminals together.".to_owned(),
				));
			}
			let res = catch_unwind(AssertUnwindSafe(|| {
				let mut fanin = vec![];
				let mut fanout = vec![];
				match this.0 {
					TerminalSide::Input(..) => fanout.push(this_id),
					TerminalSide::Output(..) => fanin.push(this_id),
				}
				match other.0 {
					TerminalSide::Input(..) => fanout.push(other_id),
					TerminalSide::Output(..) => fanin.push(other_id),
				}
				match this_colour {
					WireColour::Red => this_logd.borrow_mut().add_wire_red(fanin, fanout),
					WireColour::Green => this_logd.borrow_mut().add_wire_green(fanin, fanout),
				};
			}));
			if let Err(_) = res {
				Err(Error::RuntimeError("Invalid connection.".to_owned()))
			} else {
				Ok(res.unwrap())
			}
		});
	}
}

struct Decider {
	id: NodeId,
	logd: Rc<RefCell<LogicalDesign>>,
}

struct Arithmetic {
	id: NodeId,
	logd: Rc<RefCell<LogicalDesign>>,
}

struct Lamp {
	id: NodeId,
	logd: Rc<RefCell<LogicalDesign>>,
}

struct Constant {
	id: NodeId,
	logd: Rc<RefCell<LogicalDesign>>,
}

impl UserData for Decider {
	fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
		fields.add_field_method_get("input", |_, this| {
			Ok(TerminalSide::Input(this.id, this.logd.clone()))
		});
		fields.add_field_method_get("output", |_, this| {
			Ok(TerminalSide::Output(this.id, this.logd.clone()))
		});
	}

	fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
		methods.add_method(
			"add_condition",
			|_, this, args: (AnyUserData, AnyUserData, i32, i32)| {
				let row_op = args.0.borrow::<DeciderRowConjDisj>()?;
				let expr = args.1.borrow::<DeciderExpression>()?;
				let net_left = (args.2 & 1 > 0, args.2 & 2 > 0);
				let net_right = (args.3 & 1 > 0, args.3 & 2 > 0);
				let res = catch_unwind(AssertUnwindSafe(|| {
					this.logd.borrow_mut().add_decider_comb_input(
						this.id,
						(expr.0, expr.1, expr.2),
						row_op.clone(),
						net_left,
						net_right,
					);
				}));
				if let Err(_) = res {
					Err(Error::RuntimeError("Condition.".to_owned()))
				} else {
					Ok(())
				}
			},
		);
		methods.add_method("add_output", |_, this, args: (AnyUserData, bool, i32)| {
			let sig = args.0.borrow::<Signal>()?;
			let net_out = (args.2 & 1 > 0, args.2 & 2 > 0);
			let res = catch_unwind(AssertUnwindSafe(|| {
				this.logd
					.borrow_mut()
					.add_decider_comb_output(this.id, *sig, args.1, net_out);
			}));
			if let Err(_) = res {
				Err(Error::RuntimeError("Condition.".to_owned()))
			} else {
				Ok(())
			}
		});
	}
}

impl UserData for Arithmetic {
	fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
		fields.add_field_method_get("input", |_, this| {
			Ok(TerminalSide::Input(this.id, this.logd.clone()))
		});
		fields.add_field_method_get("output", |_, this| {
			Ok(TerminalSide::Output(this.id, this.logd.clone()))
		});
	}
}

impl UserData for Constant {
	fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
		fields.add_field_method_get("output", |_, this| {
			Ok(TerminalSide::Output(this.id, this.logd.clone()))
		});
	}
}

impl UserData for Lamp {
	fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
		fields.add_field_method_get("input", |_, this| {
			Ok(TerminalSide::Input(this.id, this.logd.clone()))
		});
	}
}

impl UserData for LuaLogicalDesign {
	fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
		fields.add_field_method_get("description", |_, this| {
			Ok(this.logd.borrow().description.clone())
		});
		fields.add_field_method_set("description", |_, this, description: String| {
			this.logd.borrow_mut().set_description(description);
			Ok(())
		});
	}

	fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
		methods.add_method("add_decider", |_, this, _: ()| {
			let id = this.logd.borrow_mut().add_decider_comb();
			Ok(Decider {
				id,
				logd: this.logd.clone(),
			})
		});
		methods.add_method(
			"add_arithmetic",
			|_, this, args: (AnyUserData, AnyUserData)| {
				let expr = args.0.borrow::<ArithmeticExpression>()?;
				let out = args.1.borrow::<Signal>()?;
				let id = this
					.logd
					.borrow_mut()
					.add_arithmetic_comb((expr.0, expr.1, expr.2), *out);
				Ok(Arithmetic {
					id,
					logd: this.logd.clone(),
				})
			},
		);
		methods.add_method("add_lamp", |_, this, args: (AnyUserData,)| {
			let expr = args.0.borrow::<DeciderExpression>()?;
			let id = this.logd.borrow_mut().add_lamp((expr.0, expr.1, expr.2));
			Ok(Lamp {
				id,
				logd: this.logd.clone(),
			})
		});
		methods.add_method(
			"add_constant",
			|_, this, args: (Vec<AnyUserData>, Vec<i32>)| {
				let mut sigs = vec![];
				for x in args.0 {
					sigs.push(*x.borrow::<Signal>()?);
				}
				if sigs.len() != args.1.len() {
					return Err(Error::RuntimeError("Mismatched length.".to_owned()));
				}
				let id = this.logd.borrow_mut().add_constant_comb(sigs, args.1);
				Ok(Constant {
					id,
					logd: this.logd.clone(),
				})
			},
		);
		methods.add_method("print", |_, this, _: ()| {
			println!("{}", this.logd.borrow());
			Ok(())
		});
	}
}

pub fn get_lua() -> Result<Lua, Error> {
	let lua = Lua::new();
	let get_named_signal = lua.create_function(|_, name: String| {
		match signal_lookup_table::lookup_sig_opt(name.as_str()) {
			Some(signal) => Ok(signal),
			None => Err(Error::RuntimeError("Not a real signal.".to_owned())),
		}
	})?;
	lua.globals().set("get_named_signal", get_named_signal)?;
	lua.globals().set("Each", Signal::Each)?;
	lua.globals().set("Anything", Signal::Anything)?;
	lua.globals().set("Everything", Signal::Everything)?;
	lua.globals().set(
		"Constant",
		lua.create_function(|_, val: i32| Ok(Signal::Constant(val)))?,
	)?;

	lua.globals().set("NET_NONE", 0)?;
	lua.globals().set("NET_RED", 1)?;
	lua.globals().set("NET_GREEN", 2)?;
	lua.globals().set("NET_REDGREEN", 3)?;

	lua.globals().set(
		"get_empty_design",
		lua.create_function(|_, _: ()| {
			Ok(LuaLogicalDesign {
				logd: Rc::new(RefCell::new(LogicalDesign::new())),
			})
		})?,
	)?;

	Ok(lua)
}

extern crate chumsky;

use chumsky::prelude::*;

use crate::{
	logical_design::{DeciderOperator, Signal},
	signal_lookup_table,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeciderExpr {
	pub left: Signal,
	pub op: DeciderOperator,
	pub right: Signal,
	pub net_left: (bool, bool),
	pub net_right: (bool, bool),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeciderConj {
	pub conj: Vec<DeciderExpr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeciderSop {
	pub disj: Vec<DeciderConj>,
}

pub fn parser<'src>() -> impl Parser<'src, &'src str, DeciderSop, extra::Err<Rich<'src, char>>> {
	let named_signal = {
		let base = text::ident();
		let suffix_word = text::ident();
		let suffix_digits = text::digits(10).to_slice();
		let suffix = just('-').ignore_then(suffix_word.or(suffix_digits));
		base.then(suffix.or_not())
			.map(
				|(base_str, suffix_opt): (&str, Option<&str>)| match suffix_opt {
					Some(suffix_str) => format!("{}-{}", base_str, suffix_str),
					None => base_str.to_owned(),
				},
			)
	};
	let sig_name = named_signal.padded().try_map(|name: String, span| {
		//
		if name == "each" {
			return Ok(Signal::Each);
		}
		if name == "everything" {
			return Ok(Signal::Everything);
		}
		if name == "anything" {
			return Ok(Signal::Anything);
		}
		if name == "none" {
			return Ok(Signal::None);
		}
		if let Some(id) = signal_lookup_table::lookup_id(&name) {
			return Ok(Signal::Id(id));
		}
		Err(chumsky::error::Rich::custom(
			span,
			format!("Unknown signal name: '{}'", name),
		))
	});

	let sig_id = just("Id(")
		.padded()
		.ignore_then(text::int(10).try_map(|v: &str, span| {
			let v = v.parse();
			match v {
				Ok(v) if v >= 0 || v < signal_lookup_table::n_ids() => Ok(Signal::Id(v)),
				_ => Err(chumsky::error::Rich::custom(
					span,
					format!("Not a valid id"),
				)),
			}
		}))
		.padded()
		.then_ignore(just(")"))
		.padded();

	let constant = text::int(10)
		.try_map(|v: &str, span| {
			//
			let v = v.parse();
			match v {
				Ok(v) => Ok(Signal::Constant(v)),
				Err(e) => Err(chumsky::error::Rich::custom(
					span,
					format!("{} is not a valid constant", e),
				)),
			}
		})
		.padded();

	let op_lt = just("<").padded().map(|_| DeciderOperator::LessThan);
	let op_gt = just(">").padded().map(|_| DeciderOperator::GreaterThan);
	let op_eq = just("==").padded().map(|_| DeciderOperator::Equal);
	let op_neq = just("!=").padded().map(|_| DeciderOperator::NotEqual);
	let op_geq = just(">=")
		.padded()
		.map(|_| DeciderOperator::GreaterThanEqual);
	let op_leq = just("<=").padded().map(|_| DeciderOperator::LessThanEqual);

	let red_green = just("RG")
		.map(|_| (true, true))
		.or(just("G").map(|_| (false, true)))
		.or(just("R").map(|_| (true, false)))
		.or(empty().map(|_| (false, false)))
		.padded();
	let net_decl = just("[")
		.padded()
		.then(red_green)
		.then(just("]").padded())
		.map(|x| x.0 .1)
		.or(empty().map(|_| (true, true)))
		.padded();

	let op = op_geq
		.or(op_leq)
		.or(op_lt)
		.or(op_gt)
		.or(op_eq)
		.or(op_neq)
		.padded();

	let operand = sig_id
		.then(net_decl)
		.or(sig_name.then(net_decl))
		.or(constant.map(|x| (x, (true, true))))
		.padded();

	let expr =
		operand
			.then(op)
			.then(operand)
			.map(|(((left, net_left), op), (right, net_right))| DeciderExpr {
				left,
				op,
				right,
				net_left,
				net_right,
			});

	let conj = expr
		.padded()
		.separated_by(just("&&"))
		.collect()
		.map(|x| DeciderConj { conj: x })
		.padded();

	let disj = conj
		.padded()
		.separated_by(just("||"))
		.collect()
		.map(|x| DeciderSop { disj: x })
		.padded();

	disj.then(end()).map(|x| x.0)
}

impl DeciderSop {
	pub fn distribute_conj(&self, term: &DeciderConj) -> DeciderSop {
		let mut ret = self.clone();
		for disj in ret.disj.iter_mut() {
			disj.and(term);
		}
		ret
	}

	pub fn and(&self, term: &Self) -> DeciderSop {
		let mut ret = Self { disj: vec![] };
		for conj1 in self.disj.iter() {
			for conj2 in term.disj.iter() {
				ret.disj.push(conj1.and(&conj2));
			}
		}
		ret
	}

	pub fn and_else_emplace(&self, term: &Self) -> DeciderSop {
		let mut ret = Self { disj: vec![] };
		if self.disj.is_empty() {
			return term.clone();
		}
		for conj1 in self.disj.iter() {
			for conj2 in term.disj.iter() {
				ret.disj.push(conj1.and(&conj2));
			}
		}
		ret
	}

	pub fn complement(&self) -> DeciderSop {
		if self.disj.is_empty() {
			return self.clone();
		}
		let mut ret = self.disj[0].complement();
		for term in self.disj.iter().skip(1) {
			let tmp = ret.and(&term.complement());
			ret = tmp;
		}
		ret
	}

	pub fn or(&self, term: &DeciderSop) -> DeciderSop {
		let mut ret = self.clone();
		ret.disj.extend(term.disj.iter().cloned());
		ret
	}

	pub fn replace_none(&self, repl: Signal) -> Self {
		let disj = self
			.disj
			.iter()
			.map(|x| x.replace_none(repl))
			.collect::<Vec<_>>();
		Self { disj }
	}

	pub fn simple(left: Signal, op: DeciderOperator, right: Signal) -> Self {
		DeciderSop {
			disj: vec![DeciderConj {
				conj: vec![DeciderExpr::from_args(left, op, right)],
			}],
		}
	}

	pub fn default() -> Self {
		Self { disj: vec![] }
	}
}

impl DeciderConj {
	pub fn and(&self, term: &DeciderConj) -> DeciderConj {
		let mut ret = self.clone();
		ret.conj.extend(term.conj.iter().cloned());
		ret
	}

	pub fn complement(&self) -> DeciderSop {
		let mut ret = DeciderSop { disj: vec![] };
		for expr in self.conj.iter() {
			let term = DeciderConj {
				conj: vec![expr.complement()],
			};
			ret.disj.push(term);
		}
		ret
	}

	pub fn replace_none(&self, repl: Signal) -> Self {
		let conj = self
			.conj
			.iter()
			.map(|x| x.sub_none(repl))
			.collect::<Vec<_>>();
		Self { conj }
	}
}

impl DeciderExpr {
	pub fn complement(&self) -> DeciderExpr {
		DeciderExpr {
			left: self.left,
			op: self.op.complement(),
			right: self.right,
			net_left: self.net_left,
			net_right: self.net_right,
		}
	}

	pub fn sub_none(&self, repl: Signal) -> DeciderExpr {
		let left = if self.left.is_none() {
			repl
		} else {
			self.left.clone()
		};
		let right = if self.right.is_none() {
			repl
		} else {
			self.right.clone()
		};
		DeciderExpr {
			left,
			op: self.op,
			right,
			net_left: self.net_left,
			net_right: self.net_right,
		}
	}

	pub fn from_args(left: Signal, op: DeciderOperator, right: Signal) -> Self {
		Self {
			left,
			op,
			right,
			net_left: (true, true),
			net_right: (true, true),
		}
	}
}

extern crate chumsky;

use chumsky::prelude::*;
use itertools::Itertools;

use crate::{
	logical_design::{DeciderOperator, Signal},
	signal_lookup_table,
	util::hash_set,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct DeciderExpr {
	pub left: Signal,
	pub op: DeciderOperator,
	pub right: Signal,
	pub net_left: (bool, bool),
	pub net_right: (bool, bool),
	pub left_placeholder: Option<usize>,
	pub right_placeholder: Option<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct DeciderConj {
	pub conj: Vec<DeciderExpr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
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
				left_placeholder: None,
				right_placeholder: None,
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
		let mut disj_set = hash_set();
		for conj1 in self.disj.iter() {
			for conj2 in term.disj.iter() {
				disj_set.insert(conj1.and(&conj2));
			}
		}
		DeciderSop {
			disj: disj_set.into_iter().sorted().collect_vec(),
		}
	}

	pub fn and_else_emplace(&self, term: &Self) -> DeciderSop {
		let mut ret = Self { disj: vec![] };
		if self.disj.is_empty() {
			return term.cannon();
		}
		for conj1 in self.disj.iter() {
			for conj2 in term.disj.iter() {
				ret = ret.or(&&DeciderSop {
					disj: vec![conj1.and(&conj2)],
				});
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
		ret.cannon()
	}

	pub fn or(&self, term: &DeciderSop) -> DeciderSop {
		let mut ret = self.clone();
		ret.disj.extend(term.disj.iter().cloned());
		ret.cannon()
	}

	pub fn increment_placeholders(&self, count: usize) -> Self {
		let disj = self
			.disj
			.iter()
			.map(|x| x.increment_placeholders(count))
			.collect::<Vec<_>>();
		Self { disj }
	}

	pub fn replace_placeholders(&self, repl: &[Signal]) -> Self {
		if repl.is_empty() {
			return self.clone();
		}
		let ret = self.replace_placeholder(*repl.last().unwrap(), repl.len() - 1);
		ret.replace_placeholders(&repl[..repl.len() - 1])
	}

	pub fn replace_placeholder(&self, repl: Signal, ident: usize) -> Self {
		let disj = self
			.disj
			.iter()
			.map(|x| x.replace_placeholder(repl, ident))
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

	pub fn placeholder(
		left: Signal,
		op: DeciderOperator,
		right: Signal,
		left_placeholder: Option<usize>,
		right_placeholder: Option<usize>,
	) -> Self {
		DeciderSop {
			disj: vec![DeciderConj {
				conj: vec![DeciderExpr::with_placeholders(
					left,
					op,
					right,
					left_placeholder,
					right_placeholder,
				)],
			}],
		}
	}

	pub fn default() -> Self {
		Self { disj: vec![] }
	}

	pub fn num_terms(&self) -> usize {
		self.disj.iter().map(|x| x.conj.len()).sum::<usize>() + self.disj.len()
	}

	pub fn cannon(&self) -> DeciderSop {
		let mut disj_set = hash_set();
		for disj in self.disj.iter() {
			disj_set.insert(disj.cannon());
		}
		DeciderSop {
			disj: disj_set.into_iter().sorted().collect_vec(),
		}
	}
}

impl DeciderConj {
	pub fn and(&self, other: &DeciderConj) -> DeciderConj {
		let a = self.cannon();
		let b = other.cannon();
		let mut exprs = hash_set();
		for expr in a.conj.iter() {
			exprs.insert(expr);
		}
		for expr in b.conj.iter() {
			exprs.insert(expr);
		}
		DeciderConj {
			conj: exprs.into_iter().sorted().cloned().collect_vec(),
		}
	}

	pub fn complement(&self) -> DeciderSop {
		let mut ret = DeciderSop { disj: vec![] };
		for expr in self.conj.iter() {
			let term = DeciderConj {
				conj: vec![expr.complement()],
			};
			ret.disj.push(term);
		}
		ret.cannon()
	}

	pub fn replace_placeholder(&self, repl: Signal, ident: usize) -> Self {
		let conj = self
			.conj
			.iter()
			.map(|x| x.sub_placeholder(repl, ident))
			.collect::<Vec<_>>();
		Self { conj }.cannon()
	}

	fn increment_placeholders(&self, count: usize) -> DeciderConj {
		let conj = self
			.conj
			.iter()
			.map(|x| x.increment_placeholder(count))
			.collect::<Vec<_>>();
		Self { conj }
	}

	fn cannon(&self) -> DeciderConj {
		let mut conj_set = hash_set();
		for expr in self.conj.iter() {
			conj_set.insert(expr);
		}
		DeciderConj {
			conj: conj_set.into_iter().sorted().cloned().collect_vec(),
		}
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
			left_placeholder: self.left_placeholder,
			right_placeholder: self.right_placeholder,
		}
	}

	pub fn sub_placeholder(&self, repl: Signal, ident: usize) -> DeciderExpr {
		let (left, left_placeholder) = if self.left_placeholder == Some(ident) {
			(repl, None)
		} else {
			(self.left.clone(), self.left_placeholder)
		};
		let (right, right_placeholder) = if self.right_placeholder == Some(ident) {
			(repl, None)
		} else {
			(self.right.clone(), self.right_placeholder)
		};
		DeciderExpr {
			left,
			op: self.op,
			right,
			net_left: self.net_left,
			net_right: self.net_right,
			left_placeholder,
			right_placeholder,
		}
	}

	pub fn from_args(left: Signal, op: DeciderOperator, right: Signal) -> Self {
		Self {
			left,
			op,
			right,
			net_left: (true, true),
			net_right: (true, true),
			left_placeholder: None,
			right_placeholder: None,
		}
	}

	pub fn with_placeholders(
		left: Signal,
		op: DeciderOperator,
		right: Signal,
		left_placeholder: Option<usize>,
		right_placeholder: Option<usize>,
	) -> Self {
		Self {
			left,
			op,
			right,
			net_left: (true, true),
			net_right: (true, true),
			left_placeholder,
			right_placeholder,
		}
	}

	fn increment_placeholder(&self, count: usize) -> DeciderExpr {
		let left_placeholder = if let Some(ident) = self.left_placeholder {
			Some(ident + count)
		} else {
			self.left_placeholder
		};
		let right_placeholder = if let Some(ident) = self.right_placeholder {
			Some(ident + count)
		} else {
			self.right_placeholder
		};
		DeciderExpr {
			left: self.left,
			op: self.op,
			right: self.right,
			net_left: self.net_left,
			net_right: self.net_right,
			left_placeholder,
			right_placeholder,
		}
	}
}

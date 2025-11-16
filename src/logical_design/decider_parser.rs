extern crate chumsky;

use chumsky::prelude::*;

use crate::{
	logical_design::{DeciderOperator, Signal},
	signal_lookup_table,
};

#[derive(Debug, Clone, PartialEq)]
pub struct DeciderExpr {
	pub left: Signal,
	pub op: DeciderOperator,
	pub right: Signal,
	pub net_left: (bool, bool),
	pub net_right: (bool, bool),
}

#[derive(Debug, Clone, PartialEq)]
pub struct DeciderConj {
	pub conj: Vec<DeciderExpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DeciderDisj {
	pub disj: Vec<DeciderConj>,
}

pub fn parser<'src>() -> impl Parser<'src, &'src str, DeciderDisj, extra::Err<Rich<'src, char>>> {
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
		.map(|x| DeciderDisj { disj: x })
		.padded();

	disj.then(end()).map(|x| x.0)
}

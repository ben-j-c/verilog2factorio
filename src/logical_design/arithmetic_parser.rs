extern crate chumsky;

use chumsky::prelude::*;

use crate::{
	logical_design::{ArithmeticOperator, Signal},
	signal_lookup_table,
};

#[derive(Debug, Clone, PartialEq)]
pub enum ArithExpr {
	Expr {
		left: Box<Self>,
		op: ArithmeticOperator,
		right: Box<Self>,
	},
	Leaf {
		signal: Signal,
		net: (bool, bool),
	},
}

impl Default for ArithExpr {
	fn default() -> Self {
		Self::Leaf {
			signal: Signal::None,
			net: (false, false),
		}
	}
}

fn precedence(op: &ArithmeticOperator) -> usize {
	match op {
		// Exponentiation (right-associative)
		ArithmeticOperator::Exp => 6,
		// Multiplication, Division, Modulo
		ArithmeticOperator::Mult | ArithmeticOperator::Div | ArithmeticOperator::Mod => 5,
		// Addition, Subtraction
		ArithmeticOperator::Add | ArithmeticOperator::Sub => 4,
		// Bitwise Shift (Left/Right)
		ArithmeticOperator::Shl | ArithmeticOperator::Sshr => 3,
		// Bitwise AND
		ArithmeticOperator::And => 2,
		// Bitwise XOR
		ArithmeticOperator::Xor => 1,
		// Bitwise OR (lowest precedence)
		ArithmeticOperator::Or => 0,
	}
}

pub fn parser<'src>() -> impl Parser<'src, &'src str, ArithExpr, extra::Err<Rich<'src, char>>> {
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
		let name = name.to_lowercase();
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

	let op_add = just("+").padded().map(|_| ArithmeticOperator::Add);
	let op_sub = just("-").padded().map(|_| ArithmeticOperator::Sub);
	let op_mul = just("*").padded().map(|_| ArithmeticOperator::Mult);
	let op_div = just("/").padded().map(|_| ArithmeticOperator::Div);
	let op_mod = just("%").padded().map(|_| ArithmeticOperator::Mod);
	let op_or = just("|").padded().map(|_| ArithmeticOperator::Or);
	let op_and = just("&").padded().map(|_| ArithmeticOperator::And);
	let op_pow = just("^^").padded().map(|_| ArithmeticOperator::Exp);
	let op_xor = just("^").padded().map(|_| ArithmeticOperator::Xor);
	let op_shl = just("<<").padded().map(|_| ArithmeticOperator::Shl);
	let op_shr = just(">>").padded().map(|_| ArithmeticOperator::Sshr);

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

	let op = op_add
		.or(op_sub)
		.or(op_mul)
		.or(op_div)
		.or(op_mod)
		.or(op_or)
		.or(op_and)
		.or(op_pow)
		.or(op_xor)
		.or(op_shl)
		.or(op_shr)
		.padded();

	let operand = sig_id
		.then(net_decl)
		.or(sig_name.then(net_decl))
		.or(constant.map(|x| (x, (true, true))))
		.padded();

	let leaf = operand.padded().map(|x| ArithExpr::Leaf {
		signal: x.0,
		net: x.1,
	});

	let expr = recursive(|expr| {
		leaf.then(end())
			.map(|x| x.0)
			.or(expr
				.clone()
				.delimited_by(just("("), just(")"))
				.padded()
				.then(end())
				.map(|x| x.0))
			.or(expr
				.clone()
				.then(op.then(expr).repeated().collect::<Vec<_>>())
				.map(
					|(left, chained): (ArithExpr, Vec<(ArithmeticOperator, ArithExpr)>)| {
						let mut operands = vec![left];
						let mut ops = vec![];
						for (op, right) in chained {
							operands.push(right);
							ops.push(op);
						}
						reduce_prescedence(&mut operands, &ops)
					},
				))
	});
	expr.map(|x| x)
}

fn reduce_prescedence(operands: &mut [ArithExpr], ops: &[ArithmeticOperator]) -> ArithExpr {
	if operands.len() == 1 {
		let mut ret = ArithExpr::default();
		std::mem::swap(&mut operands[0], &mut ret);
		return ret;
	}
	let min_precedence = ops.iter().map(precedence).min().unwrap();
	let split_idx = if min_precedence == 6 {
		// exp is right to left evaluated
		ops.iter().enumerate().rev().find_map(|(idx, op)| {
			if precedence(op) == min_precedence {
				Some(idx)
			} else {
				None
			}
		})
	} else {
		ops.iter().enumerate().find_map(|(idx, op)| {
			if precedence(op) == min_precedence {
				Some(idx)
			} else {
				None
			}
		})
	}
	.unwrap();
	let (left, right) = operands.split_at_mut(split_idx + 1);
	ArithExpr::Expr {
		left: Box::new(reduce_prescedence(left, &ops[0..split_idx])),
		op: ops[split_idx],
		right: Box::new(reduce_prescedence(right, &ops[split_idx + 1..])),
	}
}

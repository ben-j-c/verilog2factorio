use itertools::izip;

use crate::{
	logical_design::{DeciderOperator, DeciderRowConjDisj, Node, Signal},
	ndarr::Arr2,
	signal_lookup_table::n_ids,
};

use super::SimState;

impl SimState {
	pub(super) fn execute_decider_op(left: i32, op: DeciderOperator, right: i32) -> bool {
		match op {
			DeciderOperator::LessThan => left < right,
			DeciderOperator::GreaterThan => left > right,
			DeciderOperator::Equal => left == right,
			DeciderOperator::NotEqual => left != right,
			DeciderOperator::GreaterThanEqual => left >= right,
			DeciderOperator::LessThanEqual => left <= right,
		}
	}

	pub(super) fn execute_decider_output_everything_model(
		&self,
		node: &Node,
		outp_state_red: &[i32],
		outp_state_green: &[i32],
		network: (bool, bool),
		constant: Option<i32>,
	) {
	}

	pub(super) fn execute_decider_output_signal_model(
		&self,
		node: &Node,
		output_signals: &mut [i32],
		network: (bool, bool),
		constant: Option<i32>,
		output_id: i32,
	) {
		let (expressions, expression_conj_disj, input_left_networks, input_right_networks, ..) =
			node.function.unwrap_decider();
		let end = if expressions.iter().any(|e| e.0 == Signal::Each) {
			n_ids()
		} else {
			1
		};
		let mut sat_vec = vec![false; end as usize];
		for each_id in 0..end {
			let n_expr = expressions.len();
			let mut sat_or = false;
			let mut sat_and = true;
			for idx in 0..n_expr {
				let expr = &expressions[idx];
				let each_left = if end == 1 || expr.0 != Signal::Each {
					None
				} else {
					Some(each_id)
				};
				let each_right = if end == 1 || expr.2 != Signal::Each {
					None
				} else {
					Some(each_id)
				};
				let sat = self.evaluate_decider_condition(
					node,
					expr,
					&input_left_networks[idx],
					&input_right_networks[idx],
					each_left,
					each_right,
				);
				sat_and &= sat;
				if expression_conj_disj[idx] == DeciderRowConjDisj::Or {
					sat_or |= sat_and;
					sat_and = true;
				}
			}
			if n_expr > 0 {
				sat_or |= sat_and;
			}
			sat_vec[each_id as usize] = sat_or;
		}
		for sat in sat_vec {
			if !sat {
				continue;
			}
			if let Some(c) = constant {
				output_signals[output_id as usize] += c;
			} else {
				output_signals[output_id as usize] += constant
					.unwrap_or_else(|| self.get_seen_signal_count(node.id, output_id, network));
			}
		}
	}

	pub(super) fn execute_decider_output_each_model(
		&self,
		node: &Node,
		outp_state_red: &[i32],
		outp_state_green: &[i32],
		network: (bool, bool),
		constant: Option<i32>,
	) {
		let (
			expressions,
			expression_conj_disj,
			input_left_networks,
			input_right_networks,
			output_network,
			use_input_count,
			constants,
		) = node.function.unwrap_decider();
	}

	pub(super) fn execute_decider_output_anything_model(
		&self,
		node: &Node,
		outp_state_red: &[i32],
		outp_state_green: &[i32],
		network: (bool, bool),
		constant: Option<i32>,
	) {
	}

	fn evaluate_decider_condition(
		&self,
		node: &Node,
		expr: &(Signal, DeciderOperator, Signal),
		left_network: &(bool, bool),
		right_network: &(bool, bool),
		each_check_left: Option<i32>,
		each_check_right: Option<i32>,
	) -> bool {
		if expr.0 == Signal::Everything {
			todo!()
		}
		// Simple case
		let left = if expr.0 == Signal::Anything {
			self.get_seen_signal_count_any(node.id, *left_network).0
		} else if let Signal::Id(id) = expr.0 {
			self.get_seen_signal_count(node.id, id, *left_network)
		} else if let Some(id) = each_check_left {
			self.get_seen_signal_count(node.id, id, *left_network)
		} else {
			0
		};
		let right = if expr.0 == Signal::Anything {
			self.get_seen_signal_count_any(node.id, *right_network).0
		} else if let Signal::Id(id) = expr.2 {
			self.get_seen_signal_count(node.id, id, *right_network)
		} else if let Some(id) = each_check_right {
			self.get_seen_signal_count(node.id, id, *right_network)
		} else {
			0
		};
		Self::execute_decider_op(left, expr.1, right)
	}

	pub(super) fn compute_decider_comb_helper(&self, node: &Node) {
		let (
			expressions,
			expression_conj_disj,
			input_left_networks,
			input_right_networks,
			output_network,
			use_input_count,
			constants,
		) = node.function.unwrap_decider();
		let n_expr = expressions.len();
		let mut sat_or = false;
		let mut and_sat = true;
		for idx in 0..n_expr {
			let sat = self.evaluate_decider_condition(
				node,
				&expressions[idx],
				&input_left_networks[idx],
				&input_right_networks[idx],
				None,
				None,
			);
			and_sat &= sat;
		}
		sat_or |= and_sat;
		for out in &node.output {
			match out {
				Signal::Anything => todo!(),
				Signal::Id(_) => todo!(),
				Signal::Everything => todo!(),
				Signal::Each => todo!(),
				Signal::Constant(_) => todo!(),
				Signal::None => todo!(),
			}
		}
	}

	pub(super) fn compute_decider_comb(
		&self,
		node: &Node,
		new_state_red: &mut Arr2<i32>,
		new_state_green: &mut Arr2<i32>,
	) {
		let (
			expressions,
			expression_conj_disj,
			input_left_networks,
			input_right_networks,
			output_network,
			use_input_count,
			constants,
		) = node.function.unwrap_decider();
		let mut state_out = vec![0; n_ids() as usize];

		for (sig, network, use_input_count, constant) in izip!(
			node.output.iter(),
			output_network.iter(),
			use_input_count.iter(),
			constants.iter()
		) {
			let constant = constant.or(if !*use_input_count { Some(1) } else { None });
			match sig {
				Signal::Id(id) => self.execute_decider_output_signal_model(
					node,
					&mut state_out,
					*network,
					constant,
					*id,
				),
				Signal::Everything => self.execute_decider_output_everything_model(
					node, &state_out, &state_out, *network, constant,
				),
				Signal::Anything => self.execute_decider_output_anything_model(
					node, &state_out, &state_out, *network, constant,
				),
				Signal::Each => self.execute_decider_output_each_model(
					node, &state_out, &state_out, *network, constant,
				),
				Signal::Constant(_) => {
					panic!("Decider combinator has a constant as an output, which isn't valid.")
				}
				Signal::None => continue,
			}
		}
		for idx in 0..state_out.len() {
			new_state_red[node.id.0][idx] = state_out[idx];
			new_state_green[node.id.0][idx] = state_out[idx];
		}
	}
}

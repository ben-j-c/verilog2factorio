use itertools::izip;

use crate::{
	logical_design::{DeciderOperator, DeciderRowConjDisj, Node, Signal},
	ndarr::Arr2,
	signal_lookup_table::n_ids,
	util::{hash_map, hash_set},
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
		output_state: &mut [i32],
		network: (bool, bool),
		constant: Option<i32>,
	) {
		let (expressions, expression_conj_disj, input_left_networks, input_right_networks, ..) =
			node.function.unwrap_decider();
		let sat = self.evaluate_decider_expressions(
			node,
			expressions,
			expression_conj_disj,
			input_left_networks,
			input_right_networks,
			None,
		);
		if !sat {
			return;
		}
		for sig_id in 0..n_ids() {
			let count = self.get_seen_signal_count(node.id, sig_id, network);
			if let Some(constant) = constant {
				output_state[sig_id as usize] += constant;
			} else {
				output_state[sig_id as usize] += count;
			}
		}
	}

	pub(super) fn execute_decider_output_signal_model(
		&self,
		node: &Node,
		output_signals: &mut [i32],
		network: (bool, bool),
		constant: Option<i32>,
		output_id: i32,
	) {
		let mut each_seen_colours = (false, false);
		let mut has_each = false;
		let (expressions, expression_conj_disj, input_left_networks, input_right_networks, ..) =
			node.function.unwrap_decider();

		for i in 0..expressions.len() {
			if expressions[i].0 == Signal::Each {
				has_each = true;
				each_seen_colours.0 |= input_left_networks[i].0;
				each_seen_colours.1 |= input_left_networks[i].1;
			}
			if expressions[i].2 == Signal::Each {
				each_seen_colours.0 |= input_right_networks[i].0;
				each_seen_colours.1 |= input_right_networks[i].1;
			}
		}
		if has_each {
			let mut sat_vec = vec![false; n_ids() as usize];
			let (mut seen_red, mut seen_green) = self.get_seen_signals(node.id);
			if !each_seen_colours.0 {
				seen_red.clear();
			}
			if each_seen_colours.1 {
				seen_green.clear();
			}
			for each_id in seen_red.union(&seen_green) {
				if !seen_red.contains(&each_id) && !seen_green.contains(&each_id) {
					continue;
				}
				let sat = self.evaluate_decider_expressions(
					node,
					expressions,
					expression_conj_disj,
					input_left_networks,
					input_right_networks,
					Some(*each_id),
				);
				sat_vec[*each_id as usize] = sat;
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
		} else {
			let sat = self.evaluate_decider_expressions(
				node,
				expressions,
				expression_conj_disj,
				input_left_networks,
				input_right_networks,
				None,
			);
			if sat {
				if let Some(c) = constant {
					output_signals[output_id as usize] += c;
				} else {
					output_signals[output_id as usize] += constant
						.unwrap_or_else(|| self.get_seen_signal_count(node.id, output_id, network));
				}
			}
		}
	}

	fn evaluate_decider_expressions(
		&self,
		node: &Node,
		expressions: &Vec<(Signal, DeciderOperator, Signal)>,
		expression_conj_disj: &Vec<DeciderRowConjDisj>,
		input_left_networks: &Vec<(bool, bool)>,
		input_right_networks: &Vec<(bool, bool)>,
		each: Option<i32>,
	) -> bool {
		let n_expr = expressions.len();
		let mut sat_or = false;
		let mut sat_and = true;
		for idx in 0..n_expr {
			let expr = &expressions[idx];
			let each_left = each;
			let each_right = each;
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
		sat_or
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
		todo!()
	}

	pub(super) fn execute_decider_output_anything_model(
		&self,
		node: &Node,
		output_signals: &mut [i32],
		network: (bool, bool),
		constant: Option<i32>,
		has_each: bool,
	) {
		if !has_each {
			let (_, sig_id) = self.get_seen_signal_count_any(node.id, network);
			self.execute_decider_output_signal_model(
				node,
				output_signals,
				network,
				constant,
				sig_id,
			);
		} else {
			for sig_id in 0..n_ids() {
				let mut output_signals2 = vec![0; output_signals.len()];
				self.execute_decider_output_signal_model(
					node,
					&mut output_signals2,
					network,
					constant,
					sig_id,
				);
				for sig_id in 0..n_ids() as usize {
					if output_signals2[sig_id] != 0 {
						output_signals[sig_id] += 1;
						return;
					}
				}
			}
		}
	}

	pub(super) fn evaluate_decider_condition(
		&self,
		node: &Node,
		expr: &(Signal, DeciderOperator, Signal),
		left_network: &(bool, bool),
		right_network: &(bool, bool),
		each_check_left: Option<i32>,
		each_check_right: Option<i32>,
	) -> bool {
		if expr.0 == Signal::Everything {
			for i in 0..n_ids() {
				let seen = self.get_seen_signal_count(node.id, i, *left_network);
				if seen == 0 {
					continue;
				}
				let expr = (Signal::Id(i), expr.1, expr.2);
				if self.evaluate_decider_condition(
					node,
					&expr,
					left_network,
					right_network,
					each_check_left,
					each_check_right,
				) {
					return false;
				}
			}
			return true;
		}
		// Simple case
		let left = if expr.0 == Signal::Anything {
			self.get_seen_signal_count_any(node.id, *left_network).0
		} else if let Signal::Id(id) = expr.0 {
			self.get_seen_signal_count(node.id, id, *left_network)
		} else if let Some(id) = each_check_left {
			self.get_seen_signal_count(node.id, id, *left_network)
		} else if let Signal::Constant(c) = expr.0 {
			c
		} else {
			0
		};
		let right = if expr.0 == Signal::Anything {
			self.get_seen_signal_count_any(node.id, *right_network).0
		} else if let Signal::Id(id) = expr.2 {
			self.get_seen_signal_count(node.id, id, *right_network)
		} else if let Some(id) = each_check_right {
			self.get_seen_signal_count(node.id, id, *right_network)
		} else if let Signal::Constant(c) = expr.2 {
			c
		} else {
			0
		};
		Self::execute_decider_op(left, expr.1, right)
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
					node,
					&mut state_out,
					*network,
					constant,
				),
				Signal::Anything => {
					let has_each = expressions.iter().any(|e| e.0 == Signal::Each);
					self.execute_decider_output_anything_model(
						node,
						&mut state_out,
						*network,
						constant,
						has_each,
					)
				}
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

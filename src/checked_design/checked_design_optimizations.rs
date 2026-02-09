use std::ops::{Range, RangeFrom};

use super::*;

pub trait Optimization {
	fn apply(design: &mut CheckedDesign, mapped_design: &MappedDesign) -> usize;
}

pub struct ConstantFold {}

impl Optimization for ConstantFold {
	fn apply(des: &mut CheckedDesign, mapped_design: &MappedDesign) -> usize {
		let mut n_pruned = 0;
		for id in 0..des.nodes.len() {
			let node = &des.nodes[id];
			if !node.constants.is_empty() {
				continue;
			}
			if let NodeType::CellBody { .. } = &node.node_type {
			} else {
				continue;
			};
			let mapped = if let Some(m) = mapped_design.get_cell_option(&node.mapped_id) {
				m
			} else {
				continue;
			};
			let optimization_applies = mapped.cell_type.get_decider_op().is_some()
				|| mapped.cell_type.get_arithmetic_op().is_some()
				|| matches!(
					mapped.cell_type,
					ImplementableOp::PMux(_, _) | ImplementableOp::Mux
				);
			if !optimization_applies {
				continue;
			}
			let (body, output, input) = des.get_fanin_body_subgraphs(id);
			// The actual constant merging.
			des.nodes[id].constants = vec![None; body.len()];
			for (idx, body_id) in body.iter().enumerate() {
				let fanin_body = &des.nodes[*body_id];
				if !matches!(fanin_body.node_type, NodeType::CellBody { .. }) {
					continue;
				}
				if let BodyType::Constant { value } = fanin_body.node_type.get_cell_type() {
					des.nodes[id].constants[idx] = Some(value);
					des.disconnect(output[idx], input[idx]);
				}
			}
			// If the input constant has no other fanout, then prune it.
			for idx in 0..body.len() {
				n_pruned += des.try_prune(body[idx]);
			}
		}
		n_pruned
	}
}

pub struct DeciderFold {}

impl DeciderFold {
	fn pin_applies(des: &CheckedDesign, body: ImplementableOp, input: NodeId) -> bool {
		let input = &des.nodes[input];
		let pin_name = input.node_type.mapped_terminal_name();
		match body {
			ImplementableOp::ReduceAnd => true,
			ImplementableOp::ReduceOr => true,
			ImplementableOp::LUT(_) => true,
			ImplementableOp::PMux(_, _) => pin_name.starts_with("S"),
			ImplementableOp::Mux => pin_name == "S",
			ImplementableOp::Sop(_) => true,
			ImplementableOp::SopNot(_) => true,
			_ => unreachable!(),
		}
	}
}

impl Optimization for DeciderFold {
	fn apply(des: &mut CheckedDesign, mapped_design: &MappedDesign) -> usize {
		let mut n_pruned = 0;
		for id in 0..des.nodes.len() {
			let node = &des.nodes[id];
			if !node.folded_expressions.is_empty() {
				continue;
			}
			if let NodeType::CellBody { .. } = &node.node_type {
			} else {
				continue;
			};
			let mapped = if let Some(m) = mapped_design.get_cell_option(&node.mapped_id) {
				m
			} else {
				continue;
			};
			let n_select = match mapped.cell_type {
				ImplementableOp::LUT(x) | ImplementableOp::PMux(_, x) | ImplementableOp::Sop(x) => {
					x
				},
				ImplementableOp::Mux => 1,
				_ => continue, // Npt applicable
			};
			let (body, output, input) = des.get_fanin_body_subgraphs(id);
			let mut folded_expressions = vec![];
			let mut pin_counter = 0;
			for (idx, body_id) in body.iter().enumerate() {
				if *body_id == NodeId::MAX {
					continue;
				}
				if !DeciderFold::pin_applies(des, mapped.cell_type, input[idx]) {
					continue;
				}
				pin_counter += 1;
				folded_expressions.push(None);
				let folded_expressions = folded_expressions.last_mut().unwrap();
				let fanin_body = &des.nodes[*body_id];
				if !matches!(fanin_body.node_type, NodeType::CellBody { .. }) {
					continue;
				}

				let fanin_body_mapped =
					if let Some(m) = mapped_design.get_cell_option(&fanin_body.mapped_id) {
						m
					} else {
						continue;
					};
				// Finally, retain the body node for future reference when building the next
				if fanin_body_mapped.cell_type.get_decider_op().is_some()
					&& des.count_connected_terminals(*body_id) == 1
				{
					assert!(
						!fanin_body.constants.is_empty(),
						"Decider needs at least 1 constant to be folded in."
					);
					// Here I precompute what the expression for the folded expression should be. In the PMux/LUT
					// implementation I'll check this first and use it instead of s[i] == 1.
					let left = fanin_body.constants[0] // Recall, terminals are order sensitive, so 0 is left.
						.map(Signal::Constant)
						.unwrap_or(Signal::None);
					let op = fanin_body_mapped.cell_type.get_decider_op().unwrap();
					let right = fanin_body.constants[1]
						.map(Signal::Constant)
						.unwrap_or(Signal::None);
					assert_ne!(left, right);
					let left_placeholder = if left == Signal::None {
						Some(pin_counter - 1)
					} else {
						None
					};
					let right_placeholder = if right == Signal::None {
						Some(pin_counter - 1)
					} else {
						None
					};
					*folded_expressions = Some(DeciderSop::placeholder(
						left,
						op,
						right,
						left_placeholder,
						right_placeholder,
					));
					// We need to wire around the expression being folded in.
					//                        1                 2              3                 4                 5
					// third_party_output (O) -> body_input (I) -> body_id (B) -> output[idx] (O) -> input[idx] (I) -> id
					//        |                   ^ the cell being pruned                               ^
					//        +-------------------------------------------------------------------------+
					//        6 This wire is being added by the connect call.
					let (third_party_output, body_input) =
						des.get_sigular_output_input_pair(*body_id);
					des.disconnect(output[idx], input[idx]); // 4
					des.connect(third_party_output, input[idx]); // 6
					if des.nodes[output[idx]].fanout.is_empty() {
						// Can only remove this iff the decider being wired around is no longer needed by anything.
						des.disconnect(third_party_output, body_input); // 1
					}
				}
			}
			assert!(n_select == folded_expressions.len());
			if matches!(mapped.cell_type, ImplementableOp::Mux) {
				if folded_expressions[0].is_some() {
					let xx = folded_expressions.pop().unwrap().unwrap();
					des.nodes[id].folded_expressions = FoldedData::Single(xx);
				}
			} else {
				des.nodes[id].folded_expressions = FoldedData::Vec(folded_expressions);
			}
			// If the input constant has no other fanout, then prune it.
			for idx in 0..body.len() {
				if body[idx] == NodeId::MAX {
					continue;
				}
				n_pruned += des.try_prune(body[idx]);
			}
		}
		n_pruned
	}
}

pub struct SopNot {}

impl Optimization for SopNot {
	fn apply(des: &mut CheckedDesign, mapped_design: &MappedDesign) -> usize {
		let mut n_pruned = 0;
		for id in 0..des.nodes.len() {
			{
				let node = &mut des.nodes[id];
				if !node.folded_expressions.is_empty() {
					continue;
				}
				if !matches!(&node.node_type, NodeType::CellBody { .. }) {
					continue;
				};
				let mapped = if let Some(m) = mapped_design.get_cell_option(&node.mapped_id) {
					m
				} else {
					continue;
				};
				let optimization_applies = matches!(mapped.cell_type, ImplementableOp::Neg);
				if !optimization_applies {
					continue;
				};
			}

			let (body, output, input) = des.get_fanin_body_subgraphs(id);
			assert!(body.len() == 1);
			let (body, _output, _input) = (body[0], output[0], input[0]);
			assert!(body != NodeId::MAX);

			// Convert body to SopNot
			let body_node = &mut des.nodes[body];
			if let NodeType::CellBody {
				cell_type: BodyType::MultiPart {
					op: ImplementableOp::Sop(width),
				},
			} = &body_node.node_type
			{
				body_node.node_type = NodeType::CellBody {
					cell_type: BodyType::MultiPart {
						op: ImplementableOp::SopNot(*width),
					},
				};
			} else {
				continue;
			};

			let not_branch = des.new_node(
				"Y_BAR",
				NodeType::CellOutput {
					port: "Y_BAR".to_owned(),
					connected_id: usize::MAX,
				},
			);
			des.connect(body, not_branch);

			let new_to_connect = des.nodes[des.nodes[id].fanout[0]].fanout.clone();

			n_pruned += des.disconnect_and_try_prune(id);

			for to_connect in new_to_connect {
				des.connect(not_branch, to_connect);
			}
		}
		n_pruned
	}
}

pub struct MuxToPmux {}

impl Optimization for MuxToPmux {
	fn apply(des: &mut CheckedDesign, _mapped_design: &MappedDesign) -> usize {
		let n_pruned = 0;
		for id in 0..des.nodes.len() {
			let node = &mut des.nodes[id];
			if !matches!(
				&node.node_type,
				NodeType::CellBody {
					cell_type: BodyType::MultiPart {
						op: ImplementableOp::Mux,
					}
				}
			) {
				continue;
			}
			node.node_type = NodeType::CellBody {
				cell_type: BodyType::MultiPart {
					op: ImplementableOp::PMux(false, 1),
				},
			};
			let expr = match &node.folded_expressions {
				FoldedData::Vec(_) => unreachable!(),
				FoldedData::Single(expr) => expr.clone(),
				FoldedData::None => {
					continue;
				},
			};
			node.folded_expressions = FoldedData::Vec(vec![Some(expr)]);
		}
		n_pruned
	}
}

pub struct PMuxFold {}

impl Optimization for PMuxFold {
	fn apply(des: &mut CheckedDesign, _mapped_design: &MappedDesign) -> usize {
		let mut n_pruned = 0;
		let get_body_driving_pin = |des: &CheckedDesign, id: NodeId, pin: usize| {
			let input = des.nodes[id].fanin[pin];
			let output = des.nodes[input].fanin.get(0)?;
			let body = des.nodes[*output].fanin[0];
			Some(body)
		};
		let n_siblings_for_pin = |des: &CheckedDesign, id: NodeId, pin: usize| {
			let input = des.nodes[id].fanin[pin];
			let output = des.nodes[input].fanin[0];
			des.nodes[output].fanout.len()
		};
		for id in 0..des.nodes.len() {
			let (width_sink, width_source, source_id, source_full_case) = {
				let node = &des.nodes[id];
				let n_ports_sink = match_or_continue!(
					NodeType::CellBody {
						cell_type: BodyType::MultiPart {
							op: ImplementableOp::PMux(false, width),
						},
					},
					&node.node_type,
					*width
				);
				let source_id = unwrap_or_continue!(get_body_driving_pin(des, id, 0));
				let source_node = &des.nodes[source_id];
				let (source_full_case, n_ports_source) = match_or_continue!(
					NodeType::CellBody {
						cell_type: BodyType::MultiPart {
							op: ImplementableOp::PMux(full_case, width),
						},
					},
					&source_node.node_type,
					*full_case,
					*width
				);
				if n_siblings_for_pin(des, id, 0) != 1 {
					//println!("Skipping {id} -> {source_id} due to multi-fanout.");
					continue;
				}
				(n_ports_sink, n_ports_source, source_id, source_full_case)
			};
			let source_has_a = !source_full_case;
			if width_sink > 1 || width_source > 1 {
				continue;
			}
			// Now we know pmux -> Y -> A -> pmux
			let source_s_range = {
				let start_idx = source_has_a as usize + width_source;
				start_idx..start_idx + width_sink
			};
			force_insert_expressions(des, source_id, source_s_range);
			let sink_s_range = {
				let start_idx = 1 + width_sink;
				start_idx..start_idx + width_source
			};
			force_insert_expressions(des, id, sink_s_range);

			let mut to_del = vec![source_id];
			{
				let (source, sink) = util::mut_idx(&mut des.nodes, source_id, id);
				to_del.push(source.fanout[0]);
				{
					// insert B ports
					let idx = 1 + width_sink;
					let start_idx = source_has_a as usize;
					sink.fanin.splice(
						idx..idx,
						source.fanin[start_idx..start_idx + width_source]
							.iter()
							.cloned(),
					);
					sink.constants.splice(
						idx..idx,
						source.constants[start_idx..start_idx + width_source]
							.iter()
							.cloned(),
					);
				}
				{
					// insert S ports
					let start_idx = source_has_a as usize + width_source;
					sink.fanin.extend(
						source.fanin[start_idx..start_idx + width_source]
							.iter()
							.copied(),
					);
					sink.constants.extend(
						source.constants[start_idx..start_idx + width_source]
							.iter()
							.cloned(),
					);
				}
				// replace A
				to_del.push(sink.fanin[0]); // Not needed anymore
				if source_has_a {
					sink.fanin[0] = source.fanin[0];
					sink.constants[0] = source.constants[0];
				}
				if let NodeType::CellBody {
					cell_type:
						BodyType::MultiPart {
							op: ImplementableOp::PMux(full_case, width),
						},
				} = &mut sink.node_type
				{
					*full_case = source_full_case;
					*width = width_source + width_sink;
				}
				// For all new S pins, they must not fire when any of the original fire, so you must
				// Distribute the conjunction of the existing pin expressions with the new ones.
				let sink_exprs = sink.folded_expressions.unwrap_vec();
				let mut combined_sink_expr = DeciderSop::default();
				for sink_expr in sink_exprs.iter() {
					let sink_expr = sink_expr.as_ref().unwrap();
					combined_sink_expr = combined_sink_expr.or(&sink_expr);
				}
				let combined_sink_expr = combined_sink_expr.complement();
				let mut new_exprs = sink_exprs.iter().cloned().collect_vec();
				let source_exprs = source.folded_expressions.unwrap_vec();
				for source_expr in source_exprs.iter() {
					let expr = source_expr.as_ref().unwrap();
					new_exprs.push(Some(
						combined_sink_expr.and(&expr.increment_placeholders(width_sink)),
					));
				}
				sink.folded_expressions = FoldedData::Vec(new_exprs);
				if !source_has_a {
					sink.fanin.remove(0);
					sink.constants.remove(0);
				}
			}
			{
				for i in 0..des.nodes[id].fanin.len() {
					let fid = des.nodes[id].fanin[i];
					des.nodes[fid].fanout = vec![id];
				}
			}
			for x in to_del {
				let node = &mut des.nodes[x];
				node.node_type = NodeType::Pruned;
				node.fanin.clear();
				node.fanout.clear();
				n_pruned += 1;
			}
		}
		n_pruned
	}
}

pub(crate) fn force_insert_expressions(des: &mut CheckedDesign, id: NodeId, ports: Range<usize>) {
	let width = ports.len();
	let source_node = &mut des.nodes[id];
	if source_node.folded_expressions.is_empty() {
		let folded_expr = vec![None; width];
		source_node.folded_expressions = FoldedData::Vec(folded_expr);
	}
	{
		let exprs = source_node.folded_expressions.unwrap_vec_mut();
		izip!(exprs.iter_mut().enumerate()).for_each(|(idx, expr)| {
			if expr.is_some() {
				return;
			}
			*expr = Some(DeciderSop::placeholder(
				Signal::None,
				DeciderOperator::Equal,
				Signal::Constant(1),
				Some(idx),
				None,
			));
		});
	}
	if source_node.constants.is_empty() {
		source_node.constants = vec![None; width];
	}
}

pub(crate) fn update_folded_data_isotropic_ports(data: &mut FoldedData, signals: Vec<Signal>) {
	match data {
		FoldedData::Vec(exprs) => {
			izip!(exprs, signals.iter().enumerate()).for_each(|(expr, (ident, signal))| {
				if let Some(expr) = expr {
					*expr = expr.replace_placeholder(*signal, ident);
				}
			})
		},
		FoldedData::Single(_) => {
			panic!("There shouldn't be any isotropic cells with ports with folded expressions. If single input cell is folded, should still use Vec");
		},
		FoldedData::None => {},
	}
}

pub(crate) fn update_folded_data_mux(node: &mut Node, signals: Vec<Signal>) {
	match &mut node.folded_expressions {
		FoldedData::Vec(_) => panic!("Mux should not have a vec of folded data."),
		FoldedData::Single(expr) => {
			*expr = expr.replace_placeholder(signals[2], 0); // S
		},
		FoldedData::None => {},
	}
}

pub(crate) fn update_folded_data_pmux(
	node: &mut Node,
	signals: Vec<Signal>,
	full_case: bool,
	width: usize,
) {
	match &mut node.folded_expressions {
		FoldedData::Vec(expr) => {
			if !full_case {
				izip!(expr, signals[1 + width..].iter().enumerate()).for_each(
					|(expr, (ident, signal))| {
						if let Some(expr) = expr {
							*expr = expr.replace_placeholder(*signal, ident);
						}
					},
				);
			} else {
				izip!(expr, signals[width..].iter().enumerate()).for_each(
					|(expr, (ident, signal))| {
						if let Some(expr) = expr {
							*expr = expr.replace_placeholder(*signal, ident);
						}
					},
				);
			}
		},
		FoldedData::Single(_expr) => {
			panic!("PMux should have a vec of folded data.")
		},
		FoldedData::None => {},
	}
}

pub struct MuxDuplication {}

impl Optimization for MuxDuplication {
	fn apply(des: &mut CheckedDesign, _mapped_design: &MappedDesign) -> usize {
		let get_body_driving_pin = |des: &CheckedDesign, id: NodeId, pin: usize| {
			let input = des.nodes[id].fanin[pin];
			let output = des.nodes[input].fanin.get(0)?;
			let body = des.nodes[*output].fanin[0];
			Some(body)
		};
		let n_siblings_for_pin = |des: &CheckedDesign, id: NodeId, pin: usize| {
			let input = des.nodes[id].fanin[pin];
			let output = des.nodes[input].fanin[0];
			des.nodes[output].fanout.len()
		};
		for id in 0..des.nodes.len() {
			let (width_sink, width_source, source_id) = {
				let node = &des.nodes[id];
				let width_sink = match_or_continue!(
					NodeType::CellBody {
						cell_type: BodyType::MultiPart {
							op: ImplementableOp::PMux(false, width),
						},
					},
					&node.node_type,
					*width
				);
				let source_id = unwrap_or_continue!(get_body_driving_pin(des, id, 0));
				let source_node = &des.nodes[source_id];
				let width_source = match_or_continue!(
					NodeType::CellBody {
						cell_type: BodyType::MultiPart {
							op: ImplementableOp::PMux(_, width),
						},
					},
					&source_node.node_type,
					*width
				);
				if n_siblings_for_pin(des, id, 0) == 1 {
					continue;
				}
				(width_sink, width_source, source_id)
			};
			if width_sink > 1 || width_source > 1 {
				continue;
			}
			let fanout = des.nodes[source_id].fanout[0];
			let fanin = des.nodes[source_id].fanin.clone();
			let new_body = des.new_node("$v2f_pmux_duplication", des.node_type(source_id).clone());
			des.nodes[new_body].constants = des.nodes[source_id].constants.clone();
			des.nodes[new_body].folded_expressions =
				des.nodes[source_id].folded_expressions.clone();

			for i in 0..fanin.len() {
				let new_fid =
					des.new_node("$v2f_pmux_duplication", des.node_type(fanin[i]).clone());
				des.connect(new_fid, new_body);
				if let Some(id) = des.nodes[fanin[i]].fanin.get(0) {
					des.connect(*id, new_fid);
				}
			}
			let new_fanout = des.new_node("$v2f_pmux_duplication", des.node_type(fanout).clone());
			des.connect(new_body, new_fanout);

			des.disconnect(fanout, des.nodes[id].fanin[0]);
			des.connect(new_fanout, des.nodes[id].fanin[0]);
			des.check_connections();
		}
		0
	}
}

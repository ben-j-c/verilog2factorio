use std::ops::Range;

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
	fn apply(des: &mut CheckedDesign, _mapped_design: &MappedDesign) -> usize {
		let mut n_pruned = 0;
		for id in 0..des.nodes.len() {
			{
				let node = &mut des.nodes[id];
				match_or_continue!(
					NodeType::CellBody {
						cell_type: BodyType::AY {
							op: ImplementableOp::Neg,
						},
					},
					&node.node_type,
					()
				);
			}

			let (body, output, input) = des.get_fanin_body_subgraphs(id);
			if body.len() == 0 {
				continue;
			}
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

pub struct PMuxFoldA {}

impl Optimization for PMuxFoldA {
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
			let (sink_width, source_width, source_id, source_full_case) = {
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
			let source_start_b = source_has_a as usize;
			let source_end_b = source_start_b + source_width;
			let source_start_s = source_end_b;
			let source_end_s = source_start_s + source_width;
			let sink_has_a = true;
			let sink_start_b = sink_has_a as usize;
			let sink_end_b = sink_start_b + sink_width;
			let sink_start_s = sink_end_b;
			let sink_end_s = sink_start_s + sink_width;

			if sink_width > 4 || source_width > 4 {
				continue;
			}
			// Now we know pmux -> Y -> A -> pmux
			force_insert_expressions(des, source_id, source_start_s..source_end_s);
			force_insert_expressions(des, id, sink_start_s..sink_end_s);

			let mut to_del = vec![source_id];
			{
				let (source, sink) = util::mut_idx(&mut des.nodes, source_id, id);
				to_del.push(source.fanout[0]);
				{
					// insert B ports
					sink.fanin.splice(
						sink_end_b..sink_end_b,
						source.fanin[source_start_b..source_end_b].iter().cloned(),
					);
					sink.constants.splice(
						sink_end_b..sink_end_b,
						source.constants[source_start_b..source_end_b]
							.iter()
							.cloned(),
					);
				}
				{
					// insert S ports
					sink.fanin
						.extend(source.fanin[source_start_s..source_end_s].iter().copied());
					sink.constants.extend(
						source.constants[source_start_s..source_end_s]
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
					*width = source_width + sink_width;
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
						combined_sink_expr.and(&expr.increment_placeholders(sink_width)),
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
			if width_sink > 6 || width_source > 6 {
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

/*
Graph(G) -- some graph connecting to input of source
Sink(X): [B0, B1, S0, S1]
Source(W): [B0, B1, S0, S1]
G -> ... -> W-> Y -> B1 -> X

Optimization:
Sink(X): [XB0, B_dummy, WB0, WB1, XS0, XS1, WS0, WS1]
delete Source(W)

A problem with the current implementation is stopping progress:
B_dummy is deleted, so the width doesn't match causing a crash.
It needs to be retained, but detached from G and given a constant 0.

XS1 is there, but I need to make sure its still connected to G. If not
WS0 and WS1 wont see its important gating signal that it inherits from XS1.

*/
pub struct PMuxFoldB {}

impl Optimization for PMuxFoldB {
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
		for sink_id in 0..des.nodes.len() {
			let (sink_full_case, sink_width) = match_or_continue!(
				NodeType::CellBody {
					cell_type: BodyType::MultiPart {
						op: ImplementableOp::PMux(full_case, width),
					},
				},
				des.node_type(sink_id),
				*full_case,
				*width
			);
			for sink_pin in 0.. {
				let (source_width, source_id, source_full_case) = {
					// Dont count A
					if !sink_full_case && sink_pin == 0 {
						continue;
					}
					// Dont count S
					if sink_pin >= !sink_full_case as usize + sink_width {
						break;
					}
					let source_id =
						unwrap_or_continue!(get_body_driving_pin(des, sink_id, sink_pin));
					let source_node = &des.nodes[source_id];
					let (source_full_case, source_width) = match_or_continue!(
						NodeType::CellBody {
							cell_type: BodyType::MultiPart {
								op: ImplementableOp::PMux(full_case, width),
							},
						},
						&source_node.node_type,
						*full_case,
						*width
					);
					if n_siblings_for_pin(des, sink_id, sink_pin) != 1 {
						//println!("Skipping {id} -> {source_id} due to multi-fanout.");
						continue;
					}
					(source_width, source_id, source_full_case)
				};

				if sink_width > 6 || source_width > 6 {
					break;
				}
				let source_has_a = !source_full_case;
				let source_start_b = source_has_a as usize;
				let source_end_b = source_start_b + source_width;
				let source_start_s = source_end_b;
				let source_end_s = source_start_s + source_width;
				let sink_has_a = !sink_full_case;
				let sink_start_b = sink_has_a as usize;
				let sink_end_b = sink_start_b + sink_width;
				let sink_start_s = sink_end_b;
				let sink_end_s = sink_start_s + sink_width;

				// Now we know pmux -> Y -> A -> pmux
				force_insert_expressions(des, source_id, source_start_s..source_end_s);
				force_insert_expressions(des, sink_id, sink_start_s..sink_end_s);

				let mut to_del = vec![source_id];
				let should_add_new_pin = {
					let (source, sink) = util::mut_idx(&mut des.nodes, source_id, sink_id);
					{
						// insert B ports
						sink.fanin.splice(
							sink_end_b..sink_end_b,
							source.fanin[source_start_b..source_end_b].iter().cloned(),
						);
						sink.constants.splice(
							sink_end_b..sink_end_b,
							source.constants[source_start_b..source_end_b]
								.iter()
								.cloned(),
						);
					}
					{
						// insert S ports
						sink.fanin
							.extend(source.fanin[source_start_s..source_end_s].iter().copied());
						sink.constants.extend(
							source.constants[source_start_s..source_end_s]
								.iter()
								.cloned(),
						);
					}
					// remove sink_pin
					to_del.push(sink.fanin[sink_pin]); // Not needed anymore
					sink.fanin.remove(sink_pin);
					if let NodeType::CellBody {
						cell_type:
							BodyType::MultiPart {
								op: ImplementableOp::PMux(_, width),
							},
					} = &mut sink.node_type
					{
						// + source_has_a because we have to transform A on the source.
						*width = source_width + sink_width + source_has_a as usize;
					}
					// For all new S pins, they must not fire when the replaced pin fires.
					let sink_exprs = sink.folded_expressions.unwrap_vec();
					let sink_expr = sink_exprs[sink_pin - sink_has_a as usize].as_ref().unwrap();
					let sink_expr = sink_expr.complement();
					let mut new_exprs = sink_exprs.iter().cloned().collect_vec();
					new_exprs.remove(sink_pin + sink_has_a as usize); // Remove previous pin's expression.
					let source_exprs = source.folded_expressions.unwrap_vec();
					for source_expr in source_exprs.iter() {
						let expr = source_expr.as_ref().unwrap();
						new_exprs.push(Some(
							sink_expr.and(&expr.increment_placeholders(sink_width)),
						));
					}
					if source_has_a {
						let mut combined_expr = DeciderSop::default();
						for source_expr in source_exprs.iter() {
							let expr = source_expr.as_ref().unwrap();
							combined_expr = combined_expr.or(&expr);
						}
						combined_expr = combined_expr.complement().and(&sink_expr);
						new_exprs.push(Some(combined_expr)); // S pin for source.A
						let final_b_end = sink_has_a as usize + sink_width + source_width;
						// Put source.A into sink.B
						sink.constants.insert(final_b_end, source.constants[0]);
						sink.fanin.insert(final_b_end, source.fanin[0]);
						// Dummy S pin for source.A
						sink.constants.push(Some(0));
						// should_add_new_pin branch below will add this dummy S.
					}
					sink.folded_expressions = FoldedData::Vec(new_exprs);
					source_has_a
				};
				if should_add_new_pin {
					let new_pin = des.new_node(
						"dummy-S",
						NodeType::CellInput {
							port: "dummy-S".to_owned(),
							connected_id: usize::MAX,
						},
					);
					des.connect(new_pin, sink_id);
				}

				// Forcibly take the fanin nodes so delete can cleanly remove unused pins/nodes.
				{
					for i in 0..des.nodes[sink_id].fanin.len() {
						let fid = des.nodes[sink_id].fanin[i];
						des.nodes[fid].fanout = vec![sink_id];
					}
				}
				for x in to_del {
					let node = &mut des.nodes[x];
					node.node_type = NodeType::Pruned;
					node.fanin.clear();
					node.fanout.clear();
					n_pruned += 1;
				}
				break;
			}
		}
		n_pruned
	}
}

pub struct NotDetectAndReplace {}

impl Optimization for NotDetectAndReplace {
	fn apply(des: &mut CheckedDesign, mapped_design: &MappedDesign) -> usize {
		let mut n_pruned = 0;
		for id in 0..des.nodes.len() {
			let table = {
				let node = &mut des.nodes[id];
				match_or_continue!(
					NodeType::CellBody {
						cell_type: BodyType::MultiPart {
							op: ImplementableOp::Sop(1),
						},
					},
					&node.node_type,
					()
				);
				let cell = mapped_design.get_cell(&node.mapped_id);
				cell.parameters["TABLE"]
					.into_bool_vec()
					.unwrap()
					.into_iter()
					.rev()
					.collect_vec()
			};
			if table[0] {
				let node = &mut des.nodes[id];
				node.node_type = NodeType::CellBody {
					cell_type: BodyType::AY {
						op: ImplementableOp::Neg,
					},
				};
				assert!(!table[1])
			} else {
				// Is a nop? weird, but acceptable.
				let output_pin = des.nodes[id].fanout[0];
				let input_pin = des.nodes[id].fanin[0];
				let source = des.nodes[input_pin].fanin[0];
				while !des.nodes[output_pin].fanout.is_empty() {
					let sink = *des.nodes[output_pin].fanout.last().unwrap();
					des.connect(source, sink);
					des.disconnect(output_pin, sink);
				}
				n_pruned += des.disconnect_and_try_prune(id);
			}
		}
		n_pruned
	}
}

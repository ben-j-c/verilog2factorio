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
			for (idx, body_id) in body.iter().enumerate() {
				if *body_id == NodeId::MAX {
					continue;
				}
				if !DeciderFold::pin_applies(des, mapped.cell_type, input[idx]) {
					continue;
				}
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
					*folded_expressions = Some(DeciderSop::simple(left, op, right));
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
			if matches!(mapped.cell_type, ImplementableOp::Mux) && folded_expressions[0].is_some() {
				let xx = folded_expressions.pop().unwrap().unwrap();
				des.nodes[id].folded_expressions = FoldedData::Single(xx);
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

pub struct PmuxFold {}

impl Optimization for PmuxFold {
	fn apply(des: &mut CheckedDesign, mapped_design: &MappedDesign) -> usize {
		let mut n_pruned = 0;
		let get_body_driving_pin = |id: NodeId, pin: usize| {
			let input = des.nodes[id].fanin[pin];
			let output = des.nodes[input].fanin.get(0)?;
			let body = des.nodes[*output].fanin[0];
			Some(body)
		};
		let n_siblings_for_pin = |id: NodeId, pin: usize| {
			let input = des.nodes[id].fanin[pin];
			let output = des.nodes[input].fanin[0];
			des.nodes[output].fanout.len()
		};
		for id in 0..des.nodes.len() {
			let (n_ports_sink, n_ports_source, source_id, source_full_case) = {
				let node = &des.nodes[id];
				let n_ports_sink = match_or_continue!(
					NodeType::CellBody {
						cell_type: BodyType::MultiPart {
							op: ImplementableOp::PMux(false, n_ports),
						},
					},
					&node.node_type,
					*n_ports
				);
				let source_id = unwrap_or_continue!(get_body_driving_pin(id, 0));
				if n_siblings_for_pin(id, 0) != 1 {
					continue;
				}
				let source_node = &des.nodes[source_id];
				let (source_full_case, n_ports_source) = match_or_continue!(
					NodeType::CellBody {
						cell_type: BodyType::MultiPart {
							op: ImplementableOp::PMux(full_case, n_ports),
						},
					},
					&source_node.node_type,
					*full_case,
					*n_ports
				);
				(n_ports_sink, n_ports_source, source_id, source_full_case)
			};
			// Now we know pmux -> Y -> A -> pmux
			let source_ports = des.nodes[source_id].fanin.clone();
			let node = &des.nodes[id];
		}
		n_pruned
	}
}

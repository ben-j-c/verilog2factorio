use std::collections::HashMap;

use itertools::izip;
use serde::Serialize;

use crate::logical_design::{
	self, ArithmeticOperator, DeciderOperator, DeciderRowConjDisj, LogicalDesign, Signal,
};
use crate::phy::{PhyId, PhyNode, PhysicalDesign};
use crate::signal_lookup_table;

#[derive(Debug, Clone, Serialize)]
pub struct BlueprintInterior {
	icons: Vec<()>,
	entities: Vec<Entity>,
	item: String,
	#[serde(skip_serializing_if = "Option::is_none")]
	description: Option<String>,
	version: i64,
	wires: Vec<BlueprintWire>,
}

#[derive(Debug, Clone, Serialize)]
pub struct SerializableDesign {
	blueprint: BlueprintInterior,
}

#[derive(Debug, Clone)]
struct BlueprintWire {
	source_entity_number: usize,
	source_wire_connector_id: usize,
	target_entity_number: usize,
	target_wire_connector_id: usize,
}

impl Serialize for BlueprintWire {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: serde::Serializer,
	{
		vec![
			self.source_entity_number,
			self.source_wire_connector_id,
			self.target_entity_number,
			self.target_wire_connector_id,
		]
		.serialize(serializer)
	}
}

#[derive(Debug, Clone, Serialize)]
struct SignalID {
	name: &'static str,
	#[serde(skip_serializing_if = "Option::is_none")]
	#[serde(rename = "type")]
	type_: Option<&'static str>,
}

#[derive(Debug, Clone, Serialize)]
struct Entity {
	entity_number: usize,
	name: &'static str,
	position: Position,
	#[serde(skip_serializing_if = "Option::is_none")]
	direction: Option<u32>,
	#[serde(skip_serializing_if = "Option::is_none")]
	connections: Option<Connections>,
	#[serde(skip_serializing_if = "Option::is_none")]
	neighbours: Option<Vec<usize>>,
	#[serde(skip_serializing_if = "Option::is_none")]
	control_behavior: Option<ControlBehavior>,
	#[serde(skip_serializing_if = "Option::is_none")]
	variation: Option<i32>,
	#[serde(skip_serializing_if = "Option::is_none")]
	switch_state: Option<bool>,
	#[serde(skip_serializing_if = "Option::is_none")]
	tags: Option<std::collections::HashMap<String, String>>,
	#[serde(skip_serializing_if = "Option::is_none")]
	player_description: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
struct ControlBehavior {
	#[serde(skip_serializing_if = "Option::is_none")]
	circuit_enabled: Option<bool>, // Used by lamps
	#[serde(skip_serializing_if = "Option::is_none")]
	is_on: Option<bool>,
	#[serde(skip_serializing_if = "Option::is_none")]
	arithmetic_conditions: Option<ArithmeticCombinatorParameters>,
	#[serde(skip_serializing_if = "Option::is_none")]
	decider_conditions: Option<DeciderCombinatorParameters>,
	#[serde(skip_serializing_if = "Option::is_none")]
	circuit_condition: Option<DeciderCombinatorCondition>, // Used by lamps
	#[serde(skip_serializing_if = "Option::is_none")]
	sections: Option<LogisticSections>, // Used by costant combinators
	#[serde(skip_serializing_if = "Option::is_none")]
	use_color: Option<bool>, // Used by lamps
}

#[derive(Debug, Clone, Serialize)]
struct LogisticSections {
	sections: Vec<LogisticSection>,
}

#[derive(Debug, Clone, Serialize)]
struct LogisticSection {
	index: u8,
	#[serde(skip_serializing_if = "Option::is_none")]
	filters: Option<Vec<BlueprintLogisticFilter>>,
	#[serde(skip_serializing_if = "Option::is_none")]
	group: Option<String>,
	#[serde(skip_serializing_if = "Option::is_none")]
	multiplier: Option<f64>,
	#[serde(skip_serializing_if = "Option::is_none")]
	active: Option<bool>,
}

#[derive(Debug, Clone, Serialize)]
struct BlueprintLogisticFilter {
	index: u16,
	#[serde(flatten)]
	#[serde(skip_serializing_if = "Option::is_none")]
	signal: Option<SignalID>,
	#[serde(skip_serializing_if = "Option::is_none")]
	comparator: Option<&'static str>,
	count: i32,
	quality: &'static str,
}

#[derive(Debug, Clone, Serialize)]
struct ArithmeticCombinatorParameters {
	#[serde(skip_serializing_if = "Option::is_none")]
	first_signal: Option<SignalID>,
	#[serde(skip_serializing_if = "Option::is_none")]
	second_signal: Option<SignalID>,
	#[serde(skip_serializing_if = "Option::is_none")]
	first_constant: Option<i32>,
	#[serde(skip_serializing_if = "Option::is_none")]
	second_constant: Option<i32>,
	operation: &'static str,
	#[serde(skip_serializing_if = "Option::is_none")]
	output_signal: Option<SignalID>,
	#[serde(skip_serializing_if = "Option::is_none")]
	first_signal_networks: Option<HashMap<String, bool>>,
	#[serde(skip_serializing_if = "Option::is_none")]
	second_signal_networks: Option<HashMap<String, bool>>,
}

#[derive(Debug, Clone, Serialize)]
struct DeciderCombinatorParameters {
	conditions: Vec<DeciderCombinatorCondition>,
	outputs: Vec<DeciderCombinatorOutput>,
}

#[derive(Debug, Clone, Serialize)]
struct DeciderCombinatorCondition {
	#[serde(skip_serializing_if = "Option::is_none")]
	first_signal: Option<SignalID>,
	#[serde(skip_serializing_if = "Option::is_none")]
	second_signal: Option<SignalID>,
	#[serde(skip_serializing_if = "Option::is_none")]
	constant: Option<i32>,
	comparator: &'static str,
	#[serde(skip_serializing_if = "Option::is_none")]
	compare_type: Option<&'static str>,
	#[serde(skip_serializing_if = "Option::is_none")]
	first_signal_networks: Option<HashMap<String, bool>>,
	#[serde(skip_serializing_if = "Option::is_none")]
	second_signal_networks: Option<HashMap<String, bool>>,
}

#[derive(Debug, Clone, Serialize)]
struct DeciderCombinatorOutput {
	signal: SignalID,
	copy_count_from_input: bool,
	#[serde(skip_serializing_if = "Option::is_none")]
	constant: Option<i32>,
	networks: Option<HashMap<String, bool>>,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Serialize)]
struct ProgrammableSpeakerCircuitParameters {}

#[derive(Debug, Clone, Serialize)]
struct Position {
	x: f64,
	y: f64,
}

impl From<(f64, f64)> for Position {
	fn from(val: (f64, f64)) -> Self {
		Position {
			x: val.0 + 0.5,
			y: val.1 + 0.5,
		}
	}
}

#[derive(Debug, Clone, Serialize)]
struct Connections {
	#[serde(rename = "1")]
	#[serde(skip_serializing_if = "Option::is_none")]
	primary: Option<ConnectionPoint>,
	#[serde(rename = "2")]
	#[serde(skip_serializing_if = "Option::is_none")]
	secondary: Option<ConnectionPoint>,
}

#[derive(Debug, Clone, Serialize)]
struct ConnectionPoint {
	#[serde(skip_serializing_if = "Option::is_none")]
	red: Option<Vec<ConnectionData>>,
	#[serde(skip_serializing_if = "Option::is_none")]
	green: Option<Vec<ConnectionData>>,
}

#[derive(Debug, Clone, Serialize)]
struct ConnectionData {
	entity_id: usize,
	circuit_id: usize,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Serialize)]
struct Color {
	r: f64,
	g: f64,
	b: f64,
	#[serde(skip_serializing_if = "Option::is_none")]
	a: Option<f64>,
}

impl SerializableDesign {
	pub fn new() -> Self {
		SerializableDesign {
			blueprint: BlueprintInterior {
				item: "blueprint".to_owned(),
				entities: vec![],
				icons: vec![],
				description: Some("v2f compile".to_owned()),
				version: 562949954797573,
				wires: vec![],
			},
		}
	}

	#[allow(dead_code)]
	pub fn set_description(&mut self, description: &str) {
		self.blueprint.description = Some(description.to_owned())
	}

	pub fn build_from(&mut self, physical: &PhysicalDesign, logical: &LogicalDesign) {
		let mut entities = vec![];
		let mut idx_entities: HashMap<PhyId, usize> = HashMap::new();
		physical.for_all_phy(|node| {
			if node.is_pole() {
				let name = match node.hop_type {
					crate::phy::WireHopType::Small => "small-electric-pole",
					crate::phy::WireHopType::Medium => "medium-electric-pole",
					crate::phy::WireHopType::Big => "big-electric-pole",
					crate::phy::WireHopType::Substation => "substation",
					_ => unreachable!(),
				};
				entities.push(Entity {
					entity_number: entities.len() + 1,
					name,
					position: node.position.into(),
					direction: None,
					connections: None,
					neighbours: None,
					control_behavior: None,
					variation: None,
					switch_state: None,
					tags: None,
					player_description: None,
				});
			} else {
				entities.push(Entity {
					entity_number: entities.len() + 1,
					name: node.resolve_name(logical),
					position: node.position.into(),
					direction: Some(node.orientation),
					connections: None,
					neighbours: None,
					control_behavior: node.resolve_control_behaviour(logical),
					variation: None,
					switch_state: None,
					tags: None,
					player_description: node.resolve_description(logical),
				});
			}
			idx_entities.insert(node.id, entities.len());
		});
		let mut wires = vec![];
		physical.for_all_wires(|wire| {
			wires.push(BlueprintWire {
				source_entity_number: idx_entities[&wire.node1_id],
				source_wire_connector_id: wire.terminal1_id.0 as usize,
				target_entity_number: idx_entities[&wire.node2_id],
				target_wire_connector_id: wire.terminal2_id.0 as usize,
			})
		});
		self.blueprint.entities = entities;
		self.blueprint.wires = wires;
	}
}

impl PhyNode {
	fn resolve_name(&self, logical: &LogicalDesign) -> &'static str {
		match &logical.get_node(self.logic).function {
			logical_design::NodeFunction::Arithmetic { .. } => "arithmetic-combinator",
			logical_design::NodeFunction::Decider { .. } => "decider-combinator",
			logical_design::NodeFunction::Constant { .. } => "constant-combinator",
			logical_design::NodeFunction::Lamp { .. } => "small-lamp",
			logical_design::NodeFunction::WireSum(_c) => unreachable!(),
		}
	}

	fn resolve_description(&self, logical: &LogicalDesign) -> Option<String> {
		logical.get_node(self.logic).description.clone()
	}

	fn resolve_control_behaviour(&self, logical: &LogicalDesign) -> Option<ControlBehavior> {
		let node = logical.get_node(self.logic);
		match &node.function {
			logical_design::NodeFunction::Arithmetic {
				op,
				input_1,
				input_2,
				input_left_network,
				input_right_network,
			} => Some(ControlBehavior {
				circuit_enabled: None,
				is_on: None,
				arithmetic_conditions: Some(ArithmeticCombinatorParameters {
					first_signal: input_1.resolve_signal_id(),
					second_signal: input_2.resolve_signal_id(),
					first_constant: input_1.resolve_constant(),
					second_constant: input_2.resolve_constant(),
					operation: op.resolve_string(),
					output_signal: node.output[0].resolve_signal_id(),
					first_signal_networks: resolve_network(*input_left_network),
					second_signal_networks: resolve_network(*input_right_network),
				}),
				decider_conditions: None,
				circuit_condition: None,
				sections: None,
				use_color: None,
			}),
			logical_design::NodeFunction::Decider {
				expressions,
				expression_conj_disj,
				use_input_count,
				input_left_networks: input_left_network,
				input_right_networks: input_right_network,
				output_network,
				constants,
			} => Some(ControlBehavior {
				circuit_enabled: None,
				is_on: None,
				arithmetic_conditions: None,
				decider_conditions: Some(DeciderCombinatorParameters {
					conditions: izip!(
						expressions.iter(),
						expression_conj_disj.iter(),
						input_left_network.iter(),
						input_right_network.iter(),
					)
					.enumerate()
					.map(
						|(
							idx,
							(
								(left_signal, operator, right_signal),
								row_operator,
								left_network,
								right_network,
							),
						)| {
							DeciderCombinatorCondition {
								first_signal: left_signal.resolve_signal_id(),
								second_signal: right_signal.resolve_signal_id(),
								constant: right_signal.resolve_constant(),
								comparator: operator.resolve_string(),
								compare_type: if idx == 0 {
									None
								} else {
									row_operator.resolve_string()
								},
								first_signal_networks: resolve_network(*left_network),
								second_signal_networks: resolve_network(*right_network),
							}
						},
					)
					.collect(),
					outputs: izip!(node.output.iter(), output_network.iter(), constants.iter())
						.enumerate()
						.map(
							|(idx, (signal, network, constant))| DeciderCombinatorOutput {
								signal: signal.resolve_signal_id().unwrap(),
								copy_count_from_input: use_input_count[idx],
								networks: resolve_network(*network),
								constant: *constant,
							},
						)
						.collect(),
				}),
				circuit_condition: None,
				sections: None,
				use_color: None,
			}),
			logical_design::NodeFunction::Constant { enabled, constants } => {
				Some(ControlBehavior {
					circuit_enabled: None,
					is_on: Some(*enabled),
					arithmetic_conditions: None,
					decider_conditions: None,
					circuit_condition: None,
					sections: Some(LogisticSections {
						sections: vec![LogisticSection {
							index: 1,
							filters: Some(
								node.output
									.iter()
									.enumerate()
									.map(|(idx, signal)| BlueprintLogisticFilter {
										index: idx as u16 + 1,
										signal: signal.resolve_signal_id(),
										comparator: Some("="),
										count: constants[idx],
										quality: "normal",
									})
									.collect(),
							),
							group: None,
							multiplier: None,
							active: None,
						}],
					}),
					use_color: None,
				})
			},
			logical_design::NodeFunction::Lamp { expression } => Some(ControlBehavior {
				circuit_enabled: Some(true),
				is_on: None,
				arithmetic_conditions: None,
				decider_conditions: None,
				circuit_condition: Some(DeciderCombinatorCondition {
					first_signal: expression.0.resolve_signal_id(),
					second_signal: expression.2.resolve_signal_id(),
					constant: expression.2.resolve_constant(),
					comparator: expression.1.resolve_string(),
					compare_type: None,
					first_signal_networks: None,
					second_signal_networks: None,
				}),
				sections: None,
				use_color: None,
			}),
			logical_design::NodeFunction::WireSum(_c) => unreachable!(),
		}
	}
}

impl Signal {
	fn resolve_signal_id(&self) -> Option<SignalID> {
		match *self {
			Signal::Id(id) => Some(SignalID {
				name: signal_lookup_table::lookup_str(id).0,
				type_: signal_lookup_table::lookup_str(id).1,
			}),
			Signal::Everything => Some(SignalID {
				name: "signal-everything",
				type_: Some("virtual"),
			}),
			Signal::Anything => Some(SignalID {
				name: "signal-anything",
				type_: Some("virtual"),
			}),
			Signal::Each => Some(SignalID {
				name: "signal-each",
				type_: Some("virtual"),
			}),
			Signal::Constant(_) => None,
			Signal::None => None,
		}
	}

	fn resolve_constant(&self) -> Option<i32> {
		match self {
			Signal::Constant(v) => Some(*v),
			Signal::None => Some(0),
			_ => None,
		}
	}
}

impl DeciderRowConjDisj {
	pub fn resolve_string(&self) -> Option<&'static str> {
		match self {
			logical_design::DeciderRowConjDisj::And => Some("and"),
			logical_design::DeciderRowConjDisj::Or => Some("or"),
			logical_design::DeciderRowConjDisj::FirstRow => None,
		}
	}
}

impl DeciderOperator {
	pub fn resolve_string(&self) -> &'static str {
		match self {
			DeciderOperator::LessThan => "<",
			DeciderOperator::GreaterThan => ">",
			DeciderOperator::Equal => "=",
			DeciderOperator::NotEqual => "\u{2260}",
			DeciderOperator::GreaterThanEqual => "\u{2265}",
			DeciderOperator::LessThanEqual => "\u{2264}",
		}
	}
}

impl ArithmeticOperator {
	pub fn resolve_string(&self) -> &'static str {
		match self {
			ArithmeticOperator::Mult => "*",
			ArithmeticOperator::Div => "/",
			ArithmeticOperator::Add => "+",
			ArithmeticOperator::Sub => "-",
			ArithmeticOperator::Mod => "%",
			ArithmeticOperator::Exp => "^",
			ArithmeticOperator::Sll => "<<",
			ArithmeticOperator::Srl => ">>",
			ArithmeticOperator::And => "AND",
			ArithmeticOperator::Or => "OR",
			ArithmeticOperator::Xor => "XOR",
		}
	}
}

fn resolve_network(val: (bool, bool)) -> Option<HashMap<String, bool>> {
	let mut ret = HashMap::new();
	if val.0 && val.1 {
		return None;
	}
	ret.insert("red".to_owned(), val.0);
	ret.insert("green".to_owned(), val.1);
	Some(ret)
}

#[cfg(test)]
mod test {

	use crate::tests;

	use super::*;
	#[test]
	fn new() {
		let mut p = PhysicalDesign::new();
		let mut s = SerializableDesign::new();
		let l = tests::logical_design_tests::get_simple_logical_design();
		p.build_from(&l);
		s.build_from(&p, &l);
		let blueprint_json = serde_json::to_string(&s).unwrap();
		println!("{}", blueprint_json);
	}
}

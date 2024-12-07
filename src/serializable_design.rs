use serde::Serialize;

use crate::logical_design::{self, DeciderOperator, DeciderRowConjDisj, LogicalDesign, Signal};
use crate::physical_design::{Combinator, PhysicalDesign};
use crate::signal_lookup_table;

#[derive(Debug, Clone, Serialize)]
pub struct SerializableDesign {
	item: String,
	entities: Vec<Entity>,
	icons: Vec<()>,
	description: Option<String>,
	version: i64,
	wires: Vec<BlueprintWire>,
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
	#[serde(rename = "type")]
	type_: Option<&'static str>,
}

#[derive(Debug, Clone, Serialize)]
struct Entity {
	entity_number: usize,
	name: &'static str,
	position: Position,
	direction: Option<u32>,
	connections: Option<Connections>,
	neighbours: Option<Vec<usize>>,
	control_behavior: Option<ControlBehavior>,
	variation: Option<i32>,
	switch_state: Option<bool>,
	tags: Option<std::collections::HashMap<String, String>>,
}

#[derive(Debug, Clone, Serialize)]
struct ControlBehavior {
	is_on: Option<bool>,
	arithmetic_conditions: Option<ArithmeticCombinatorParameters>,
	decider_conditions: Option<DeciderCombinatorParameters>,
	sections: Option<LogisticSections>,
	use_color: Option<bool>,
}

#[derive(Debug, Clone, Serialize)]
struct LogisticSections {
	sections: Vec<LogisticSection>,
}

#[derive(Debug, Clone, Serialize)]
struct LogisticSection {
	index: u8,
	filters: Option<Vec<BlueprintLogisticFilter>>,
	group: Option<String>,
	multiplier: Option<f64>,
	active: bool,
}

#[derive(Debug, Clone, Serialize)]
struct BlueprintLogisticFilter {
	index: u16,
	#[serde(flatten)]
	signal: Option<SignalID>,
	comparator: Option<String>,
	count: i32,
}

#[derive(Debug, Clone, Serialize)]
struct ArithmeticCombinatorParameters {
	first_signal: Option<SignalID>,
	second_signal: Option<SignalID>,
	first_constant: Option<i32>,
	second_constant: Option<i32>,
	operation: String,
	output_signal: Option<SignalID>,
}

#[derive(Debug, Clone, Serialize)]
struct DeciderCombinatorParameters {
	conditions: Vec<DeciderCombinatorCondition>,
	outputs: Vec<DeciderCombinatorOutput>,
}

#[derive(Debug, Clone, Serialize)]
struct DeciderCombinatorCondition {
	first_signal: Option<SignalID>,
	second_signal: Option<SignalID>,
	constant: Option<i32>,
	comparator: &'static str,
	compare_type: Option<&'static str>,
}

#[derive(Debug, Clone, Serialize)]
struct DeciderCombinatorOutput {
	signal: SignalID,
	copy_count_from_input: bool,
}

#[derive(Debug, Clone, Serialize)]
struct ProgrammableSpeakerCircuitParameters {}

#[derive(Debug, Clone, Serialize)]
struct Position {
	x: f64,
	y: f64,
}

impl Into<Position> for (f64, f64) {
	fn into(self) -> Position {
		Position {
			x: self.0,
			y: self.1,
		}
	}
}

#[derive(Debug, Clone, Serialize)]
struct Connections {
	#[serde(rename = "1")]
	primary: Option<ConnectionPoint>,
	#[serde(rename = "2")]
	secondary: Option<ConnectionPoint>,
}

#[derive(Debug, Clone, Serialize)]
struct ConnectionPoint {
	red: Option<Vec<ConnectionData>>,
	green: Option<Vec<ConnectionData>>,
}

#[derive(Debug, Clone, Serialize)]
struct ConnectionData {
	entity_id: usize,
	circuit_id: usize,
}

#[derive(Debug, Clone, Serialize)]
struct Color {
	r: f64,
	g: f64,
	b: f64,
	a: Option<f64>,
}

impl SerializableDesign {
	pub fn new() -> Self {
		SerializableDesign {
			item: "blueprint".to_owned(),
			entities: vec![],
			icons: vec![],
			description: Some("v2f compile".to_owned()),
			version: 562949954797573,
			wires: vec![],
		}
	}

	pub fn build_from(&mut self, physical: &PhysicalDesign, logical: &LogicalDesign) {
		let mut entities = vec![];
		physical.for_all_combinators(|comb| {
			entities.push(Entity {
				entity_number: entities.len() + 1,
				name: comb.resolve_name(logical),
				position: comb.position.into(),
				direction: Some(comb.orientation),
				connections: None,
				neighbours: None,
				control_behavior: comb.resolve_control_behaviour(logical),
				variation: todo!(),
				switch_state: todo!(),
				tags: todo!(),
			})
		});
		physical.for_all_poles(|pole| {});
		physical.for_all_wires(|wire| {});
	}
}

impl Combinator {
	fn resolve_name(&self, logical: &LogicalDesign) -> &'static str {
		match &logical.get_node(self.logic).function {
			logical_design::NodeFunction::Arithmetic { .. } => "arithmetic-combinator",
			logical_design::NodeFunction::Decider { .. } => "decide-combinator",
			logical_design::NodeFunction::Constant { .. } => "constant-combinator",
			logical_design::NodeFunction::Lamp { .. } => "small-lamp",
			logical_design::NodeFunction::WireSum => unreachable!(),
		}
	}

	fn resolve_control_behaviour(&self, logical: &LogicalDesign) -> Option<ControlBehavior> {
		let node = logical.get_node(self.logic);
		match &node.function {
			logical_design::NodeFunction::Arithmetic { .. } => None,
			logical_design::NodeFunction::Decider {
				expressions,
				expression_conj_disj,
				use_input_count,
			} => Some(ControlBehavior {
				is_on: None,
				arithmetic_conditions: None,
				decider_conditions: Some(DeciderCombinatorParameters {
					conditions: expressions
						.iter()
						.zip(expression_conj_disj.iter())
						.enumerate()
						.map(
							|(idx, ((left_signal, operator, right_signal), row_operator))| {
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
								}
							},
						)
						.collect(),
					outputs: node
						.output
						.iter()
						.enumerate()
						.map(|(idx, signal)| DeciderCombinatorOutput {
							signal: signal.resolve_signal_id().unwrap(),
							copy_count_from_input: use_input_count[idx],
						})
						.collect(),
				}),
				sections: None,
				use_color: None,
			}),
			logical_design::NodeFunction::Constant { enabled } => None,
			logical_design::NodeFunction::Lamp { expression } => None,
			logical_design::NodeFunction::WireSum => unreachable!(),
		}
	}
}

impl Signal {
	fn resolve_signal_id(&self) -> Option<SignalID> {
		match *self {
			Signal::Virtual(id) => Some(SignalID {
				name: signal_lookup_table::virtual_signal(id),
				type_: Some("virtual"),
			}),
			Signal::Physical(id) => Some(SignalID {
				name: signal_lookup_table::entity_signal(id),
				type_: None,
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
	fn resolve_string(&self) -> Option<&'static str> {
		match self {
			logical_design::DeciderRowConjDisj::And => Some("and"),
			logical_design::DeciderRowConjDisj::Or => Some("or"),
			logical_design::DeciderRowConjDisj::FirstRow => None,
		}
	}
}

impl DeciderOperator {
	fn resolve_string(&self) -> &'static str {
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

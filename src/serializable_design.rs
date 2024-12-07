use serde::Serialize;

use crate::logical_design::LogicalDesign;
use crate::physical_design::PhysicalDesign;

#[derive(Debug, Clone, Serialize)]
struct Blueprint {
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
	name: String,
	#[serde(rename = "type")]
	type_: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
struct Entity {
	entity_number: usize,
	name: String,
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
	comparator: String,
	compare_type: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
struct DeciderCombinatorOutput {
	signal: SignalID,
	copy_count_from_inpit: bool,
}

#[derive(Debug, Clone, Serialize)]
struct ProgrammableSpeakerCircuitParameters {}

#[derive(Debug, Clone, Serialize)]
struct Position {
	x: f64,
	y: f64,
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

impl Serialize for PhysicalDesign {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: serde::Serializer,
	{
		let bp = convert_to_blueprint(self);
		bp.serialize(serializer)
	}
}

fn convert_to_blueprint(design: &PhysicalDesign) -> Blueprint {
	Blueprint {
		item: "blueprint".to_owned(),
		entities: vec![],
		icons: vec![],
		description: Some("v2f compile".to_owned()),
		version: 562949954797573,
		wires: vec![],
	}
}

#[cfg(test)]
mod test {
	#[test]
	fn serialize_simple() {}
}

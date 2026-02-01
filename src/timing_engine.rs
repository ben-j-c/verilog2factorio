use serde::{Deserialize, Serialize};

use crate::{
	logical_design::NodeId,
	util::{HashM, HashS},
};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct Delay(pub i32);

impl Delay {
	pub fn ticks(v: i32) -> Delay {
		Delay(v)
	}

	pub fn seconds(v: i32) -> Delay {
		Delay(v * 60)
	}
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct Edge(pub NodeId, pub Delay);

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct TimingEngine {
	timing_arcs: HashM<NodeId, HashS<Edge>>,
	sink_delay: HashM<NodeId, Delay>,
	source_delay: HashM<NodeId, Delay>,
}

impl TimingEngine {
	pub fn add_arc(&mut self, send: NodeId, recv: NodeId, delay: Delay) {
		assert!(send != recv);
		self.timing_arcs
			.entry(send)
			.or_default()
			.insert(Edge(recv, delay));
	}

	pub fn set_sink_delay(&mut self, node: NodeId, delay: Delay) {
		self.sink_delay.insert(node, delay);
	}

	pub fn set_source_delay(&mut self, node: NodeId, delay: Delay) {
		self.source_delay.insert(node, delay);
	}

	pub fn get_source_delay(&self, node: NodeId) -> Option<&Delay> {
		self.source_delay.get(&node)
	}

	pub fn get_sink_delay(&self, node: NodeId) -> Option<&Delay> {
		self.sink_delay.get(&node)
	}

	pub fn get_arcs(&self, node: NodeId) -> Option<&HashS<Edge>> {
		self.timing_arcs.get(&node)
	}
}

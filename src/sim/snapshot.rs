use serde::Deserialize;

use crate::{
	logical_design::NodeId,
	signal_lookup_table,
	sim::OutputState,
	util::{hash_map, HashM},
};

#[derive(Debug, Clone, Deserialize)]
struct Snapshot {
	#[serde(default)]
	body: Vec<SnapshotBody>,
}

#[derive(Debug, Clone, Deserialize)]
struct SnapshotBody {
	event: Event,
	entities: Vec<Entity>,
	constants: Vec<Constant>,
}

#[derive(Debug, Clone, Deserialize)]
struct Event {
	name: isize,
	tick: isize,
}

#[derive(Debug, Clone, Deserialize)]
struct Entity {
	#[serde(default)]
	signals: Vec<SnapSignal>,
	description: String,
}

#[derive(Debug, Clone, Deserialize)]
struct SnapSignal {
	signal: SignalName,
	count: i32,
}

#[derive(Debug, Clone, Deserialize)]
struct SignalName {
	name: String,
}

#[derive(Debug, Clone, Deserialize)]
struct Constant {
	enabled: bool,
	signal: String,
	count: i32,
	description: String,
}

pub struct GameTrace {
	pub states: Vec<HashM<NodeId, OutputState>>,
}

fn load_snapshot<P: AsRef<std::path::Path>>(path: P) -> Option<Snapshot> {
	let path = path.as_ref();
	let file = std::fs::File::open(path);
	if file.is_err() {
		println!("Error on open: {:?}", path);
		return None;
	}
	let file = file.unwrap();
	let reader = std::io::BufReader::new(file);
	let snapshot = serde_json::from_reader(reader);
	if snapshot.is_err() {
		println!("Error on parse: {:?}", path);
	}
	snapshot.ok()
}

fn load_trace<P: AsRef<std::path::Path>>(path: P) -> GameTrace {
	let snapshot = load_snapshot(path).unwrap();
	let mut body = snapshot.body;
	let mut states = vec![];
	let id_regex = regex::Regex::new("NodeId\\((\\d+)\\)").unwrap();
	for body in body.iter().skip(1) {
		let mut tick = hash_map();
		for ent in &body.entities {
			let matches = id_regex.captures(&ent.description).unwrap();
			let id = NodeId(matches.get(1).unwrap().as_str().parse::<usize>().unwrap());
			let mut row = OutputState::default();
			for signal in &ent.signals {
				let sig = signal_lookup_table::lookup_sig_opt(&signal.signal.name).unwrap();
				row[sig.id()] = signal.count;
			}
			tick.insert(id, row);
		}
		states.push(tick);
	}
	body.pop();
	for body in body.iter().skip(1) {
		let mut tick = hash_map();
		for ent in &body.constants {
			let matches = id_regex.captures(&ent.description).unwrap();
			let id = NodeId(matches.get(1).unwrap().as_str().parse::<usize>().unwrap());
			let mut row = OutputState::default();
			let sig = signal_lookup_table::lookup_sig_opt(&ent.signal).unwrap();
			if ent.enabled {
				row[sig.id()] = ent.count;
			}
			tick.insert(id, row);
		}
		states.push(tick);
	}
	GameTrace { states }
}

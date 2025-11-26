use std::{collections::BTreeMap, io};

use itertools::Itertools;
use vcd::{Value, Vector};

use crate::util::{hash_map, HashM};

type Waveform = BTreeMap<u64, vcd::Vector>;

pub struct VCD {
	last_time: u64,
	header: vcd::Header,
	values: HashM<vcd::IdCode, Waveform>,
}

impl VCD {
	pub fn import<R: io::BufRead>(buf_reader: R) -> VCD {
		let mut parser = vcd::Parser::new(buf_reader);
		let header = parser.parse_header().unwrap();
		let mut values = hash_map();
		let mut time = 0;
		for command_result in parser {
			let command = if let Ok(command) = command_result {
				command
			} else {
				continue;
			};
			use vcd::Command::*;
			match command {
				ChangeScalar(id_code, value) => {
					let waveform = values.entry(id_code).or_insert(Waveform::new());
					waveform.insert(time, Vector::from([value]));
				},
				ChangeVector(id_code, vector) => {
					let waveform = values.entry(id_code).or_insert(Waveform::new());
					waveform.insert(time, vector);
				},
				Timestamp(t) => {
					time = t;
				},
				_ => {},
			}
		}
		VCD {
			header,
			values,
			last_time: time,
		}
	}

	pub fn has_var<S: AsRef<str>>(&self, var: S) -> bool {
		let mut path = var.as_ref().split(".").map(str::to_owned).collect_vec();
		for p in &mut path {
			if p.starts_with("$") {
				*p = "\\".to_owned() + p;
			}
		}
		self.header.find_var(&path).is_some()
	}

	pub fn get_value<S: AsRef<str>>(&self, var: S, time: u64) -> Option<Vec<Value>> {
		let var = self
			.header
			.find_var(&var.as_ref().split(".").collect_vec())?;
		let id = var.code;
		let waveform = self.values.get(&id)?;
		waveform
			.range(0..=time)
			.last()
			.map(|(_k, v)| v.clone().into())
	}

	pub fn last_time(&self) -> u64 {
		self.last_time
	}
}

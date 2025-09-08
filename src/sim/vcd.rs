use std::{collections::BTreeMap, io};

use itertools::Itertools;
use vcd::{Value, Vector};

use crate::util::{hash_map, HashM};

type Waveform = BTreeMap<u64, vcd::Vector>;

pub struct VCD {
	header: vcd::Header,
	values: HashM<vcd::IdCode, Waveform>,
}

impl VCD {
	fn import<R: io::BufRead>(buf_reader: R) -> VCD {
		let mut parser = vcd::Parser::new(buf_reader);
		let header = parser.parse_header().unwrap();
		let mut values = hash_map();
		let mut time = 0;
		for command_result in parser {
			let command = command_result.unwrap();
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
		VCD { header, values }
	}

	pub fn get_value(&self, var: String, time: u64) -> Option<Vec<Value>> {
		let var = self.header.find_var(&var.split(".").collect_vec())?;
		let id = var.code;
		let waveform = self.values.get(&id)?;
		waveform
			.range(0..=time)
			.last()
			.map(|(_k, v)| v.clone().into())
	}
}

use std::{collections::BTreeMap, io};

use crate::util::{hash_map, HashM};

pub enum Waveform {
	Value(BTreeMap<u64, vcd::Value>),
	Vector(BTreeMap<u64, vcd::Vector>),
}

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
					let waveform = values
						.entry(id_code)
						.or_insert(Waveform::Value(BTreeMap::new()));
					match waveform {
						Waveform::Value(values) => {
							values.insert(time, value);
						},
						Waveform::Vector(_) => panic!("Inconsistent waveform."),
					}
				},
				ChangeVector(id_code, vector) => {
					let waveform = values
						.entry(id_code)
						.or_insert(Waveform::Vector(BTreeMap::new()));
					match waveform {
						Waveform::Vector(values) => {
							values.insert(time, vector);
						},
						Waveform::Value(_) => panic!("Inconsistent waveform."),
					}
				},
				Timestamp(t) => {
					time = t;
				},
				_ => {},
			}
		}
		VCD { header, values }
	}
}

use std::{collections::HashMap, hash::Hash};

use serde::{
	de::{self, Visitor},
	Deserialize, Deserializer,
};

type CellType = String;
type ModelName = String;
type PortName = String;
type Attribute = String;
type Parameter = String;
type AttributeName = String;
type ParameterName = String;

#[derive(Deserialize, Debug)]
pub struct MappedDesign {
	creator: String,
	#[serde(default)]
	modules: HashMap<String, Module>,
	#[serde(default)]
	models: HashMap<String, Model>,
}

#[derive(Deserialize, Debug)]
pub struct Model {}

#[derive(Deserialize, Debug)]
pub struct Module {
	#[serde(default)]
	attributes: HashMap<String, String>,
	#[serde(default)]
	ports: HashMap<String, Port>,
	#[serde(default)]
	cells: HashMap<String, Cell>,
	#[serde(default)]
	memories: HashMap<String, Memory>,
	#[serde(default)]
	netnames: HashMap<String, Net>,
}

#[derive(Deserialize, Debug)]
pub struct Port {
	direction: Direction,
	#[serde(default)]
	bits: Vec<Bit>,
	#[serde(default)]
	offset: isize,
	#[serde(default)]
	signed: i32,
	#[serde(default)]
	upto: i32,
}

#[derive(Debug)]
pub enum Bit {
	Zero,
	One,
	Id(BitId),
}

#[derive(Deserialize, Debug)]
pub struct BitId(pub u64);

#[derive(Deserialize, Debug)]
#[serde(rename_all = "lowercase")]
pub enum Direction {
	Input,
	Output,
	Inout,
}

#[derive(Deserialize, Debug)]
pub struct Cell {
	hide_name: i32,
	#[serde(rename = "type")]
	cell_type: CellType,
	#[serde(default)]
	model: ModelName,
	#[serde(default)]
	parameters: HashMap<ParameterName, Parameter>,
	#[serde(default)]
	attributes: HashMap<AttributeName, Attribute>,
	#[serde(default)]
	port_directions: HashMap<PortName, Direction>,
	#[serde(default)]
	connections: HashMap<PortName, Vec<Bit>>,
}

#[derive(Deserialize, Debug)]
pub struct Memory {
	hide_name: i32,
	#[serde(default)]
	attributes: HashMap<AttributeName, Attribute>,
	width: i32,
	start_offset: u64,
	size: u64,
}

#[derive(Deserialize, Debug)]
pub struct Net {
	hide_name: i32,
	#[serde(default)]
	attributes: HashMap<AttributeName, Attribute>,
	bits: Vec<Bit>,
	#[serde(default)]
	offset: isize,
	#[serde(default)]
	upto: i32,
	#[serde(default)]
	signed: i32,
}

impl<'de> Deserialize<'de> for Bit {
	fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
	where
		D: Deserializer<'de>,
	{
		struct BitVisitor;
		impl<'de> Visitor<'de> for BitVisitor {
			type Value = Bit;

			fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
				formatter.write_str(r#"a string "0", "1", or an integer >= 2"#)
			}

			fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
			where
				E: de::Error,
			{
				match value {
					"0" => Ok(Bit::Zero),
					"1" => Ok(Bit::One),
					_ => Err(E::custom(format!(
						"Invalid string value for BitId: {}",
						value
					))),
				}
			}

			fn visit_u64<E>(self, value: u64) -> Result<Self::Value, E>
			where
				E: de::Error,
			{
				if value >= 2 {
					Ok(Bit::Id(BitId(value)))
				} else {
					Err(E::custom(format!(
						"Invalid integer value for BitId: {} (must be >= 2)",
						value
					)))
				}
			}
		}

		deserializer.deserialize_any(BitVisitor)
	}
}

trait FromBinStr {
	fn from_bin_str(&self) -> Option<usize>;
}

impl FromBinStr for String {
	fn from_bin_str(&self) -> Option<usize> {
		let mut retval = 0;
		for x in self.chars() {
			retval <<= 1;
			retval += if x == '1' { 1 } else { 0 };
			if x != '1' && x != '0' {
				return None;
			}
		}
		Some(retval)
	}
}

#[cfg(test)]
mod test {
	use std::{fs::File, io::BufReader};

	use super::*;

	#[test]
	fn design_test1() {
		let file = File::open("./test_designs/output/test1.json").unwrap();
		let reader = BufReader::new(file);
		let mapped_design: MappedDesign = serde_json::from_reader(reader).unwrap();
		let test1 = mapped_design.modules.get("test1");
		assert!(test1.is_some());
		let test1 = test1.unwrap();
		println!("{:?}", test1);
		assert!(test1.ports.contains_key("signal_0"));
		assert!(test1.ports.contains_key("signal_1"));
		assert!(test1.ports.contains_key("signal_2"));
		assert_eq!(test1.attributes["top"].from_bin_str(), Some(1));
		for (_cell_name, cell) in test1.cells.iter() {
			assert_eq!(cell.cell_type, "$mul");
			assert_eq!(cell.parameters["A_SIGNED"].from_bin_str(), Some(1));
			assert_eq!(cell.parameters["B_SIGNED"].from_bin_str(), Some(1));
		}
	}
}

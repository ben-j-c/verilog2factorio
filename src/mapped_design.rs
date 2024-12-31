use std::collections::HashMap;

use itertools::Itertools;
use serde::{
	de::{self, Visitor},
	Deserialize, Deserializer,
};

pub type CellType = String;
pub type ModelName = String;
pub type PortName = String;
pub type Attribute = String;
pub type Parameter = String;
pub type AttributeName = String;
pub type ParameterName = String;

#[allow(dead_code)]
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

#[allow(dead_code)]
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

#[allow(dead_code)]
#[derive(Deserialize, Debug)]
pub struct Port {
	pub direction: Direction,
	#[serde(default)]
	pub bits: Vec<Bit>,
	#[serde(default)]
	pub offset: isize,
	#[serde(default)]
	pub signed: i32,
	#[serde(default)]
	pub upto: i32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Bit {
	Zero,
	One,
	Id(BitId),
}

impl Bit {
	pub fn is_constant(&self) -> bool {
		match self {
			Bit::Zero => true,
			Bit::One => true,
			Bit::Id(bit_id) => false,
		}
	}

	pub fn is_connection(&self) -> bool {
		!self.is_constant()
	}
}

pub trait Integer: Copy + PartialOrd + Sized {
	fn zero() -> Self;
	fn one() -> Self;
	fn sll(self) -> Self;
	fn inc(self) -> Self;
}

macro_rules! impl_integer {
	($($t:ty)*) => {
		$(impl Integer for $t {
			fn zero() -> Self { 0 }
			fn one() -> Self { 1 }
			fn sll(self) -> Self { self << 1 }
			fn inc(self) -> Self { self +  1}
		})*
	};
}

impl_integer!(i8 u8 i16 u16 i32 u32 i64 u64 i128 u128 isize usize);

pub trait BitSliceOps {
	fn is_all_constants(&self) -> bool;
	fn is_all_connections(&self) -> bool;
	fn get_sections(&self) -> Vec<Vec<Bit>>;
	fn get_constant<T>(&self) -> T
	where
		T: Integer;
}

impl BitSliceOps for Vec<Bit> {
	fn is_all_constants(&self) -> bool {
		self.iter().all(|x| x.is_constant())
	}

	fn is_all_connections(&self) -> bool {
		self.iter().all(|x| x.is_connection())
	}

	fn get_sections(&self) -> Vec<Vec<Bit>> {
		let mut retval = vec![];
		let mut section_start = 0;
		for i in 1..self.len() {
			if self[section_start].is_constant() == self[i].is_constant() {
				continue;
			} else {
				retval.push(
					self[section_start..i]
						.iter()
						.map(|bit| bit.clone())
						.collect_vec(),
				);
				section_start = i;
			}
		}
		retval.push(self[section_start..self.len()].to_vec());
		retval
	}

	fn get_constant<T>(&self) -> T
	where
		T: Integer,
	{
		assert!(
			self.is_all_constants(),
			"Can't get constant value from a bit vector that represents connections between nodes."
		);
		let mut retval = T::zero();
		for bit in self.iter().rev() {
			match bit {
				Bit::Zero => retval = retval.sll(),
				Bit::One => retval = retval.sll().inc(),
				Bit::Id(_) => unreachable!(),
			}
		}
		retval
	}
}

#[derive(Deserialize, Debug, Clone, Copy, PartialEq, Eq)]
pub struct BitId(pub u64);

#[derive(Deserialize, Debug, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum Direction {
	Input,
	Output,
	Inout,
}

#[allow(dead_code)]
#[derive(Deserialize, Debug)]
pub struct Cell {
	pub hide_name: i32,
	#[serde(rename = "type")]
	pub cell_type: CellType,
	#[serde(default)]
	pub model: ModelName,
	#[serde(default)]
	pub parameters: HashMap<ParameterName, Parameter>,
	#[serde(default)]
	pub attributes: HashMap<AttributeName, Attribute>,
	#[serde(default)]
	pub port_directions: HashMap<PortName, Direction>,
	#[serde(default)]
	pub connections: HashMap<PortName, Vec<Bit>>,
}

#[allow(dead_code)]
#[derive(Deserialize, Debug)]
pub struct Memory {
	hide_name: i32,
	#[serde(default)]
	attributes: HashMap<AttributeName, Attribute>,
	width: i32,
	start_offset: u64,
	size: u64,
}

#[allow(dead_code)]
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

pub(crate) trait FromBinStr {
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

impl MappedDesign {
	pub fn for_all_top_level_io<F>(&self, mut func: F)
	where
		F: FnMut(&Self, &str, &Port),
	{
		for module in self.modules.values() {
			if let Some(is_top) = module.attributes.get("top") {
				if is_top.from_bin_str() == Some(1) {
					for (port_name, port) in &module.ports {
						func(self, port_name, port);
					}
					return;
				}
			}
		}
		panic!("No module was identified as the top level design");
	}

	pub fn for_all_cells<F>(&self, mut func: F)
	where
		F: FnMut(&Self, &str, &Cell),
	{
		for module in self.modules.values() {
			if let Some(is_top) = module.attributes.get("top") {
				if is_top.from_bin_str() == Some(1) {
					for (cell_name, cell) in &module.cells {
						func(self, cell_name, cell)
					}
					return;
				}
			}
		}
		panic!("No module was identified as the top level design");
	}

	pub fn get_cell<'a>(&'a self, cell_name: &str) -> &'a Cell {
		for module in self.modules.values() {
			if let Some(is_top) = module.attributes.get("top") {
				if is_top.from_bin_str() == Some(1) {
					return module.cells.get(cell_name).unwrap();
				}
			}
		}
		panic!("No module was identified as the top level design");
	}

	pub fn get_port<'a>(&'a self, port_name: &str) -> &'a Port {
		for module in self.modules.values() {
			if let Some(is_top) = module.attributes.get("top") {
				if is_top.from_bin_str() == Some(1) {
					return module.ports.get(port_name).unwrap();
				}
			}
		}
		panic!("No module was identified as the top level design");
	}
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

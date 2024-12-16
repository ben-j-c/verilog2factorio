use std::collections::HashMap;

use clap::builder::Str;
use serde::{
	de::{self, Visitor},
	Deserialize, Deserializer,
};

#[derive(Deserialize)]
pub struct MappedDesign {
	creator: String,
	modules: HashMap<String, Module>,
	models: HashMap<String, Model>,
}

#[derive(Deserialize)]
pub struct Model {}

#[derive(Deserialize)]
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

#[derive(Deserialize)]
pub struct Port {
	direction: Direction,
	#[serde(default)]
	bits: Vec<Bit>,
	#[serde(default)]
	offset: Option<isize>,
	#[serde(default)]
	signed: Option<usize>,
}

pub enum Bit {
	Zero,
	One,
	Id(BitId),
}

#[derive(Deserialize)]
pub struct BitId(pub u64);

#[derive(Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Direction {
	Input,
	Output,
	Inout,
}

#[derive(Deserialize)]
pub struct Cell {}

#[derive(Deserialize)]
pub struct Memory {}

#[derive(Deserialize)]
pub struct Net {}

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

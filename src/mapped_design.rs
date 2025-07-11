use itertools::{chain, izip, Itertools};
use rayon::collections::hash_map;
use serde::{
	de::{self, Visitor},
	Deserialize, Deserializer,
};

use crate::{
	checked_design::{BodyType, ImplementableOp},
	util::{hash_map, HashM},
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
	modules: HashM<String, Module>,
	#[serde(default)]
	models: HashM<String, Model>,
}

#[derive(Deserialize, Debug)]
pub struct Model {}

#[allow(dead_code)]
#[derive(Deserialize, Debug)]
pub struct Module {
	#[serde(default)]
	attributes: HashM<String, String>,
	#[serde(default)]
	ports: HashM<String, Port>,
	#[serde(default)]
	cells: HashM<String, Cell>,
	#[serde(default)]
	memories: HashM<String, Memory>,
	#[serde(default)]
	netnames: HashM<String, Net>,
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
			Bit::Id(_bit_id) => false,
		}
	}

	pub fn is_connection(&self) -> bool {
		!self.is_constant()
	}

	pub fn id_unwrap(&self) -> &BitId {
		if let Bit::Id(id) = self {
			id
		} else {
			panic!("Unwrapped a non-id bit");
		}
	}

	pub fn bool_unwrap(&self) -> bool {
		match self {
			Bit::Zero => false,
			Bit::One => true,
			Bit::Id(_) => panic!("Unwrapped a connection as a constant."),
		}
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
			fn sll(self) -> Self { self << Self::one() }
			fn inc(self) -> Self { self +  Self::one()}
		})*
	};
}

impl_integer!(i8 u8 i16 u16 i32 u32 i64 u64 i128 u128 isize usize);

pub trait BitSliceOps {
	fn is_all_constants(&self) -> bool;
	fn get_constant<T>(&self) -> T
	where
		T: Integer;
}

impl BitSliceOps for Vec<Bit> {
	fn is_all_constants(&self) -> bool {
		self.iter().all(|x| x.is_constant())
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

#[derive(Deserialize, Debug, PartialEq, Eq, Clone, Copy)]
#[serde(rename_all = "lowercase")]
pub enum Direction {
	Input,
	Output,
	Inout,
}

#[derive(Deserialize, Debug)]
pub struct MappedCell {
	pub hide_name: i32,
	#[serde(rename = "type")]
	pub cell_type: CellType,
	#[serde(default)]
	pub model: ModelName,
	#[serde(default)]
	pub parameters: HashM<ParameterName, Parameter>,
	#[serde(default)]
	pub attributes: HashM<AttributeName, Attribute>,
	#[serde(default)]
	pub port_directions: HashM<PortName, Direction>,
	#[serde(default)]
	pub connections: HashM<PortName, Vec<Bit>>,
}

#[derive(Debug)]
pub struct Cell {
	pub hide_name: i32,
	pub cell_type: ImplementableOp,
	pub model: ModelName,
	pub parameters: HashM<ParameterName, Parameter>,
	pub attributes: HashM<AttributeName, Attribute>,
	pub port_directions: HashM<PortName, Direction>,
	pub connections: HashM<PortName, Vec<Bit>>,
}

#[allow(dead_code)]
#[derive(Deserialize, Debug)]
pub struct Memory {
	hide_name: i32,
	#[serde(default)]
	attributes: HashM<AttributeName, Attribute>,
	width: i32,
	start_offset: u64,
	size: u64,
}

#[allow(dead_code)]
#[derive(Deserialize, Debug)]
pub struct Net {
	hide_name: i32,
	#[serde(default)]
	attributes: HashM<AttributeName, Attribute>,
	bits: Vec<Bit>,
	#[serde(default)]
	offset: isize,
	#[serde(default)]
	upto: i32,
	#[serde(default)]
	signed: i32,
}

impl Cell {
	/// Defines the canonical ordering of ports in fanin/fanout.
	pub(crate) fn get_terminal_names(&self) -> Vec<String> {
		match self.cell_type.get_body_type() {
			BodyType::ABY => vec!["A".to_owned(), "B".to_owned(), "Y".to_owned()],
			BodyType::AY => vec!["A".to_owned(), "Y".to_owned()],
			BodyType::MultiPart => match &self.cell_type {
				ImplementableOp::DFF => vec!["D".to_owned(), "CLK".to_owned(), "Q".to_owned()],
				ImplementableOp::Swizzle => {
					unreachable!("Imaginary cell encountered before it should be created.")
				},
				ImplementableOp::LUT(n) => (0..*n)
					.map(|i| format!("A{}", i))
					.chain(vec!["Y".to_owned()])
					.collect_vec(),
				ImplementableOp::PMux(full_case, s_width) => chain!(
					if *full_case {
						None
					} else {
						Some("A".to_owned())
					},
					(0..*s_width).map(|i| format!("B{}", i)),
					(0..*s_width).map(|i| format!("S{}", i)),
					["Y".to_owned()]
				)
				.collect_vec(),
				ImplementableOp::Memory => {
					let n_rd_ports = self.parameters["RD_PORTS"].unwrap_bin_str();
					let n_wr_ports = self.parameters["WR_PORTS"].unwrap_bin_str();
					let mut ports = vec![];
					for i in 0..n_rd_ports {
						let expected = [
							format!("RD_ADDR_{i}"),
							format!("RD_DATA_{i}"),
							format!("RD_CLK_{i}"),
							format!("RD_EN_{i}"),
							format!("RD_ARST_{i}"),
							format!("RD_SRST_{i}"),
						];
						for port in expected {
							if self.port_directions.contains_key(&port) {
								ports.push(port);
							}
						}
					}
					for i in 0..n_wr_ports {
						let expected = [
							format!("WR_ADDR_{i}"),
							format!("WR_DATA_{i}"),
							format!("WR_CLK_{i}"),
							format!("WR_EN_{i}"),
						];
						for port in expected {
							if self.port_directions.contains_key(&port) {
								ports.push(port);
							}
						}
					}
					ports
				},
				ImplementableOp::ReduceAnd => {
					let width = self.parameters["A_WIDTH"].unwrap_bin_str();
					chain!((0..width).map(|i| format!("A{i}")), ["Y".to_owned()]).collect_vec()
				},
				ImplementableOp::ReduceOr => {
					let width = self.parameters["A_WIDTH"].unwrap_bin_str();
					chain!((0..width).map(|i| format!("A{i}")), ["Y".to_owned()]).collect_vec()
				},
				_ => unreachable!(),
			},
			BodyType::Constant { .. } => vec!["Y".to_owned()],
			BodyType::Nop => vec!["A".to_owned(), "Y".to_owned()],
		}
	}
}

pub(crate) trait FromBinStr {
	fn from_bin_str(&self) -> Option<usize>;
	fn unwrap_bin_str(&self) -> usize;
}

pub(crate) trait IntoBoolVec {
	fn into_bool_vec(&self) -> Option<Vec<bool>>;
}

impl IntoBoolVec for String {
	fn into_bool_vec(&self) -> Option<Vec<bool>> {
		let mut retval = vec![];
		for x in self.chars() {
			if x != '1' && x != '0' {
				return None;
			}
			if x == '1' {
				retval.push(true);
			} else {
				retval.push(false);
			}
		}
		Some(retval)
	}
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

	fn unwrap_bin_str(&self) -> usize {
		self.from_bin_str().expect("String is not correct format.")
	}
}

impl MappedDesign {
	pub fn max_bit(&self) -> u64 {
		let mut max_bit = 1;
		self.for_all_top_level_io(|_, _, p| {
			for bit in &p.bits {
				if let Bit::Id(id) = bit {
					max_bit = max_bit.max(id.0)
				}
			}
		});
		self.for_all_cells(|_, _, c| {
			for bits in c.connections.values() {
				for bit in bits {
					if let Bit::Id(id) = bit {
						max_bit = max_bit.max(id.0)
					}
				}
			}
		});
		max_bit
	}

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

	pub fn get_cell_option<'a>(&'a self, cell_name: &str) -> Option<&'a Cell> {
		for module in self.modules.values() {
			if let Some(is_top) = module.attributes.get("top") {
				if is_top.from_bin_str() == Some(1) {
					return module.cells.get(cell_name);
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

	pub(crate) fn get_top_source(&self) -> String {
		for module in self.modules.values() {
			if let Some(is_top) = module.attributes.get("top") {
				if is_top.from_bin_str() == Some(1) {
					return module.attributes["src"].clone();
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
		impl Visitor<'_> for BitVisitor {
			type Value = Bit;

			fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
				formatter.write_str(r#"a string "0", "1", "x", "z", or an integer >= 2"#)
			}

			fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
			where
				E: de::Error,
			{
				match value {
					"0" | "x" | "z" => Ok(Bit::Zero),
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

impl<'de> Deserialize<'de> for Cell {
	fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
	where
		D: Deserializer<'de>,
	{
		let helper = MappedCell::deserialize(deserializer)?;

		let op = match helper.cell_type.as_str() {
			"v2f_and" => ImplementableOp::AndBitwise,
			"v2f_or" => ImplementableOp::OrBitwise,
			"v2f_xor" => ImplementableOp::XorBitwise,
			"v2f_shl" => ImplementableOp::Shl,
			"v2f_shr" => ImplementableOp::Shr,
			"v2f_mul" => ImplementableOp::Mul,
			"v2f_div" => ImplementableOp::Div,
			"v2f_mod" => ImplementableOp::Mod,
			"v2f_pow" => ImplementableOp::Pow,
			"v2f_add" => ImplementableOp::Add,
			"v2f_sub" => ImplementableOp::Sub,
			"v2f_gt" => ImplementableOp::GreaterThan,
			"v2f_lt" => ImplementableOp::LessThan,
			"v2f_eq" => ImplementableOp::Equal,
			"v2f_ne" => ImplementableOp::NotEqual,
			"v2f_ge" => ImplementableOp::GreaterThanEqual,
			"v2f_le" => ImplementableOp::LessThanEqual,
			"v2f_reduce_or" => ImplementableOp::Add,
			"v2f_reduce_and" => ImplementableOp::Add,
			"v2f_rolling_accumulate" => ImplementableOp::V2FRollingAccumulate,
			"v2f_neg" => ImplementableOp::Neg,
			"v2f_pmux" => ImplementableOp::PMux(false, 0),
			"$dff" => ImplementableOp::DFF,
			"$swizzle" => unreachable!("This is a fake op, we don't accept it in a design."),
			"$lut" => ImplementableOp::LUT(0),
			"$mem_v2" => ImplementableOp::Memory,
			_ => panic!("Can't implement this operation."),
		};

		if helper.cell_type == *"$mem_v2" {
			Ok(convert_mem_v2(helper))
		} else if helper.cell_type == *"$lut" {
			Ok(convert_lut_ports(helper))
		} else if helper.cell_type == *"v2f_pmux" {
			Ok(convert_pmux_ports(helper))
		} else if helper.cell_type.starts_with("v2f_reduce") {
			Ok(convert_reduce_x_ports(helper))
		} else {
			Ok(Cell {
				hide_name: helper.hide_name,
				cell_type: op,
				model: helper.model,
				parameters: helper.parameters,
				attributes: helper.attributes,
				port_directions: helper.port_directions,
				connections: helper.connections,
			})
		}
	}
}

fn convert_mem_v2(cell: MappedCell) -> Cell {
	let no_init = !cell.parameters["INIT"].is_empty();
	let valid_reg_init = cell.parameters["RD_INIT_VALUE"]
		.chars()
		.all(|c| c == '0' || c == 'x');
	let valid_reg_init = valid_reg_init
		&& cell.parameters["RD_SRST_VALUE"]
			.chars()
			.all(|c| c == '0' || c == 'x');
	let abits = cell.parameters["ABITS"].unwrap_bin_str();
	let width = cell.parameters["WIDTH"].unwrap_bin_str();
	let wr_port_count = cell.parameters["WR_PORTS"].unwrap_bin_str();
	assert!(
		!cell.parameters["RD_TRANSPARENCY_MASK"].contains("1"),
		"Currently memory doesn't support transparent reads."
	);
	assert!(
		wr_port_count == 0 || no_init,
		"The game doesn't support initial values for writeable memories. {:?}",
		cell.attributes["src"]
	);
	assert!(
		valid_reg_init,
		"The game doesn't support initial values for registers. {:?}",
		cell.attributes["src"]
	);
	assert!(
		width <= 32,
		"The game doesn't support port widths larger than 32. {:?}",
		cell.attributes["src"]
	);
	assert!(
		abits <= 32,
		"The game doesn't support port widths larger than 32. {:?}",
		cell.attributes["src"]
	);

	let tmp1 = cell.connections["RD_ADDR"].iter().copied().chunks(abits);
	let tmp2 = cell.connections["RD_DATA"].iter().copied().chunks(width);
	let rd_ports = izip!(
		tmp1.into_iter().map(|c| c.collect_vec()),
		tmp2.into_iter().map(|c| c.collect_vec()),
		cell.connections["RD_ARST"].iter(),
		cell.connections["RD_CLK"].iter(),
		cell.connections["RD_EN"].iter(),
		cell.connections["RD_SRST"].iter(),
	)
	.collect_vec();
	assert_eq!(rd_ports.len(), cell.parameters["RD_PORTS"].unwrap_bin_str());

	let tmp1 = cell.connections["WR_ADDR"].iter().copied().chunks(abits);
	let tmp2 = cell.connections["WR_DATA"].iter().copied().chunks(width);
	let wr_ports = izip!(
		tmp1.into_iter().map(|c| c.collect_vec()),
		tmp2.into_iter().map(|c| c.collect_vec()),
		cell.connections["WR_CLK"].iter(),
		cell.connections["WR_EN"]
			.iter()
			.chunks(width)
			.into_iter()
			.map(|c| {
				let tmp3 = c.collect_vec();
				assert!(
					tmp3.iter().all(|v| *v == tmp3[0]),
					"We currently don't support bit masks."
				);
				tmp3[0]
			}),
	)
	.collect_vec();
	assert_eq!(wr_ports.len(), cell.parameters["WR_PORTS"].unwrap_bin_str());

	let mut directions = hash_map();
	let mut connections: HashM<String, Vec<Bit>> = hash_map();
	for i in 0..rd_ports.len() {
		let (addr, data, arst, clk, en, srst) = &rd_ports[i];
		directions.insert(format!("RD_ADDR_{}", i), Direction::Input);
		connections.insert(format!("RD_ADDR_{}", i), addr.clone());
		directions.insert(format!("RD_DATA_{}", i), Direction::Output);
		connections.insert(format!("RD_DATA_{}", i), data.clone());
		if clk.is_connection() {
			directions.insert(format!("RD_CLK_{}", i), Direction::Input);
			connections.insert(format!("RD_CLK_{}", i), vec![**clk]);
		}
		if en.is_connection() {
			directions.insert(format!("RD_EN_{}", i), Direction::Input);
			connections.insert(format!("RD_EN_{}", i), vec![**en]);
		}
		if arst.is_connection() {
			directions.insert(format!("RD_ARST_{}", i), Direction::Input);
			connections.insert(format!("RD_ARST_{}", i), vec![**arst]);
		}
		if srst.is_connection() {
			directions.insert(format!("RD_SRST_{}", i), Direction::Input);
			connections.insert(format!("RD_SRST_{}", i), vec![**arst]);
		}
	}

	for i in 1..wr_ports.len() {
		let (_addr, _data, clk0, _en) = &wr_ports[i - 1];
		let (_addr, _data, clk1, _en) = &wr_ports[i];
		assert!(clk0 == clk1, "Can only have a single writer clock.");
	}

	for i in 0..wr_ports.len() {
		let (addr, data, clk, en) = &wr_ports[i];
		directions.insert(format!("WR_ADDR_{}", i), Direction::Input);
		connections.insert(format!("WR_ADDR_{}", i), addr.clone());
		directions.insert(format!("WR_DATA_{}", i), Direction::Input);
		connections.insert(format!("WR_DATA_{}", i), data.clone());
		if i == 0 {
			directions.insert(format!("WR_CLK_{}", i), Direction::Input);
			connections.insert(format!("WR_CLK_{}", i), vec![**clk]);
		}
		if en.is_connection() {
			directions.insert(format!("WR_EN_{}", i), Direction::Input);
			connections.insert(format!("WR_EN_{}", i), vec![**en]);
		}
	}

	Cell {
		hide_name: cell.hide_name,
		cell_type: ImplementableOp::Memory,
		model: cell.model,
		parameters: cell.parameters,
		attributes: cell.attributes,
		port_directions: directions,
		connections,
	}
}

// Mark of shame.
// Wow I have the foresight of a goldfish. I should have really looked at what a LUT is mapped at before I made that huge ass commit that assumes LUTs will have single bit ports. It's like wow how could this happen to me?! Good luck for me that I didn't have this option and didn't need to do some global bullshit or you would see yet another XXX_design.rs file to fix myopic me.
fn convert_lut_ports(pre_mapped: MappedCell) -> Cell {
	let mut new_connections = hash_map();
	let mut new_directions = hash_map();
	assert!(pre_mapped.connections.contains_key("A"));
	assert!(pre_mapped.connections.contains_key("Y"));
	assert!(pre_mapped.port_directions.contains_key("A"));
	assert!(pre_mapped.port_directions.contains_key("Y"));
	assert_eq!(pre_mapped.port_directions["A"], Direction::Input);
	assert_eq!(pre_mapped.port_directions["Y"], Direction::Output);
	for (idx, bit) in pre_mapped.connections["A"].iter().enumerate() {
		new_connections.insert(format!("A{}", idx), vec![*bit]);
		new_directions.insert(format!("A{}", idx), pre_mapped.port_directions["A"]);
	}
	new_connections.insert("Y".to_owned(), pre_mapped.connections["Y"].clone());
	new_directions.insert("Y".to_owned(), Direction::Output);
	Cell {
		hide_name: pre_mapped.hide_name,
		cell_type: ImplementableOp::LUT(pre_mapped.parameters["WIDTH"].unwrap_bin_str()),
		model: pre_mapped.model,
		parameters: pre_mapped.parameters,
		attributes: pre_mapped.attributes,
		port_directions: new_directions,
		connections: new_connections,
	}
}

fn convert_pmux_ports(pre_mapped: MappedCell) -> Cell {
	let width = pre_mapped.parameters["WIDTH"].unwrap_bin_str();
	let mut new_connections = hash_map();
	let mut new_directions = hash_map();
	assert!(pre_mapped.connections.contains_key("A"));
	assert!(pre_mapped.connections.contains_key("B"));
	assert!(pre_mapped.connections.contains_key("S"));
	assert!(pre_mapped.connections.contains_key("Y"));
	assert!(pre_mapped.port_directions.contains_key("A"));
	assert!(pre_mapped.port_directions.contains_key("B"));
	assert!(pre_mapped.port_directions.contains_key("S"));
	assert!(pre_mapped.port_directions.contains_key("Y"));
	assert_eq!(pre_mapped.port_directions["A"], Direction::Input);
	assert_eq!(pre_mapped.port_directions["B"], Direction::Input);
	assert_eq!(pre_mapped.port_directions["S"], Direction::Input);
	assert_eq!(pre_mapped.port_directions["Y"], Direction::Output);
	for (idx, chunk) in pre_mapped.connections["B"]
		.iter()
		.chunks(width)
		.into_iter()
		.enumerate()
		.take(pre_mapped.parameters["S_WIDTH"].unwrap_bin_str())
	{
		let bits = chunk.copied().collect_vec();
		assert_eq!(
			bits.len(),
			width,
			"Width of port not multiple of pmux width."
		);
		new_connections.insert(format!("B{}", idx), bits);
		new_directions.insert(format!("B{}", idx), Direction::Input);
	}
	for (idx, bit) in pre_mapped.connections["S"].iter().enumerate() {
		new_connections.insert(format!("S{}", idx), vec![*bit]);
		new_directions.insert(format!("S{}", idx), Direction::Input);
	}
	let full_case = pre_mapped
		.attributes
		.get("full_case")
		.map(|full_case| full_case.from_bin_str() == Some(1))
		.unwrap_or_default();
	let s_width = pre_mapped.parameters["S_WIDTH"].unwrap_bin_str();
	if !full_case {
		new_connections.insert("A".to_string(), pre_mapped.connections["A"].clone());
		new_directions.insert("A".to_string(), Direction::Input);
	}
	new_connections.insert("Y".to_owned(), pre_mapped.connections["Y"].clone());
	new_directions.insert("Y".to_owned(), Direction::Output);
	Cell {
		hide_name: pre_mapped.hide_name,
		cell_type: ImplementableOp::PMux(full_case, s_width),
		model: pre_mapped.model,
		parameters: pre_mapped.parameters,
		attributes: pre_mapped.attributes,
		port_directions: new_directions,
		connections: new_connections,
	}
}

fn convert_reduce_x_ports(pre_mapped: MappedCell) -> Cell {
	let mut new_connections = hash_map();
	let mut new_directions = hash_map();
	assert!(pre_mapped.connections.contains_key("A"));
	assert!(pre_mapped.connections.contains_key("Y"));
	assert!(pre_mapped.port_directions.contains_key("A"));
	assert!(pre_mapped.port_directions.contains_key("Y"));
	assert_eq!(pre_mapped.port_directions["A"], Direction::Input);
	assert_eq!(pre_mapped.port_directions["Y"], Direction::Output);
	for (idx, bit) in pre_mapped.connections["A"].iter().enumerate() {
		new_connections.insert(format!("A{}", idx), vec![*bit]);
		new_directions.insert(format!("A{}", idx), pre_mapped.port_directions["A"]);
	}
	new_connections.insert("Y".to_owned(), pre_mapped.connections["Y"].clone());
	new_directions.insert("Y".to_owned(), Direction::Output);
	let cell_type = match pre_mapped.cell_type.as_str() {
		"v2f_reduce_or" => ImplementableOp::ReduceOr,
		"v2f_reduce_and" => ImplementableOp::ReduceAnd,
		_ => unreachable!(),
	};
	Cell {
		hide_name: pre_mapped.hide_name,
		cell_type,
		model: pre_mapped.model,
		parameters: pre_mapped.parameters,
		attributes: pre_mapped.attributes,
		port_directions: new_directions,
		connections: new_connections,
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
		let test1 = mapped_design.modules.get("top");
		assert!(test1.is_some());
		let test1 = test1.unwrap();
		println!("{:?}", test1);
		assert!(test1.ports.contains_key("signal_0"));
		assert!(test1.ports.contains_key("signal_1"));
		assert!(test1.ports.contains_key("signal_2"));
		assert_eq!(test1.attributes["top"].from_bin_str(), Some(1));
		for (_cell_name, cell) in test1.cells.iter() {
			assert_eq!(cell.cell_type, ImplementableOp::Mul);
			assert_eq!(cell.parameters["A_SIGNED"].from_bin_str(), Some(1));
			assert_eq!(cell.parameters["B_SIGNED"].from_bin_str(), Some(1));
		}
	}

	#[test]
	fn multiport() {
		let file = File::open("./test_designs/output/test10.json").unwrap();
		let reader = BufReader::new(file);
		let mapped_design: MappedDesign = serde_json::from_reader(reader).unwrap();
		let top = mapped_design.modules.get("top");
		assert!(top.is_some());
		let top = top.unwrap();
		assert!(top.ports.contains_key("signal_1_1"));
		assert!(top.ports.contains_key("signal_1_2"));
		assert!(top.ports.contains_key("signal_2_1"));
		assert!(top.ports.contains_key("signal_2_2"));
		assert_eq!(top.attributes["top"].from_bin_str(), Some(1));
		assert!(top.cells["memory"].connections.contains_key("RD_DATA_0"));
		assert!(top.cells["memory"].connections.contains_key("RD_DATA_1"));
		assert!(top.cells["memory"].connections.contains_key("RD_ADDR_0"));
		assert!(top.cells["memory"].connections.contains_key("RD_ADDR_1"));
		assert_eq!(top.cells["memory"].connections["RD_DATA_0"].len(), 32);
		assert_eq!(top.cells["memory"].connections["RD_DATA_1"].len(), 32);
		assert_eq!(top.cells["memory"].connections["RD_ADDR_0"].len(), 7);
		assert_eq!(top.cells["memory"].connections["RD_ADDR_1"].len(), 7);
		assert_eq!(
			top.cells["memory"].port_directions["RD_DATA_0"],
			Direction::Output
		);
		assert_eq!(
			top.cells["memory"].port_directions["RD_DATA_1"],
			Direction::Output
		);
		assert_eq!(
			top.cells["memory"].port_directions["RD_ADDR_0"],
			Direction::Input
		);
		assert_eq!(
			top.cells["memory"].port_directions["RD_ADDR_1"],
			Direction::Input
		);
	}

	#[test]
	fn multiport_ram() {
		let file = File::open("./test_designs/output/test11.json").unwrap();
		let reader = BufReader::new(file);
		let mapped_design: MappedDesign = serde_json::from_reader(reader).unwrap();
		let top = mapped_design.modules.get("top");
		assert!(top.is_some());
		let top = top.unwrap();
		assert!(top.ports.contains_key("signal_1_1"));
		assert!(top.ports.contains_key("signal_1_2"));
		assert!(top.ports.contains_key("signal_2_1"));
		assert!(top.ports.contains_key("signal_2_2"));
		assert!(top.ports.contains_key("signal_A"));
		assert!(top.ports.contains_key("signal_C"));
		assert!(top.ports.contains_key("signal_D"));
		assert!(top.ports.contains_key("signal_E"));
		assert_eq!(top.attributes["top"].from_bin_str(), Some(1));
		assert!(top.cells["memory"].connections.contains_key("RD_DATA_0"));
		assert!(top.cells["memory"].connections.contains_key("RD_DATA_1"));
		assert!(top.cells["memory"].connections.contains_key("RD_ADDR_0"));
		assert!(top.cells["memory"].connections.contains_key("RD_ADDR_1"));
		assert_eq!(top.cells["memory"].connections["RD_DATA_0"].len(), 32);
		assert_eq!(top.cells["memory"].connections["RD_DATA_1"].len(), 32);
		assert_eq!(top.cells["memory"].connections["RD_ADDR_0"].len(), 8);
		assert_eq!(top.cells["memory"].connections["RD_ADDR_1"].len(), 8);
		assert_eq!(
			top.cells["memory"].port_directions["RD_DATA_0"],
			Direction::Output
		);
		assert_eq!(
			top.cells["memory"].port_directions["RD_DATA_1"],
			Direction::Output
		);
		assert_eq!(
			top.cells["memory"].port_directions["RD_ADDR_0"],
			Direction::Input
		);
		assert_eq!(
			top.cells["memory"].port_directions["RD_ADDR_1"],
			Direction::Input
		);
		assert_eq!(
			top.cells["memory"].port_directions["WR_ADDR_0"],
			Direction::Input
		);
		assert_eq!(top.cells["memory"].connections["WR_ADDR_0"].len(), 8);
		assert_eq!(top.cells["memory"].connections["WR_EN_0"].len(), 1);
		assert_eq!(
			top.cells["memory"].connections["WR_EN_0"],
			top.ports["signal_E"].bits
		);
		assert_eq!(
			top.cells["memory"].connections["WR_ADDR_0"],
			top.ports["signal_A"].bits[0..8]
		);
		assert_eq!(
			top.cells["memory"].connections["WR_DATA_0"],
			top.ports["signal_D"].bits
		);
		assert_eq!(
			top.cells["memory"].connections["WR_CLK_0"],
			top.ports["signal_C"].bits
		);
	}
}

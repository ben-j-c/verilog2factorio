use std::collections::HashMap;
use std::fmt::Display;
use std::vec;

use itertools::Itertools;

use crate::mapped_design::{Bit, Direction, FromBinStr, Integer};
use crate::mapped_design::{BitSliceOps, MappedDesign};

type NodeId = usize;

#[derive(Debug)]
pub struct ConnectedDesign {
	/// Node details
	pub node_info: Vec<NodeIo>,
	/// Which NodeId are connected to this Node
	pub fanout: Vec<Vec<NodeId>>,
	/// Which NodeId connect to this Node
	pub fanin: Vec<Vec<NodeId>>,
	/// For this terminal, the input expressions
	pub expr: Vec<Vec<CoarseExpr>>,
	/// (mapped_id, terminal_number) -> NodeId. An index for io
	pub mapped_id_to_terminal: HashMap<(String, usize), NodeId>,
	/// terminal_name -> (terminal_number, terminal_direction)
	pub terminal_name2typeid: HashMap<String, (usize, Direction)>,
	pub terminal_typeid2direction: Vec<Direction>,

	pub promote_all_nets_to_ports: bool,
}

#[derive(Debug)]
pub enum NodeIo {
	Port {
		direction: Direction,
		mapped_id: String,
		terminal_number: usize,
		bits: Vec<Bit>,
	},
	Cell {
		mapped_id: String,
		terminal_number: usize,
		terminal_name: String,
		bits: Vec<Bit>,
	},
}

impl NodeIo {
	fn bit(&self, bit_number: usize) -> Bit {
		match self {
			NodeIo::Port { bits, .. } => bits[bit_number],
			NodeIo::Cell { bits, .. } => bits[bit_number],
		}
	}

	pub fn n_bits(&self) -> usize {
		match self {
			NodeIo::Port { bits, .. } => bits.len(),
			NodeIo::Cell { bits, .. } => bits.len(),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CoarseExpr {
	DriverChunk {
		driver_ioid: NodeId,
		shift: i32,
		bit_start: usize,
		bit_end: usize,
	},
	ConstantChunk {
		shift: i32,
		value: Vec<bool>,
	},
}

impl CoarseExpr {
	pub fn unwrap_mask_shift(&self) -> (i32, i32) {
		match self {
			CoarseExpr::DriverChunk {
				shift,
				bit_start,
				bit_end,
				..
			} => {
				let upper = (1_i64 << bit_end) - 1;
				let lower = (1_i64 << bit_start) - 1;
				((upper ^ lower) as i32, *shift)
			},
			_ => panic!("Unwrapped a constant as a driver."),
		}
	}

	pub fn is_driver(&self) -> bool {
		matches!(self, CoarseExpr::DriverChunk { .. })
	}

	pub fn is_constant(&self) -> bool {
		matches!(self, CoarseExpr::ConstantChunk { .. })
	}

	pub fn unwrap_constant_value(&self) -> &Vec<bool> {
		match self {
			CoarseExpr::DriverChunk { .. } => panic!("Unwrapped driver."),
			CoarseExpr::ConstantChunk { value, .. } => value,
		}
	}

	pub fn n_bits(&self) -> usize {
		match self {
			CoarseExpr::DriverChunk {
				bit_start, bit_end, ..
			} => bit_end - bit_start,
			CoarseExpr::ConstantChunk { value, .. } => value.len(),
		}
	}
}

impl ConnectedDesign {
	pub fn new() -> Self {
		ConnectedDesign {
			node_info: vec![],
			fanout: vec![],
			fanin: vec![],
			expr: vec![],
			mapped_id_to_terminal: HashMap::new(),
			terminal_name2typeid: HashMap::new(),
			terminal_typeid2direction: vec![],
			promote_all_nets_to_ports: false,
		}
	}

	pub fn max_id(&self) -> NodeId {
		self.node_info.len() - 1
	}

	pub fn build_from(&mut self, mapped_design: &MappedDesign) {
		let max_bit = mapped_design.max_bit();
		let mut max_terminal_number = 2;
		{
			let mut terminal_map = HashMap::<String, (usize, Direction)>::new();
			let mut terminal_map_id = vec![Direction::Input, Direction::Output];
			terminal_map.insert("A".to_owned(), (0, Direction::Input));
			terminal_map.insert("Y".to_owned(), (1, Direction::Output));
			mapped_design.for_all_cells(|_, _mapped_id, cell| {
				for terminal in cell.connections.keys() {
					if let std::collections::hash_map::Entry::Vacant(v) =
						terminal_map.entry(terminal.clone())
					{
						v.insert((max_terminal_number, cell.port_directions[terminal]));
						terminal_map_id.push(cell.port_directions[terminal]);
						max_terminal_number += 1;
					}
				}
			});
			self.terminal_name2typeid = terminal_map;
			self.terminal_typeid2direction = terminal_map_id;
		}
		let mut bit_map: Vec<Vec<Vec<NodeId>>> =
			vec![vec![vec![]; max_terminal_number]; max_bit as usize + 1];

		let mut io_map = vec![];
		mapped_design.for_all_cells(|_, mapped_id, cell| {
			for terminal_name in cell.get_terminal_names() {
				let (terminal_number, _) = self.terminal_name2typeid[&terminal_name];
				let ioid: NodeId = io_map.len();
				io_map.push(NodeIo::Cell {
					mapped_id: mapped_id.to_owned(),
					terminal_number,
					terminal_name: terminal_name.clone(),
					bits: cell.connections[&terminal_name].clone(),
				});
				for bit in cell.connections[&terminal_name].iter() {
					if let Bit::Id(bitid) = bit {
						bit_map[bitid.0 as usize][terminal_number].push(ioid);
					}
				}
			}
		});
		mapped_design.for_all_top_level_io(|_, mapped_id, port| {
			if port.direction == Direction::Input || port.direction == Direction::Inout {
				let ioid = io_map.len();
				io_map.push(NodeIo::Port {
					direction: port.direction,
					mapped_id: mapped_id.to_owned(),
					terminal_number: 1,
					bits: port.bits.clone(),
				});
				port.bits.iter().for_each(|b| {
					if let Bit::Id(bitid) = b {
						bit_map[bitid.0 as usize][1].push(ioid);
					}
				});
			}
			if port.direction == Direction::Output || port.direction == Direction::Inout {
				let ioid = io_map.len();
				io_map.push(NodeIo::Port {
					direction: port.direction,
					mapped_id: mapped_id.to_owned(),
					terminal_number: 0,
					bits: port.bits.clone(),
				});
				port.bits.iter().for_each(|b| {
					if let Bit::Id(bitid) = b {
						bit_map[bitid.0 as usize][0].push(ioid);
					}
				});
			}
		});
		for (netname, net) in mapped_design.iter_netnames() {
			if !self.promote_all_nets_to_ports {
				if let Some(keep_str) = net.attributes.get("keep") {
					if keep_str.from_bin_str() != Some(1) {
						continue;
					}
				} else {
					continue;
				}
			}
			if mapped_design.is_port(netname) {
				continue;
			}
			let ioid = io_map.len();
			io_map.push(NodeIo::Port {
				direction: Direction::Output,
				mapped_id: netname.to_owned(),
				terminal_number: 0,
				bits: net.bits.clone(),
			});
			net.bits.iter().for_each(|b| {
				if let Bit::Id(bitid) = b {
					bit_map[bitid.0 as usize][0].push(ioid);
				}
			});
		}
		let mut fanout_vec = vec![vec![]; io_map.len()];
		let mut fanin_vec = vec![vec![]; io_map.len()];
		let mut expr_vec = vec![vec![]; io_map.len()];
		for (ioid, io) in io_map.iter().enumerate() {
			let (bits, is_driver, is_sink, port_number) = match io {
				NodeIo::Port {
					direction,
					terminal_number: port_number,
					bits,
					..
				} => (
					bits,
					*direction != Direction::Output,
					*direction != Direction::Input,
					port_number,
				),
				NodeIo::Cell {
					mapped_id,
					terminal_number,
					terminal_name,
					..
				} => {
					let cell = mapped_design.get_cell(mapped_id);
					(
						&cell.connections[terminal_name],
						cell.port_directions[terminal_name] != Direction::Input,
						cell.port_directions[terminal_name] != Direction::Output,
						terminal_number,
					)
				},
			};
			if is_sink {
				let mut required_shift = vec![0; bits.len()];
				let mut required_driver_bit_number = vec![None; bits.len()];
				let mut required_constant = vec![None; bits.len()];
				let mut target_ioid = vec![None; bits.len()];
				for (bit_number, bit) in bits.iter().enumerate() {
					let driver_opt = self.get_driver(&bit_map, &io_map, ioid, bit_number);
					if bit.is_connection() && driver_opt.is_some() {
						let (ioid_driver, bit_number_driver) = driver_opt.unwrap();
						required_shift[bit_number] =
							(bit_number as i32) - (bit_number_driver as i32);
						required_driver_bit_number[bit_number] = Some(bit_number_driver);
						target_ioid[bit_number] = Some(ioid_driver);
					} else {
						if bit.is_constant() {
							required_constant[bit_number] = Some(bit.bool_unwrap());
						} else {
							required_constant[bit_number] = Some(false);
						}
						required_shift[bit_number] = bit_number as i32;
					}
				}
				expr_vec[ioid] = coarsen_bit_ranges(
					&required_shift,
					&required_driver_bit_number,
					&required_constant,
					&target_ioid,
				);
				let mut fanin = vec![];
				for fragment in expr_vec[ioid].iter() {
					if let CoarseExpr::DriverChunk { driver_ioid, .. } = fragment {
						fanin.push(*driver_ioid)
					}
				}
				fanin_vec[ioid] = fanin;
			} else {
				assert!(is_driver);
				for (bit_number, bit) in bits.iter().enumerate() {
					assert_eq!(
						self.get_driver_unwrap(&bit_map, &io_map, ioid, bit_number),
						(ioid, bit_number)
					);
					let attached_ports = &bit_map[bit.id_unwrap().0 as usize];
					for (port_number_attached, attached_ioids) in attached_ports.iter().enumerate()
					{
						if port_number_attached == *port_number {
							continue;
						}
						for attached_ioid in attached_ioids.iter() {
							if fanout_vec[ioid].contains(attached_ioid) {
								continue;
							}
							fanout_vec[ioid].push(*attached_ioid);
						}
					}
				}
			}
		}
		let mut nodeio_mapping = HashMap::<(String, usize), NodeId>::new();
		for (nodeid, node) in io_map.iter().enumerate() {
			let (mapped_id, terminal_number) = match &node {
				NodeIo::Port {
					mapped_id,
					terminal_number,
					..
				} => (mapped_id, terminal_number),
				NodeIo::Cell {
					mapped_id,
					terminal_number,
					..
				} => (mapped_id, terminal_number),
			};
			match nodeio_mapping.entry((mapped_id.clone(), *terminal_number)) {
				std::collections::hash_map::Entry::Vacant(e) => {
					e.insert(nodeid);
				},
				_ => assert!(false),
			}
		}
		self.expr = expr_vec;
		self.fanout = fanout_vec;
		self.fanin = fanin_vec;
		self.node_info = io_map;
		self.mapped_id_to_terminal = nodeio_mapping;
	}

	pub fn get_node_id(&self, mapped_id: &String, terminal_name: &String) -> NodeId {
		self.mapped_id_to_terminal[&(mapped_id.clone(), self.terminal_number(terminal_name))]
	}

	pub fn terminal_direction(&self, terminal_name: &str) -> Direction {
		self.terminal_name2typeid[terminal_name].1
	}

	pub fn terminal_direction_by_id(&self, terminal_number: usize) -> Direction {
		self.terminal_typeid2direction[terminal_number]
	}

	pub fn terminal_number(&self, terminal_name: &str) -> usize {
		self.terminal_name2typeid[terminal_name].0
	}

	fn get_driver_unwrap(
		&self,
		bit_map: &Vec<Vec<Vec<NodeId>>>,
		io_map: &Vec<NodeIo>,
		ioid: NodeId,
		bit_number: usize,
	) -> (usize, usize) {
		let io_entry = &io_map[ioid];
		let bit_id = io_entry.bit(bit_number).id_unwrap().0;
		match self.get_driver(bit_map, io_map, ioid, bit_number) {
			Some(v) => v,
			None => panic!(
				"Can't find driver for bit {bit_id} in the design. I.e., I think its floating."
			),
		}
	}

	fn get_driver(
		&self,
		bit_map: &Vec<Vec<Vec<NodeId>>>,
		io_map: &Vec<NodeIo>,
		ioid: NodeId,
		bit_number: usize,
	) -> Option<(usize, usize)> {
		let io_entry = &io_map[ioid];
		let bit_id = io_entry.bit(bit_number).id()?.0;
		let attached = &bit_map[bit_id as usize];
		let ioid_driver = match attached
			.iter()
			.enumerate()
			.find(|(terminal_number, terminal_ioids)| {
				self.terminal_direction_by_id(*terminal_number) == Direction::Output
					&& !terminal_ioids.is_empty()
			})
			.map(|(_, ioid_driver)| *ioid_driver.first().unwrap())
		{
			Some(v) => v,
			None => return None,
		};
		let bits_driver = match &io_map[ioid_driver] {
			NodeIo::Port { bits, .. } => bits,
			NodeIo::Cell { bits, .. } => bits,
		};
		for (bit_number_driver, bit_driver) in bits_driver.iter().enumerate() {
			if *bit_driver == io_entry.bit(bit_number) {
				return Some((ioid_driver, bit_number_driver));
			}
		}
		None
	}
}

/// Actually coarsen contiguous runs of bits that share the same driver I/O, same shift,
/// or are constant, etc.
fn coarsen_bit_ranges(
	required_shift: &[i32],
	required_driver_bit_number: &[Option<usize>],
	required_constant: &[Option<bool>],
	target_ioid: &[Option<usize>],
) -> Vec<CoarseExpr> {
	let mut exprs = Vec::new();
	let mut start = 0;
	let n = required_shift.len();

	if n == 0 {
		return exprs;
	}

	assert_eq!(n, required_shift.len());
	assert_eq!(n, required_driver_bit_number.len());
	assert_eq!(n, required_constant.len());
	assert_eq!(n, target_ioid.len());

	for i in 1..n + 1 {
		if i == n
			|| !(required_constant[i].is_some() == required_constant[i - 1].is_some()
				&& target_ioid[i] == target_ioid[i - 1]
				&& (required_shift[i] == required_shift[i - 1]
					|| required_constant[i].is_some()
						&& required_shift[i] == i as i32
						&& required_shift[i - 1] == (i as i32 - 1)))
		{
			let const_val = required_constant[start];
			let driver_id = target_ioid[start];
			if const_val.is_some() {
				exprs.push(CoarseExpr::ConstantChunk {
					shift: required_shift[start],
					value: required_constant[start..i]
						.iter()
						.map(|x| x.unwrap())
						.collect_vec(),
				});
			} else if let Some(did) = driver_id {
				exprs.push(CoarseExpr::DriverChunk {
					driver_ioid: did,
					shift: required_shift[start],
					bit_start: start,
					bit_end: i,
				});
			} else {
				unreachable!()
			}
			start = i;
		}
	}

	exprs
}

impl BitSliceOps for Vec<bool> {
	fn is_all_constants(&self) -> bool {
		true
	}

	fn get_constant<T>(&self) -> T
	where
		T: Integer,
	{
		let mut retval = T::zero();
		for bit in self.iter().rev() {
			if *bit {
				retval = retval.sll().inc()
			} else {
				retval = retval.sll()
			}
		}
		retval
	}
}

impl Display for ConnectedDesign {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		writeln!(f, "Expr:")?;
		for (i, x) in self.expr.iter().enumerate() {
			writeln!(f, "{i} {:?}", x)?;
		}
		writeln!(f, "Fanin:")?;
		for (i, x) in self.fanin.iter().enumerate() {
			writeln!(f, "{i} {:?}", x)?;
		}
		writeln!(f, "Fanout:")?;
		for (i, x) in self.fanout.iter().enumerate() {
			writeln!(f, "{i} {:?}", x)?;
		}
		writeln!(f, "Mapped id to terminal:")?;
		for x in self.mapped_id_to_terminal.iter() {
			writeln!(f, "{:?}", x)?;
		}
		writeln!(f, "Node info:")?;
		for (i, x) in self.node_info.iter().enumerate() {
			writeln!(f, "{i} {:?}", x)?;
		}
		Ok(())
	}
}

#[cfg(test)]
mod tests {
	use std::{fs::File, io::BufReader};

	use super::*;

	#[test]
	fn coarsen_0() {
		let required_shift = vec![];
		let required_driver_bit_number = vec![];
		let required_constant = vec![];
		let target_ioid = vec![];
		let expr = coarsen_bit_ranges(
			&required_shift,
			&required_driver_bit_number,
			&required_constant,
			&target_ioid,
		);
		assert_eq!(expr.len(), 0);
	}

	#[test]
	fn coarsen_1_bit() {
		let required_shift = vec![5];
		let required_driver_bit_number = vec![Some(0)];
		let required_constant = vec![None];
		let target_ioid = vec![Some(10)];
		let expr = coarsen_bit_ranges(
			&required_shift,
			&required_driver_bit_number,
			&required_constant,
			&target_ioid,
		);
		assert_eq!(expr.len(), 1);
		match expr[0] {
			CoarseExpr::DriverChunk {
				driver_ioid,
				shift,
				bit_start,
				bit_end,
			} => {
				assert_eq!(driver_ioid, 10);
				assert_eq!(shift, 5);
				assert_eq!(bit_start, 0);
				assert_eq!(bit_end, 1);
			},
			CoarseExpr::ConstantChunk { .. } => assert!(false),
		}
	}

	#[test]
	fn coarsen_no_shift() {
		let required_shift = vec![0, 0, 0, 0, 0, 0, 0, 0];
		let required_driver_bit_number = vec![
			Some(0),
			Some(1),
			Some(2),
			Some(3),
			Some(4),
			Some(5),
			Some(6),
			Some(7),
		];
		let required_constant = vec![None, None, None, None, None, None, None, None];
		let target_ioid = vec![
			Some(10),
			Some(10),
			Some(10),
			Some(10),
			Some(10),
			Some(10),
			Some(10),
			Some(10),
		];
		let expr = coarsen_bit_ranges(
			&required_shift,
			&required_driver_bit_number,
			&required_constant,
			&target_ioid,
		);
		assert_eq!(expr.len(), 1);
		match expr[0] {
			CoarseExpr::DriverChunk {
				driver_ioid,
				shift,
				bit_start,
				bit_end,
			} => {
				assert_eq!(driver_ioid, 10);
				assert_eq!(shift, 0);
				assert_eq!(bit_start, 0);
				assert_eq!(bit_end, 8);
			},
			CoarseExpr::ConstantChunk { .. } => assert!(false),
		}
	}

	#[test]
	fn coarsen_complex() {
		let required_shift = vec![0, 0, 0, 3, 4, 1, 1, 2];
		let required_driver_bit_number = vec![
			Some(0),
			Some(1),
			Some(2),
			Some(3),
			Some(4),
			Some(5),
			Some(6),
			Some(7),
		];
		let required_constant = vec![None, None, None, Some(true), Some(false), None, None, None];
		let target_ioid = vec![
			Some(10),
			Some(10),
			Some(10),
			None,
			None,
			Some(11),
			Some(11),
			Some(11),
		];
		let expr = coarsen_bit_ranges(
			&required_shift,
			&required_driver_bit_number,
			&required_constant,
			&target_ioid,
		);
		assert_eq!(expr.len(), 4);
		match expr[0] {
			CoarseExpr::DriverChunk {
				driver_ioid,
				shift,
				bit_start,
				bit_end,
			} => {
				assert_eq!(driver_ioid, 10);
				assert_eq!(shift, 0);
				assert_eq!(bit_start, 0);
				assert_eq!(bit_end, 3);
			},
			CoarseExpr::ConstantChunk { .. } => assert!(false),
		}

		match &expr[1] {
			CoarseExpr::DriverChunk { .. } => assert!(false),
			CoarseExpr::ConstantChunk { shift, value } => {
				assert_eq!(*shift, 3);
				assert_eq!(*value, vec![true, false]);
			},
		}

		match expr[2] {
			CoarseExpr::DriverChunk {
				driver_ioid,
				shift,
				bit_start,
				bit_end,
			} => {
				assert_eq!(driver_ioid, 11);
				assert_eq!(shift, 1);
				assert_eq!(bit_start, 5);
				assert_eq!(bit_end, 7);
			},
			CoarseExpr::ConstantChunk { .. } => assert!(false),
		}

		match expr[3] {
			CoarseExpr::DriverChunk {
				driver_ioid,
				shift,
				bit_start,
				bit_end,
			} => {
				assert_eq!(driver_ioid, 11);
				assert_eq!(shift, 2);
				assert_eq!(bit_start, 7);
				assert_eq!(bit_end, 8);
			},
			CoarseExpr::ConstantChunk { .. } => assert!(false),
		}
	}

	#[test]
	fn swizzle() {
		let file = File::open("./test_designs/output/test6.json").unwrap();
		let reader = BufReader::new(file);
		let mapped_design: MappedDesign = serde_json::from_reader(reader).unwrap();
		let mut connected_design = ConnectedDesign::new();
		connected_design.build_from(&mapped_design);
		let expected = vec![
			CoarseExpr::DriverChunk {
				driver_ioid: 1,
				shift: -27,
				bit_start: 0,
				bit_end: 4,
			},
			CoarseExpr::ConstantChunk {
				shift: 4,
				value: vec![true, true, true, true],
			},
			CoarseExpr::DriverChunk {
				driver_ioid: 1,
				shift: 0,
				bit_start: 8,
				bit_end: 17,
			},
			CoarseExpr::ConstantChunk {
				shift: 17,
				value: vec![false, false, false, false, true, true, true, true],
			},
			CoarseExpr::DriverChunk {
				driver_ioid: 1,
				shift: 25,
				bit_start: 25,
				bit_end: 32,
			},
		];
		let expected2 = vec![
			CoarseExpr::DriverChunk {
				driver_ioid: 0,
				shift: -27,
				bit_start: 0,
				bit_end: 4,
			},
			CoarseExpr::ConstantChunk {
				shift: 4,
				value: vec![true, true, true, true],
			},
			CoarseExpr::DriverChunk {
				driver_ioid: 0,
				shift: 0,
				bit_start: 8,
				bit_end: 17,
			},
			CoarseExpr::ConstantChunk {
				shift: 17,
				value: vec![false, false, false, false, true, true, true, true],
			},
			CoarseExpr::DriverChunk {
				driver_ioid: 0,
				shift: 25,
				bit_start: 25,
				bit_end: 32,
			},
		];
		if connected_design.expr[0].is_empty() {
			assert_eq!(connected_design.expr[1], expected2);
			assert_eq!(connected_design.fanout[0], vec![1]);
		} else {
			assert_eq!(connected_design.expr[0], expected);
			assert_eq!(connected_design.fanout[1], vec![0]);
		}
	}

	#[test]
	fn design1() {
		let file = File::open("./test_designs/output/test1.json").unwrap();
		let reader = BufReader::new(file);
		let mapped_design: MappedDesign = serde_json::from_reader(reader).unwrap();
		let mut cd = ConnectedDesign::new();
		cd.build_from(&mapped_design);
		assert_eq!(cd.expr.len(), 6);
		let mut counter = 0;
		for (i, x) in cd.expr.iter().enumerate() {
			println!("{:?} {:?}", i, x);
			assert!(x.len() <= 1);
			match x.first() {
				Some(CoarseExpr::DriverChunk {
					driver_ioid,
					shift,
					bit_start,
					bit_end,
				}) => {
					counter += 1;
					assert_eq!(*shift, 0);
					assert_eq!(*bit_start, 0);
					assert_eq!(*bit_end, 32);
					assert!(*driver_ioid < 6);
				},
				None => {},
				_ => assert!(false),
			}
		}
		assert_eq!(counter, 3);
	}
}

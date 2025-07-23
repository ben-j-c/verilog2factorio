use std::{fs::File, io::BufReader};

use crate::{
	checked_design::CheckedDesign, logical_design::LogicalDesign, mapped_design::MappedDesign,
	phy::PhysicalDesign, serializable_design::SerializableDesign,
};

#[test]
fn design_test1() {
	let file = File::open("./test_designs/output/test1.json").unwrap();
	let reader = BufReader::new(file);
	let mapped_design: MappedDesign = serde_json::from_reader(reader).unwrap();
	let mut checked_design = CheckedDesign::new();
	let mut logical_design = LogicalDesign::new();
	let mut physical_design = PhysicalDesign::new();
	let mut serializable_design = SerializableDesign::new();
	checked_design.build_from(&mapped_design);
	logical_design.build_from(&checked_design, &mapped_design);
	logical_design.for_all(|_x, y| println!("{:?}", y));
	physical_design.build_from(&logical_design);
	serializable_design.build_from(&physical_design, &logical_design);
	let blueprint_json = serde_json::to_string(&serializable_design).unwrap();
	println!("{}", blueprint_json);
}

#[test]
fn design_test2() {
	let file = File::open("./test_designs/output/test2.json").unwrap();
	let reader = BufReader::new(file);
	let mapped_design: MappedDesign = serde_json::from_reader(reader).unwrap();
	let mut checked_design = CheckedDesign::new();
	let mut logical_design = LogicalDesign::new();
	let mut physical_design = PhysicalDesign::new();
	let mut serializable_design = SerializableDesign::new();
	checked_design.build_from(&mapped_design);
	logical_design.build_from(&checked_design, &mapped_design);
	logical_design.for_all(|_x, y| println!("{:?}", y));
	physical_design.build_from(&logical_design);
	serializable_design.build_from(&physical_design, &logical_design);
	let blueprint_json = serde_json::to_string(&serializable_design).unwrap();
	println!("{}", blueprint_json);
}

#[test]
fn design_test3() {
	let file = File::open("./test_designs/output/test3.json").unwrap();
	let reader = BufReader::new(file);
	let mapped_design: MappedDesign = serde_json::from_reader(reader).unwrap();
	let mut checked_design = CheckedDesign::new();
	let mut logical_design = LogicalDesign::new();
	let mut physical_design = PhysicalDesign::new();
	let mut serializable_design = SerializableDesign::new();
	checked_design.build_from(&mapped_design);
	logical_design.build_from(&checked_design, &mapped_design);
	logical_design.for_all(|_x, y| println!("{:?}", y));
	physical_design.build_from(&logical_design);
	serializable_design.build_from(&physical_design, &logical_design);
	let blueprint_json = serde_json::to_string(&serializable_design).unwrap();
	println!("{}", blueprint_json);
}

#[test]
fn design_complex_expr() {
	let file = File::open("./test_designs/output/complex_expr.json").unwrap();
	let reader = BufReader::new(file);
	let mapped_design: MappedDesign = serde_json::from_reader(reader).unwrap();
	let mut checked_design = CheckedDesign::new();
	let mut logical_design = LogicalDesign::new();
	let mut physical_design = PhysicalDesign::new();
	let mut serializable_design = SerializableDesign::new();
	checked_design.build_from(&mapped_design);
	logical_design.build_from(&checked_design, &mapped_design);
	logical_design.for_all(|_, y| println!("{:?}", y));
	physical_design.build_from(&logical_design);
	serializable_design.build_from(&physical_design, &logical_design);
	let blueprint_json = serde_json::to_string(&serializable_design).unwrap();
	println!("\n{}\n", blueprint_json);
}

#[test]
fn design_balancer() {
	let file = File::open("./test_designs/output/test4.json").unwrap();
	let reader = BufReader::new(file);
	let mapped_design: MappedDesign = serde_json::from_reader(reader).unwrap();
	let mut checked_design = CheckedDesign::new();
	let mut logical_design = LogicalDesign::new();
	let mut physical_design = PhysicalDesign::new();
	let mut serializable_design = SerializableDesign::new();
	checked_design.build_from(&mapped_design);
	logical_design.build_from(&checked_design, &mapped_design);
	logical_design.for_all(|_, node| println!("{:?}", node));
	physical_design.build_from(&logical_design);
	serializable_design.build_from(&physical_design, &logical_design);
	let blueprint_json = serde_json::to_string(&serializable_design).unwrap();
	println!("{}", blueprint_json);
}

#[test]
fn design_single_dff() {
	let file = File::open("./test_designs/output/test5.json").unwrap();
	let reader = BufReader::new(file);
	let mapped_design: MappedDesign = serde_json::from_reader(reader).unwrap();
	let mut checked_design = CheckedDesign::new();
	let mut logical_design = LogicalDesign::new();
	let mut physical_design = PhysicalDesign::new();
	let mut serializable_design = SerializableDesign::new();
	checked_design.build_from(&mapped_design);
	logical_design.build_from(&checked_design, &mapped_design);
	logical_design.for_all(|_, y| println!("{:?}", y));
	physical_design.build_from(&logical_design);
	serializable_design.build_from(&physical_design, &logical_design);
	let blueprint_json = serde_json::to_string(&serializable_design).unwrap();
	println!("{}", blueprint_json);
}

#[test]
fn design_swizzle_bits() {
	let file = File::open("./test_designs/output/test6.json").unwrap();
	let reader = BufReader::new(file);
	let mapped_design: MappedDesign = serde_json::from_reader(reader).unwrap();
	let mut checked_design = CheckedDesign::new();
	let mut logical_design = LogicalDesign::new();
	let mut physical_design = PhysicalDesign::new();
	let mut serializable_design = SerializableDesign::new();
	checked_design.build_from(&mapped_design);
	logical_design.build_from(&checked_design, &mapped_design);
	logical_design.for_all(|_, y| println!("{:?}", y));
	physical_design.build_from(&logical_design);
	serializable_design.build_from(&physical_design, &logical_design);
	let blueprint_json = serde_json::to_string(&serializable_design).unwrap();
	println!("\n{}", blueprint_json);
}

#[test]
fn design_simple_lut() {
	let file = File::open("./test_designs/output/test7.json").unwrap();
	let reader = BufReader::new(file);
	let mapped_design: MappedDesign = serde_json::from_reader(reader).unwrap();
	let mut checked_design = CheckedDesign::new();
	let mut logical_design = LogicalDesign::new();
	let mut physical_design = PhysicalDesign::new();
	let mut serializable_design = SerializableDesign::new();
	checked_design.build_from(&mapped_design);
	logical_design.build_from(&checked_design, &mapped_design);
	logical_design.for_all(|_, y| println!("{:?}", y));
	physical_design.build_from(&logical_design);
	serializable_design.build_from(&physical_design, &logical_design);
	let blueprint_json = serde_json::to_string(&serializable_design).unwrap();
	println!("\n{}", blueprint_json);
}

#[test]
fn design_simple_lut_alterante() {
	let file = File::open("./test_designs/output/test9.json").unwrap();
	let reader = BufReader::new(file);
	let mapped_design: MappedDesign = serde_json::from_reader(reader).unwrap();
	let mut checked_design = CheckedDesign::new();
	let mut logical_design = LogicalDesign::new();
	let mut physical_design = PhysicalDesign::new();
	let mut serializable_design = SerializableDesign::new();
	checked_design.build_from(&mapped_design);
	logical_design.build_from(&checked_design, &mapped_design);
	logical_design.for_all(|_, y| println!("{:?}", y));
	physical_design.build_from(&logical_design);
	serializable_design.build_from(&physical_design, &logical_design);
	let blueprint_json = serde_json::to_string(&serializable_design).unwrap();
	println!("\n{}", blueprint_json);
}

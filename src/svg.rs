use std::fs::File;
use std::io::{self, Write};

pub type ShapeId = usize;

pub struct SVG {
	shapes: Vec<Shape>,
}

#[derive(Debug, Clone)]
struct RectData {
	x: i32,
	y: i32,
	w: i32,
	h: i32,
	color: (u8, u8, u8),
	label: Option<String>,
	hover: Option<String>,
}

#[derive(Debug, Clone)]
struct LineData {
	shape_a: ShapeId,
	shape_b: ShapeId,
	attachment_a: u32,
	attachment_b: u32,
}

#[derive(Debug, Clone)]
enum Shape {
	Rect(RectData),
	Line(LineData),
}

impl SVG {
	pub fn new() -> Self {
		SVG { shapes: Vec::new() }
	}

	pub fn add_rect(
		&mut self,
		x: i32,
		y: i32,
		w: i32,
		h: i32,
		colour: (u8, u8, u8),
		label: Option<String>,
		hover: Option<String>,
	) -> ShapeId {
		let rect = RectData {
			x,
			y,
			w,
			h,
			color: colour,
			label: label.map(|s| s.to_string()),
			hover: hover.map(|s| s.to_string()),
		};
		self.shapes.push(Shape::Rect(rect));
		self.shapes.len() - 1
	}

	pub fn add_wire(
		&mut self,
		a: ShapeId,
		b: ShapeId,
		attachment_a: u32,
		attachment_b: u32,
	) -> ShapeId {
		assert!(attachment_a <= 3, "attachment_a must be 0..3");
		assert!(attachment_b <= 3, "attachment_b must be 0..3");
		assert!(a < self.shapes.len(), "Invalid shape index for 'a'");
		assert!(b < self.shapes.len(), "Invalid shape index for 'b'");

		let line = LineData {
			shape_a: a,
			shape_b: b,
			attachment_a,
			attachment_b,
		};
		self.shapes.push(Shape::Line(line));
		self.shapes.len() - 1
	}

	pub fn save(&self, filename: &str) -> io::Result<()> {
		let (mut max_w, mut max_h) = (0, 0);
		for shape in &self.shapes {
			if let Shape::Rect(r) = shape {
				let right = r.x + r.w;
				let bottom = r.y + r.h;
				if right > max_w {
					max_w = right;
				}
				if bottom > max_h {
					max_h = bottom;
				}
			}
		}
		let (svg_w, svg_h) = (max_w + 20, max_h + 20);

		let mut svg_data = format!(
			r#"<svg xmlns="http://www.w3.org/2000/svg" width="{w}" height="{h}">"#,
			w = svg_w,
			h = svg_h
		);

		for shape in &self.shapes {
			match shape {
				Shape::Rect(r) => {
					let color_str = color_to_string(r.color);
					svg_data.push_str(&format!(
						r#"<rect x="{x}" y="{y}" width="{w}" height="{h}" fill="{fill}">"#,
						x = r.x,
						y = r.y,
						w = r.w,
						h = r.h,
						fill = color_str
					));
					if let Some(txt) = &r.hover {
						svg_data.push_str(&format!(r#"<title>{}</title>"#, txt));
					}
					svg_data.push_str("</rect>");

					if let Some(txt) = &r.label {
						let cx = r.x + r.w / 2;
						let cy = r.y + r.h / 2;
						svg_data.push_str(&format!(
                            r#"<text x="{cx}" y="{cy}" text-anchor="middle" alignment-baseline="middle" fill="black">{label}</text>"#,
                            cx = cx,
                            cy = cy,
                            label = txt
                        ));
					}
				}
				Shape::Line(l) => {
					let wire_color = match l.attachment_a {
						0 | 2 => "red",
						1 | 3 => "green",
						_ => unreachable!(),
					};
					let (rect_a, rect_b) =
						match (self.shapes.get(l.shape_a), self.shapes.get(l.shape_b)) {
							(Some(Shape::Rect(ra)), Some(Shape::Rect(rb))) => (ra, rb),
							_ => panic!("Wire references non-rectangle shape!"),
						};
					let (x1, y1) = rect_attachment(rect_a, l.attachment_a);
					let (x2, y2) = rect_attachment(rect_b, l.attachment_b);

					svg_data.push_str(&format!(
						r#"<line x1="{x1}" y1="{y1}" x2="{x2}" y2="{y2}" stroke="{col}" stroke-width="1" />"#,
						x1 = x1,
						y1 = y1,
						x2 = x2,
						y2 = y2,
						col = wire_color
					));
				}
			}
		}

		svg_data.push_str("</svg>");

		let mut file = File::create(filename)?;
		file.write_all(svg_data.as_bytes())?;
		Ok(())
	}
}

fn rect_attachment(r: &RectData, attachment_index: u32) -> (i32, i32) {
	let wx1 = r.x + (r.w as f32 * 0.25) as i32;
	let wx2 = r.x + (r.w as f32 * 0.75) as i32;
	let wy1 = r.y + (r.h as f32 * 0.25) as i32;
	let wy2 = r.y + (r.h as f32 * 0.75) as i32;

	match attachment_index {
		3 => (wx1, wy1),
		2 => (wx1, wy2),
		1 => (wx2, wy1),
		0 => (wx2, wy2),
		_ => unreachable!(),
	}
}

fn color_to_string(c: (u8, u8, u8)) -> String {
	format!("rgb({},{},{})", c.0, c.1, c.2)
}

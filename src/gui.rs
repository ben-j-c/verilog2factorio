use chumsky::Parser;
use egui::{Color32, Id};
use egui_snarl::ui::{PinInfo, SnarlStyle, WireLayer};

use crate::logical_design::{
	self, arithmetic_parser, ArithmeticOperator, DeciderOperator, Signal, WireColour,
};
use crate::{logical_design::NodeId, LogDRef};

#[derive(serde::Deserialize, serde::Serialize)]
#[serde(default)]
pub struct V2FApp {
	snarl: egui_snarl::Snarl<Node>,
	viewer: V2FViewer,
}

impl Default for V2FApp {
	fn default() -> Self {
		let snarl = egui_snarl::Snarl::new();
		Self {
			snarl,
			viewer: V2FViewer::default(),
		}
	}
}

impl V2FApp {
	pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
		if let Some(storage) = cc.storage {
			eframe::get_value(storage, eframe::APP_KEY).unwrap_or_default()
		} else {
			Default::default()
		}
	}
}

impl eframe::App for V2FApp {
	/// Called by the framework to save state before shutdown.
	fn save(&mut self, storage: &mut dyn eframe::Storage) {
		eframe::set_value(storage, eframe::APP_KEY, self);
	}

	/// Called each time the UI needs repainting, which may be many times per second.
	fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
		// Put your widgets into a `SidePanel`, `TopBottomPanel`, `CentralPanel`, `Window` or `Area`.
		// For inspiration and more examples, go to https://emilk.github.io/egui

		egui::TopBottomPanel::top("top_panel").show(ctx, |ui| {
			// The top panel is often a good place for a menu bar:

			egui::MenuBar::new().ui(ui, |ui| {
				// NOTE: no File->Quit on web pages!
				let is_web = cfg!(target_arch = "wasm32");
				if !is_web {
					ui.menu_button("File", |ui| {
						if ui.button("Quit").clicked() {
							ctx.send_viewport_cmd(egui::ViewportCommand::Close);
						}
					});
					ui.add_space(16.0);
				}

				egui::widgets::global_theme_preference_buttons(ui);
			});
		});

		egui::SidePanel::left("side_panel").show(ctx, |ui| {
			egui::ScrollArea::vertical().show(ui, |ui| {});
		});

		let mut style = SnarlStyle::new();
		style.wire_layer = Some(WireLayer::AboveNodes);
		style.wire_width = Some(3.0);

		egui::CentralPanel::default().show(ctx, |ui| {
			egui_snarl::ui::SnarlWidget::new()
				.id(Id::new("node-canvas"))
				.style(style)
				.show(&mut self.snarl, &mut self.viewer, ui)
		});

		egui::TopBottomPanel::bottom("bottom_panel").show(ctx, |ui| {
			ui.with_layout(egui::Layout::bottom_up(egui::Align::LEFT), |ui| {
				source_link(ui);
				egui::warn_if_debug_build(ui);
			});
		});
	}
}

fn source_link(ui: &mut egui::Ui) {
	ui.horizontal(|ui| {
		ui.spacing_mut().item_spacing.x = 0.0;
		ui.label("Licensed exclusively under AGLPv3. Source available at ");
		ui.hyperlink_to("v2f", "https://github.com/ben-j-c/verilog2factorio");
		ui.label(".");
	});
}

#[derive(Default, serde::Deserialize, serde::Serialize)]
struct Node {
	lid: NodeId,
	logd: LogDRef,
	left_input_text: Vec<String>,
	right_input_text: Vec<String>,
	output_input_text: Vec<String>,
}

#[derive(Debug, Default, serde::Deserialize, serde::Serialize)]
struct V2FViewer {
	logd: LogDRef,
}

impl V2FViewer {
	fn add_decider(&mut self) -> Node {
		let mut logd = self.logd.write().unwrap();
		Node {
			lid: logd.add_decider(),
			logd: self.logd.clone(),
			left_input_text: Vec::default(),
			right_input_text: Vec::default(),
			output_input_text: Vec::default(),
		}
	}

	fn add_arithmetic(&mut self) -> Node {
		let mut logd = self.logd.write().unwrap();
		Node {
			lid: logd.add_arithmetic(
				(Signal::None, ArithmeticOperator::Add, Signal::None),
				Signal::None,
			),
			logd: self.logd.clone(),
			left_input_text: vec!["none".to_owned()],
			right_input_text: vec!["none".to_owned()],
			output_input_text: vec!["none".to_owned()],
		}
	}

	fn add_constant(&mut self) -> Node {
		let mut logd = self.logd.write().unwrap();
		Node {
			lid: logd.add_constant(vec![], vec![]),
			logd: self.logd.clone(),
			left_input_text: Vec::default(),
			right_input_text: Vec::default(),
			output_input_text: Vec::default(),
		}
	}

	fn add_lamp(&mut self) -> Node {
		let mut logd = self.logd.write().unwrap();
		Node {
			lid: logd.add_lamp((Signal::None, DeciderOperator::Equal, Signal::None)),
			logd: self.logd.clone(),
			left_input_text: vec!["none".to_owned()],
			right_input_text: vec!["none".to_owned()],
			output_input_text: Vec::default(),
		}
	}

	fn add_display_panel(&mut self) -> Node {
		let mut logd = self.logd.write().unwrap();
		Node {
			lid: logd.add_display_panel(),
			logd: self.logd.clone(),
			left_input_text: Vec::default(),
			right_input_text: Vec::default(),
			output_input_text: Vec::default(),
		}
	}
}

impl egui_snarl::ui::SnarlViewer<Node> for V2FViewer {
	fn title(&mut self, node: &Node) -> String {
		let logd = node.logd.read().unwrap();
		let node = logd.get_node(node.lid);
		let pfx = match &node.function {
			logical_design::NodeFunction::Arithmetic { .. } => "Arith.",
			logical_design::NodeFunction::Decider { .. } => "Deci. ",
			logical_design::NodeFunction::Constant { .. } => "Const.",
			logical_design::NodeFunction::Lamp { .. } => "Lamp",
			logical_design::NodeFunction::DisplayPanel { .. } => "Disp.",
			logical_design::NodeFunction::WireSum(_) => unreachable!(),
		};
		match &node.description {
			Some(d) => format!("{} {}", pfx, d),
			None => format!("{}", pfx),
		}
	}

	fn inputs(&mut self, node: &Node) -> usize {
		let logd = node.logd.read().unwrap();
		let node = logd.get_node(node.lid);
		match node.function {
			logical_design::NodeFunction::Arithmetic { .. } => 2,
			logical_design::NodeFunction::Decider { .. } => 2,
			logical_design::NodeFunction::Constant { .. } => 0,
			logical_design::NodeFunction::Lamp { .. } => 2,
			logical_design::NodeFunction::DisplayPanel { .. } => 2,
			logical_design::NodeFunction::WireSum(_) => unreachable!(),
		}
	}

	fn show_input(
		&mut self,
		pin: &egui_snarl::InPin,
		_ui: &mut egui::Ui,
		_snarl: &mut egui_snarl::Snarl<Node>,
	) -> impl egui_snarl::ui::SnarlPin + 'static {
		if pin.id.input == 0 {
			PinInfo::circle().with_fill(Color32::from_rgb(0xb0, 0x00, 0x00))
		} else if pin.id.input == 1 {
			PinInfo::circle().with_fill(Color32::from_rgb(0x00, 0xb0, 0x00))
		} else {
			unreachable!()
		}
	}

	fn outputs(&mut self, node: &Node) -> usize {
		let logd = node.logd.read().unwrap();
		let node = logd.get_node(node.lid);
		match node.function {
			logical_design::NodeFunction::Arithmetic { .. } => 2,
			logical_design::NodeFunction::Decider { .. } => 2,
			logical_design::NodeFunction::Constant { .. } => 2,
			logical_design::NodeFunction::Lamp { .. } => 0,
			logical_design::NodeFunction::DisplayPanel { .. } => 0,
			logical_design::NodeFunction::WireSum(_) => unreachable!(),
		}
	}

	fn show_output(
		&mut self,
		pin: &egui_snarl::OutPin,
		_ui: &mut egui::Ui,
		_snarl: &mut egui_snarl::Snarl<Node>,
	) -> impl egui_snarl::ui::SnarlPin + 'static {
		if pin.id.output == 0 {
			PinInfo::circle().with_fill(Color32::from_rgb(0xb0, 0x00, 0x00))
		} else if pin.id.output == 1 {
			PinInfo::circle().with_fill(Color32::from_rgb(0x00, 0xb0, 0x00))
		} else {
			unreachable!()
		}
	}

	fn connect(
		&mut self,
		from: &egui_snarl::OutPin,
		to: &egui_snarl::InPin,
		snarl: &mut egui_snarl::Snarl<Node>,
	) {
		let mut logd = self.logd.write().unwrap();
		let (lid_left, lid_right) = {
			let lhs = snarl.get_node(from.id.node).unwrap();
			let rhs = snarl.get_node(to.id.node).unwrap();
			if from.id.output != to.id.input {
				return;
			}
			(lhs.lid, rhs.lid)
		};
		if snarl.connect(from.id, to.id) {
			if from.id.output == 0 {
				logd.add_wire_red(vec![lid_left], vec![lid_right]);
			} else {
				logd.add_wire_green(vec![lid_left], vec![lid_right]);
			}
		}
	}

	fn disconnect(
		&mut self,
		from: &egui_snarl::OutPin,
		to: &egui_snarl::InPin,
		snarl: &mut egui_snarl::Snarl<Node>,
	) {
		let mut logd = self.logd.write().unwrap();
		let (lid_left, lid_right) = {
			let lhs = snarl.get_node(from.id.node).unwrap();
			let rhs = snarl.get_node(to.id.node).unwrap();
			if from.id.output != to.id.input {
				return;
			}
			(lhs.lid, rhs.lid)
		};
		if snarl.disconnect(from.id, to.id) {
			if from.id.output == 0 {
				logd.find_wire_between_disconnect(lid_left, lid_right, WireColour::Red);
			} else {
				logd.find_wire_between_disconnect(lid_left, lid_right, WireColour::Green);
			}
		}
	}

	fn has_graph_menu(&mut self, _pos: egui::Pos2, _snarl: &mut egui_snarl::Snarl<Node>) -> bool {
		true
	}

	fn show_graph_menu(
		&mut self,
		pos: egui::Pos2,
		ui: &mut egui::Ui,
		snarl: &mut egui_snarl::Snarl<Node>,
	) {
		ui.label("Add Node");
		if ui.button("Decider").clicked() {
			snarl.insert_node(pos, self.add_decider());
			ui.close();
		}
		if ui.button("Arithmetic").clicked() {
			snarl.insert_node(pos, self.add_arithmetic());
			ui.close();
		}
		if ui.button("Constant").clicked() {
			snarl.insert_node(pos, self.add_constant());
			ui.close();
		}
		if ui.button("Lamp").clicked() {
			snarl.insert_node(pos, self.add_lamp());
			ui.close();
		}
		if ui.button("Display Panel").clicked() {
			snarl.insert_node(pos, self.add_display_panel());
			ui.close();
		}
	}

	fn has_body(&mut self, node: &Node) -> bool {
		true
	}

	fn show_body(
		&mut self,
		node: egui_snarl::NodeId,
		inputs: &[egui_snarl::InPin],
		outputs: &[egui_snarl::OutPin],
		ui: &mut egui::Ui,
		snarl: &mut egui_snarl::Snarl<Node>,
	) {
		let mut logd = self.logd.write().unwrap();
		let node = if let Some(node) = snarl.get_node_mut(node) {
			node
		} else {
			return;
		};
		let lnode = logd.mut_node(node.lid);
		match &mut lnode.function {
			logical_design::NodeFunction::Arithmetic {
				op,
				input_1,
				input_2,
				input_left_network,
				input_right_network,
			} => {
				ui.vertical(|ui| {
					add_signal_input_box(
						ui,
						input_1,
						input_left_network,
						&mut node.left_input_text[0],
					);
					egui::ComboBox::from_label("")
						.width(ui.spacing().combo_width / 2.0)
						.selected_text(format!("{}", op.resolve_string()))
						.show_ui(ui, |ui| {
							use ArithmeticOperator::*;
							ui.selectable_value(op, Mult, Mult.resolve_string());
							ui.selectable_value(op, Div, Div.resolve_string());
							ui.selectable_value(op, Add, Add.resolve_string());
							ui.selectable_value(op, Sub, Sub.resolve_string());
							ui.selectable_value(op, Mod, Mod.resolve_string());
							ui.selectable_value(op, Exp, Exp.resolve_string());
							ui.selectable_value(op, Shl, Shl.resolve_string());
							ui.selectable_value(op, Sshr, Sshr.resolve_string());
							ui.selectable_value(op, And, And.resolve_string());
							ui.selectable_value(op, Or, Or.resolve_string());
							ui.selectable_value(op, Xor, Xor.resolve_string());
						});
					add_signal_input_box(
						ui,
						input_2,
						input_right_network,
						&mut node.right_input_text[0],
					);
					add_signal_input_box_name_only_label(
						ui,
						&mut lnode.output[0],
						&mut node.output_input_text[0],
						"Output",
					);
				});
			},
			logical_design::NodeFunction::Decider {
				expressions,
				expression_conj_disj,
				input_left_networks,
				input_right_networks,
				output_network,
				use_input_count,
				constants,
			} => {},
			logical_design::NodeFunction::Constant { enabled, constants } => {
				ui.vertical(|ui| {
					ui.checkbox(enabled, "Enabled");
					if ui.button("Add constant").clicked() {
						lnode.output.push(Signal::None);
						node.output_input_text.push("none".to_owned());
						constants.push(0);
					}
					for i in 0..constants.len() {
						ui.horizontal(|ui| {
							add_signal_input_box_name_only(
								ui,
								&mut lnode.output[i],
								&mut node.output_input_text[i],
							);
							let dv = egui::DragValue::new(&mut constants[i]).speed(1);
							ui.add(dv);
						});
					}
				});
			},
			logical_design::NodeFunction::Lamp { expression } => {
				//
				ui.horizontal(|ui| {
					add_signal_input_box_name_only(
						ui,
						&mut expression.0,
						&mut node.left_input_text[0],
					);

					let op = &mut expression.1;

					egui::ComboBox::from_label("")
						.width(ui.spacing().combo_width / 2.0)
						.selected_text(format!("{}", op.resolve_string()))
						.show_ui(ui, |ui| {
							use DeciderOperator::*;
							ui.selectable_value(op, LessThan, LessThan.resolve_string());
							ui.selectable_value(op, GreaterThan, GreaterThan.resolve_string());
							ui.selectable_value(op, Equal, Equal.resolve_string());
							ui.selectable_value(op, NotEqual, NotEqual.resolve_string());
							ui.selectable_value(
								op,
								GreaterThanEqual,
								GreaterThanEqual.resolve_string(),
							);
							ui.selectable_value(op, LessThanEqual, LessThanEqual.resolve_string());
						});

					add_signal_input_box_name_only(
						ui,
						&mut expression.2,
						&mut node.right_input_text[0],
					);
				});
			},
			logical_design::NodeFunction::DisplayPanel {
				input_1,
				input_2,
				op,
				text,
			} => {
				ui.vertical(|ui| {
					if ui.button("Add").clicked() {
						lnode.output.push(Signal::None);
						node.output_input_text.push("none".to_owned());
						input_1.push(Signal::None);
						node.left_input_text.push("none".to_owned());
						input_2.push(Signal::None);
						node.right_input_text.push("none".to_owned());
						text.push(None);
						op.push(DeciderOperator::LessThan);
					}
					for i in 0..input_1.len() {
						ui.horizontal(|ui| {
							add_signal_input_box_name_only(
								ui,
								&mut lnode.output[i],
								&mut node.output_input_text[i],
							);

							let mut tmp = false;
							if let Some(text) = &mut text[i] {
								let edit_left = egui::TextEdit::singleline(text)
									.desired_width(ui.spacing().text_edit_width / 2.0);
								ui.add(edit_left);
							} else if ui.checkbox(&mut tmp, "<no label>").clicked() {
								text[i] = Some(String::new());
							}

							add_signal_input_box_name_only(
								ui,
								&mut input_1[i],
								&mut node.left_input_text[i],
							);

							let op = &mut op[i];
							egui::ComboBox::new(egui::Id::new("op_combo").with(i), "")
								.width(ui.spacing().combo_width / 2.0)
								.selected_text(format!("{}", op.resolve_string()))
								.show_ui(ui, |ui| {
									use DeciderOperator::*;
									ui.selectable_value(op, LessThan, LessThan.resolve_string());
									ui.selectable_value(
										op,
										GreaterThan,
										GreaterThan.resolve_string(),
									);
									ui.selectable_value(op, Equal, Equal.resolve_string());
									ui.selectable_value(op, NotEqual, NotEqual.resolve_string());
									ui.selectable_value(
										op,
										GreaterThanEqual,
										GreaterThanEqual.resolve_string(),
									);
									ui.selectable_value(
										op,
										LessThanEqual,
										LessThanEqual.resolve_string(),
									);
								});

							add_signal_input_box_name_only(
								ui,
								&mut input_2[i],
								&mut node.right_input_text[i],
							);
						});
					}
				});
			},
			logical_design::NodeFunction::WireSum(_) => unreachable!(),
		};
	}

	fn has_node_menu(&mut self, _node: &Node) -> bool {
		true
	}

	fn show_node_menu(
		&mut self,
		nodeid: egui_snarl::NodeId,
		_inputs: &[egui_snarl::InPin],
		_outputs: &[egui_snarl::OutPin],
		ui: &mut egui::Ui,
		snarl: &mut egui_snarl::Snarl<Node>,
	) {
		let mut logd = self.logd.write().unwrap();
		if ui.button("Delete").clicked() {
			let node = snarl.get_node_mut(nodeid).unwrap();
			logd.prune(node.lid);
			snarl.remove_node(nodeid);
			ui.close();
		}
	}
}

fn add_signal_input_box(
	ui: &mut egui::Ui,
	signal: &mut Signal,
	nets: &mut (bool, bool),
	text: &mut String,
) {
	let parse: Option<Signal> = {
		let sig_parser = arithmetic_parser::signal_parser();
		let res = sig_parser.parse(&text);
		res.into_output()
	};
	ui.vertical(|ui| {
		let edit_left =
			egui::TextEdit::singleline(text).desired_width(ui.spacing().text_edit_width / 2.0);
		let res = if parse.is_none() {
			ui.add(edit_left.background_color(Color32::from_rgb(0x70, 0x00, 0x00)))
		} else {
			ui.add(edit_left)
		};
		if res.lost_focus() {
			if let Some(sig) = parse {
				*signal = sig;
			}
			*text = signal.unparse();
		}
		ui.horizontal(|ui| {
			ui.checkbox(&mut nets.0, "R");
			ui.checkbox(&mut nets.1, "G");
		});
	});
}

fn add_signal_input_box_name_only_label(
	ui: &mut egui::Ui,
	signal: &mut Signal,
	text: &mut String,
	name: &str,
) {
	let parse: Option<Signal> = {
		let sig_parser = arithmetic_parser::signal_parser();
		let res = sig_parser.parse(&text);
		res.into_output()
	};
	ui.vertical(|ui| {
		ui.label(name);
		let edit_left =
			egui::TextEdit::singleline(text).desired_width(ui.spacing().text_edit_width / 2.0);
		let res = if parse.is_none() {
			ui.add(edit_left.background_color(Color32::from_rgb(0x70, 0x00, 0x00)))
		} else {
			ui.add(edit_left)
		};
		if res.lost_focus() {
			if let Some(sig) = parse {
				*signal = sig;
			}
			*text = signal.unparse();
		}
	});
}

fn add_signal_input_box_name_only(ui: &mut egui::Ui, signal: &mut Signal, text: &mut String) {
	let parse: Option<Signal> = {
		let sig_parser = arithmetic_parser::signal_parser();
		let res = sig_parser.parse(&text);
		res.into_output()
	};
	let edit_left =
		egui::TextEdit::singleline(text).desired_width(ui.spacing().text_edit_width / 2.0);
	let res = if parse.is_none() {
		ui.add(edit_left.background_color(Color32::from_rgb(0x70, 0x00, 0x00)))
	} else {
		ui.add(edit_left)
	};
	if res.lost_focus() {
		if let Some(sig) = parse {
			*signal = sig;
		}
		*text = signal.unparse();
	}
}

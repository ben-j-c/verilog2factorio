use chumsky::Parser;
use egui::{output, Color32, Id};
use egui_snarl::ui::{PinInfo, SnarlStyle, WireLayer};

use crate::logical_design::{
	self, arithmetic_parser, decider_parser, ArithmeticOperator, DeciderOperator, Signal,
	WireColour,
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

#[derive(serde::Deserialize, serde::Serialize)]
enum Node {
	Arithmetic {
		lid: NodeId,
		left: String,
		right: String,
		output: String,
	},
	Decider {
		lid: NodeId,
		inputs: String,
		parse_bad: bool,
		outputs: Vec<String>,
	},
	Constant {
		lid: NodeId,
		outputs: Vec<String>,
	},
	Lamp {
		lid: NodeId,
		left: String,
		right: String,
	},
	DisplayPanel {
		lid: NodeId,
		lefts: Vec<String>,
		rights: Vec<String>,
		icons: Vec<String>,
	},
}

#[derive(Debug, Default, serde::Deserialize, serde::Serialize)]
struct V2FViewer {
	logd: LogDRef,
}

impl V2FViewer {
	fn add_decider(&mut self) -> Node {
		let mut logd = self.logd.write().unwrap();
		let lid = logd.add_decider();
		logd.set_decider_expr(lid, "none < none");
		Node::Decider {
			lid,
			inputs: "none < none".to_owned(),
			parse_bad: false,
			outputs: vec![],
		}
	}

	fn add_arithmetic(&mut self) -> Node {
		let mut logd = self.logd.write().unwrap();
		Node::Arithmetic {
			lid: logd.add_arithmetic(
				(Signal::None, ArithmeticOperator::Add, Signal::None),
				Signal::None,
			),
			left: "none".to_owned(),
			right: "none".to_owned(),
			output: "none".to_owned(),
		}
	}

	fn add_constant(&mut self) -> Node {
		let mut logd = self.logd.write().unwrap();
		Node::Constant {
			lid: logd.add_constant(vec![], vec![]),
			outputs: vec![],
		}
	}

	fn add_lamp(&mut self) -> Node {
		let mut logd = self.logd.write().unwrap();
		Node::Lamp {
			lid: logd.add_lamp((Signal::None, DeciderOperator::Equal, Signal::None)),
			left: "none".to_owned(),
			right: "none".to_owned(),
		}
	}

	fn add_display_panel(&mut self) -> Node {
		let mut logd = self.logd.write().unwrap();
		Node::DisplayPanel {
			lid: logd.add_display_panel(),
			lefts: vec![],
			rights: vec![],
			icons: vec![],
		}
	}
}

impl Node {
	fn get_lid(&self) -> NodeId {
		*match self {
			Node::Arithmetic { lid, .. } => lid,
			Node::Decider { lid, .. } => lid,
			Node::Constant { lid, .. } => lid,
			Node::Lamp { lid, .. } => lid,
			Node::DisplayPanel { lid, .. } => lid,
		}
	}
}

impl egui_snarl::ui::SnarlViewer<Node> for V2FViewer {
	fn title(&mut self, node: &Node) -> String {
		let logd = self.logd.read().unwrap();
		let lnode = logd.get_node(node.get_lid());
		let pfx = match &node {
			Node::Arithmetic { .. } => "Arithmetic",
			Node::Decider { .. } => "Decider",
			Node::Constant { .. } => "Constant",
			Node::Lamp { .. } => "Lamp",
			Node::DisplayPanel { .. } => "Display panel",
		};
		match &lnode.description {
			Some(d) => format!("{} {}", pfx, d),
			None => format!("{}", pfx),
		}
	}

	fn inputs(&mut self, node: &Node) -> usize {
		match node {
			Node::Arithmetic { .. } => 2,
			Node::Decider { .. } => 2,
			Node::Constant { .. } => 0,
			Node::Lamp { .. } => 2,
			Node::DisplayPanel { .. } => 2,
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
		match node {
			Node::Arithmetic { .. } => 2,
			Node::Decider { .. } => 2,
			Node::Constant { .. } => 2,
			Node::Lamp { .. } => 0,
			Node::DisplayPanel { .. } => 0,
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
			(lhs.get_lid(), rhs.get_lid())
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
			(lhs.get_lid(), rhs.get_lid())
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
		_inputs: &[egui_snarl::InPin],
		_outputs: &[egui_snarl::OutPin],
		ui: &mut egui::Ui,
		snarl: &mut egui_snarl::Snarl<Node>,
	) {
		let mut logd = self.logd.write().unwrap();
		let node = if let Some(node) = snarl.get_node_mut(node) {
			node
		} else {
			return;
		};
		match node {
			Node::Arithmetic {
				lid,
				left,
				right,
				output,
			} => {
				let lnode = logd.mut_node(*lid);
				let (op, logd_left, logd_right, net_left, net_right) =
					lnode.function.unwrap_arithmetic_mut();
				ui.vertical(|ui| {
					add_signal_input_box(ui, logd_left, net_left, left);
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
					add_signal_input_box(ui, logd_right, net_right, right);
					add_signal_input_box_name_only_label(
						ui,
						&mut lnode.output[0],
						output,
						"Output",
					);
				});
			},
			Node::Decider {
				lid,
				inputs,
				parse_bad,
				outputs,
			} => {
				ui.vertical(|ui| {
					ui.label("Decider Sum-of-Products Expression");
					let mut text_edit = egui::TextEdit::multiline(inputs);
					if *parse_bad {
						text_edit = text_edit.background_color(Color32::from_rgb(0x70, 0x00, 0x00))
					}
					let interact = ui.add(text_edit);
					if interact.lost_focus() {
						let parse: Option<decider_parser::DeciderSop> = {
							let sig_parser = decider_parser::parser();
							let res = sig_parser.parse(inputs);
							res.into_output()
						};
						if let Some(expr) = parse {
							logd.set_decider_inputs(*lid, &expr);
						}
					}
					if ui.button("Add output").clicked() {
						outputs.push("none".to_owned());
						logd.add_decider_out_input_count(*lid, Signal::None, (true, true));
					}
					let mut i = 0;
					while i < outputs.len() {
						ui.horizontal(|ui| {
							if ui.button("X").clicked() {
								outputs.remove(i);
								logd.remove_decider_output(*lid, i);
								i -= 1;
								return;
							}
							add_signal_input_box_name_only_no_constants(
								ui,
								&mut logd.mut_node(*lid).output[i],
								&mut outputs[i],
							);
							let lnode = logd.mut_node(*lid);
							let (_, _, _, _, output_network, use_input_count, constants) =
								lnode.function.unwrap_decider_mut();
							ui.checkbox(&mut use_input_count[i], "Use input count");
							if use_input_count[i] {
								if constants[i].is_none() {
									constants[i] = Some(1);
								}
								let dv =
									egui::DragValue::new(constants[i].as_mut().unwrap()).speed(1);
								ui.add(dv);
							} else {
								if constants[i].is_some() {
									constants[i] = None;
								}
							}
							ui.checkbox(&mut output_network[i].0, "R");
							ui.checkbox(&mut output_network[i].1, "G");
						});
						i += 1;
					}
				});
			},
			Node::Constant { lid, outputs } => {
				let lnode = logd.mut_node(*lid);
				ui.vertical(|ui| {
					let (enabled, constants) = lnode.function.unwrap_constant_mut();
					ui.checkbox(enabled, "Enabled");
					if ui.button("Add constant").clicked() {
						lnode.output.push(Signal::None);
						outputs.push("none".to_owned());
						constants.push(0);
					}
					for i in 0..constants.len() {
						ui.horizontal(|ui| {
							add_signal_input_box_name_only_no_constants(
								ui,
								&mut lnode.output[i],
								&mut outputs[i],
							);
							let dv = egui::DragValue::new(&mut constants[i]).speed(1);
							ui.add(dv);
						});
					}
				});
			},
			Node::Lamp { lid, left, right } => {
				let lnode = logd.mut_node(*lid);
				let expression = lnode.function.unwrap_lamp_mut();
				ui.horizontal(|ui| {
					add_signal_input_box_name_only(ui, &mut expression.0, left);

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

					add_signal_input_box_name_only(ui, &mut expression.2, right);
				});
			},
			Node::DisplayPanel {
				lid,
				lefts,
				rights,
				icons,
			} => {
				let lnode = logd.mut_node(*lid);
				let (input_1, input_2, op, text) = lnode.function.unwrap_display_panel_mut();
				ui.vertical(|ui| {
					if ui.button("Add").clicked() {
						lnode.output.push(Signal::None);
						icons.push("none".to_owned());

						input_1.push(Signal::None);
						lefts.push("none".to_owned());

						input_2.push(Signal::None);
						rights.push("none".to_owned());

						text.push(None);
						op.push(DeciderOperator::LessThan);
					}
					for i in 0..input_1.len() {
						ui.horizontal(|ui| {
							add_signal_input_box_name_only(ui, &mut lnode.output[i], &mut icons[i]);

							let mut tmp = false;
							if let Some(text) = &mut text[i] {
								let edit_left = egui::TextEdit::singleline(text)
									.desired_width(ui.spacing().text_edit_width / 2.0);
								ui.add(edit_left);
							} else if ui.checkbox(&mut tmp, "<no label>").clicked() {
								text[i] = Some(String::new());
							}

							add_signal_input_box_name_only(ui, &mut input_1[i], &mut lefts[i]);

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

							add_signal_input_box_name_only(ui, &mut input_2[i], &mut rights[i]);
						});
					}
				});
			},
		}
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
			logd.prune(node.get_lid());
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

fn add_signal_input_box_name_only_no_constants(
	ui: &mut egui::Ui,
	signal: &mut Signal,
	text: &mut String,
) {
	let parse: Option<Signal> = {
		let sig_parser = arithmetic_parser::signal_parser_no_constant();
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

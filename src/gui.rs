use std::path::PathBuf;

use chumsky::Parser;
use egui::{Color32, Id};
use egui_extras::syntax_highlighting::CodeTheme;
use egui_snarl::ui::{PinInfo, SnarlStyle, WireLayer};
use itertools::Itertools;

use crate::logical_design::{
	arithmetic_parser, decider_parser, ArithmeticOperator, DeciderOperator, LogicalDesign, Signal,
	WireColour,
};
use crate::phy::PhysicalDesign;
use crate::serializable_design::SerializableDesign;
use crate::signal_lookup_table;
use crate::sim::SimState;
use crate::{logical_design::NodeId, LogDRef};

#[derive(serde::Deserialize, serde::Serialize)]
#[serde(default)]
pub struct V2FApp {
	snarl: egui_snarl::Snarl<Node>,
	viewer: V2FViewer,

	blueprint: Option<String>,

	cwd: PathBuf,
}

impl Default for V2FApp {
	fn default() -> Self {
		let snarl = egui_snarl::Snarl::new();
		Self {
			snarl,
			viewer: V2FViewer::default(),
			blueprint: None,
			cwd: std::env::current_dir().unwrap(),
		}
	}
}

impl V2FApp {
	pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
		if let Some(storage) = cc.storage {
			let mut ret: Self = eframe::get_value(storage, eframe::APP_KEY).unwrap_or_default();
			if let Some((sim, _)) = &mut ret.viewer.sim {
				sim.reinit_logd(ret.viewer.logd.clone());
			}
			match std::env::set_current_dir(&ret.cwd) {
				Ok(()) => {},
				Err(_) => {
					println!("Failed to open {:?}", ret.cwd);
					println!("Falling back to default.");
					return Default::default();
				},
			}
			ret
		} else {
			Default::default()
		}
	}
}

impl eframe::App for V2FApp {
	fn save(&mut self, storage: &mut dyn eframe::Storage) {
		eframe::set_value(storage, eframe::APP_KEY, self);
	}

	fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
		egui::TopBottomPanel::top("top_panel").show(ctx, |ui| {
			egui::MenuBar::new().ui(ui, |ui| {
				#[cfg(not(target_arch = "wasm32"))]
				{
					ui.menu_button("File", |ui| {
						if ui.button("New").clicked() {
							*self = V2FApp::default();
						}
						if ui.button("Change Directory").clicked() {
							let task = rfd::FileDialog::new().pick_folder();
							if let Some(path) = task {
								println!("Selected path: {:?}", path);
								match std::env::set_current_dir(&path) {
									Ok(()) => {
										*self = V2FApp::default();
										self.cwd = path.clone();
									},
									Err(_) => println!("Failed to switch to different directory."),
								}
							}
						}
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
			egui::ScrollArea::vertical().show(ui, |ui| {
				#[cfg(debug_assertions)]
				if ui.button("Dump Logical Design").clicked() {
					println!("{}", self.viewer.logd.read().unwrap());
				}
				ui.horizontal(|ui| {
					if ui.button("New Simulation").clicked() {
						let logd = self.viewer.logd.clone();
						self.viewer.sim = Some((SimState::new(logd), SimSettings::default()));
					}
					if ui.button("Delete").clicked() {
						self.viewer.sim = None;
					}
				});
				if let Some((sim, settings)) = &mut self.viewer.sim {
					ui.label("Step sim");
					ui.horizontal(|ui| {
						let dv = egui::DragValue::new(&mut settings.n_steps).speed(1);
						ui.add(dv);
						if ui.button("N").clicked() {
							sim.step(settings.n_steps);
						}
						if ui.button("1").clicked() {
							sim.step(1);
						}
						if ui.button("10").clicked() {
							sim.step(10);
						}
						if ui.button("60").clicked() {
							sim.step(60);
						}
					});
					ui.checkbox(&mut settings.show_on_nodes, "Show on nodes");
					ui.label(format!("{:?}", sim.seq_num()));
					ui.label(format!("{:?}", self.viewer.logd.read().unwrap().seq_num));
				}
				ui.separator();
				if ui.button("Get blueprint").clicked() {
					let mut logd = self.viewer.logd.read().unwrap().clone();
					logd.delete_and_compact();
					let mut phyd = PhysicalDesign::new();
					phyd.build_from(&logd);
					let mut serd = SerializableDesign::new();
					serd.build_from(&phyd, &logd);
					let text = serde_json::to_string_pretty(&serd);
					match text {
						Ok(text) => self.blueprint = Some(text),
						Err(e) => self.blueprint = Some(format!("{}", e)),
					}
				}
				let mut is_open = true;
				if let Some(text) = &self.blueprint {
					egui::Window::new("Blueprint")
						.open(&mut is_open)
						.resizable(true)
						.scroll(true)
						.show(ctx, |ui| {
							if !cfg!(target_arch = "wasm32") {
								if ui.button("Copy to clipboard").clicked() {
									ctx.copy_text(text.clone());
								}
							}
							egui_extras::syntax_highlighting::code_view_ui(
								ui,
								&CodeTheme::default(),
								&text,
								"json",
							);
						});
					if !is_open {
						self.blueprint = None;
					}
				}
			});
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
	WireHubRed {
		lid: NodeId,
	},
	WireHubGreen {
		lid: NodeId,
	},
}

#[derive(Debug, Default, serde::Deserialize, serde::Serialize)]
struct V2FViewer {
	pub(crate) logd: LogDRef,
	pub(crate) sim: Option<(SimState, SimSettings)>,
}

#[derive(Debug, Default, serde::Deserialize, serde::Serialize)]
struct SimSettings {
	n_steps: u32,
	show_on_nodes: bool,
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

	fn add_wire(&mut self, colour: WireColour) -> Node {
		let mut logd = self.logd.write().unwrap();
		match colour {
			WireColour::Red => Node::WireHubRed {
				lid: logd.add_wire_floating_red(),
			},
			WireColour::Green => Node::WireHubGreen {
				lid: logd.add_wire_floating_green(),
			},
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
			Node::WireHubRed { lid } => lid,
			Node::WireHubGreen { lid } => lid,
		}
	}

	fn is_wire_hub(&self) -> bool {
		matches!(self, Node::WireHubRed { .. } | Node::WireHubGreen { .. })
	}

	fn get_colour(&self) -> WireColour {
		match self {
			Node::WireHubRed { .. } => WireColour::Red,
			Node::WireHubGreen { .. } => WireColour::Green,
			_ => unreachable!(),
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
			Node::WireHubRed { .. } => "Red wire hub",
			Node::WireHubGreen { .. } => "Green wire hub",
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
			Node::WireHubRed { .. } => 1,
			Node::WireHubGreen { .. } => 1,
		}
	}

	fn show_input(
		&mut self,
		pin: &egui_snarl::InPin,
		_ui: &mut egui::Ui,
		snarl: &mut egui_snarl::Snarl<Node>,
	) -> impl egui_snarl::ui::SnarlPin + 'static {
		let node = snarl.get_node(pin.id.node).unwrap();
		let colour = match node {
			Node::WireHubRed { .. } => Color32::from_rgb(0xb0, 0x00, 0x00),
			Node::WireHubGreen { .. } => Color32::from_rgb(0x00, 0xb0, 0x00),
			_ => {
				if pin.id.input == 0 {
					Color32::from_rgb(0xb0, 0x00, 0x00)
				} else if pin.id.input == 1 {
					Color32::from_rgb(0x00, 0xb0, 0x00)
				} else {
					unreachable!()
				}
			},
		};
		PinInfo::circle().with_fill(colour)
	}

	fn outputs(&mut self, node: &Node) -> usize {
		match node {
			Node::Arithmetic { .. } => 2,
			Node::Decider { .. } => 2,
			Node::Constant { .. } => 2,
			Node::Lamp { .. } => 0,
			Node::DisplayPanel { .. } => 0,
			Node::WireHubRed { .. } => 1,
			Node::WireHubGreen { .. } => 1,
		}
	}

	fn show_output(
		&mut self,
		pin: &egui_snarl::OutPin,
		_ui: &mut egui::Ui,
		snarl: &mut egui_snarl::Snarl<Node>,
	) -> impl egui_snarl::ui::SnarlPin + 'static {
		let node = snarl.get_node(pin.id.node).unwrap();
		let colour = match node {
			Node::WireHubRed { .. } => Color32::from_rgb(0xb0, 0x00, 0x00),
			Node::WireHubGreen { .. } => Color32::from_rgb(0x00, 0xb0, 0x00),
			_ => {
				if pin.id.output == 0 {
					Color32::from_rgb(0xb0, 0x00, 0x00)
				} else if pin.id.output == 1 {
					Color32::from_rgb(0x00, 0xb0, 0x00)
				} else {
					unreachable!()
				}
			},
		};
		PinInfo::circle().with_fill(colour)
	}

	fn connect(
		&mut self,
		from: &egui_snarl::OutPin,
		to: &egui_snarl::InPin,
		snarl: &mut egui_snarl::Snarl<Node>,
	) {
		let mut logd = self.logd.write().unwrap();
		let (lid_left, lid_right, left_hub_colour, right_hub_colour) = {
			let lhs = snarl.get_node(from.id.node).unwrap();
			let rhs = snarl.get_node(to.id.node).unwrap();
			(
				lhs.get_lid(),
				rhs.get_lid(),
				if lhs.is_wire_hub() {
					Some(lhs.get_colour())
				} else {
					None
				},
				if rhs.is_wire_hub() {
					Some(rhs.get_colour())
				} else {
					None
				},
			)
		};
		if left_hub_colour.is_some() && right_hub_colour.is_some() {
			return;
		}
		if left_hub_colour.is_some() || right_hub_colour.is_some() {
			let colour = left_hub_colour.or(right_hub_colour).unwrap();
			let colour_other = if left_hub_colour.is_some() {
				terminal_to_colour(to.id.input)
			} else {
				terminal_to_colour(from.id.output)
			};

			if colour != colour_other {
				return;
			}
			if snarl.connect(from.id, to.id) {
				logd.connect(lid_left, lid_right, colour);
			}
		} else if from.id.output != to.id.input {
			return;
		} else if snarl.connect(from.id, to.id) {
			if from.id.output == 0 {
				logd.add_wire_red_simple(lid_left, lid_right);
			} else {
				logd.add_wire_green_simple(lid_left, lid_right);
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
		let (lid_left, lid_right, left_hub_colour, right_hub_colour) = {
			let lhs = snarl.get_node(from.id.node).unwrap();
			let rhs = snarl.get_node(to.id.node).unwrap();
			(
				lhs.get_lid(),
				rhs.get_lid(),
				if lhs.is_wire_hub() {
					Some(lhs.get_colour())
				} else {
					None
				},
				if rhs.is_wire_hub() {
					Some(rhs.get_colour())
				} else {
					None
				},
			)
		};
		if !snarl.disconnect(from.id, to.id) {
			return;
		}
		if left_hub_colour.is_some() || right_hub_colour.is_some() {
			logd.disconnect(
				lid_left,
				lid_right,
				left_hub_colour.or(right_hub_colour).unwrap(),
			);
		} else {
			logd.find_wire_between_disconnect(
				lid_left,
				lid_right,
				terminal_to_colour(from.id.output),
			);
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
		let mut change = false;
		if ui.button("Decider").clicked() {
			snarl.insert_node(pos, self.add_decider());
			change = true;
		}
		if ui.button("Arithmetic").clicked() {
			snarl.insert_node(pos, self.add_arithmetic());
			change = true;
		}
		if ui.button("Constant").clicked() {
			snarl.insert_node(pos, self.add_constant());
			change = true;
		}
		if ui.button("Lamp").clicked() {
			snarl.insert_node(pos, self.add_lamp());
			change = true;
		}
		if ui.button("Display panel").clicked() {
			snarl.insert_node(pos, self.add_display_panel());
			change = true;
		}
		if ui.button("Red wire hub").clicked() {
			snarl.insert_node(pos, self.add_wire(WireColour::Red));
			change = true;
		}
		if ui.button("Green wire hub").clicked() {
			snarl.insert_node(pos, self.add_wire(WireColour::Green));
			change = true;
		}
		if change {
			ui.close();
		}
	}

	fn has_body(&mut self, _node: &Node) -> bool {
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
		ui.vertical(|ui| {
			let mut logd = self.logd.write().unwrap();
			show_node_config(&mut logd, node, inputs, outputs, ui, snarl);
			drop(logd);
			let logd = self.logd.read().unwrap();
			show_node_sim_state(&logd, &mut self.sim, node, inputs, outputs, ui, snarl);
		});
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

fn show_node_config(
	logd: &mut LogicalDesign,
	node: egui_snarl::NodeId,
	_inputs: &[egui_snarl::InPin],
	_outputs: &[egui_snarl::OutPin],
	ui: &mut egui::Ui,
	snarl: &mut egui_snarl::Snarl<Node>,
) {
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
				add_signal_input_box_name_only_label(ui, &mut lnode.output[0], output, "Output");
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
							let dv = egui::DragValue::new(constants[i].as_mut().unwrap()).speed(1);
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
								ui.selectable_value(op, GreaterThan, GreaterThan.resolve_string());
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
		Node::WireHubRed { .. } => {},
		Node::WireHubGreen { .. } => {},
	}
}

fn show_node_sim_state(
	logd: &LogicalDesign,
	sim: &mut Option<(SimState, SimSettings)>,
	node: egui_snarl::NodeId,
	_inputs: &[egui_snarl::InPin],
	_outputs: &[egui_snarl::OutPin],
	ui: &mut egui::Ui,
	snarl: &mut egui_snarl::Snarl<Node>,
) {
	let node = if let Some(node) = snarl.get_node_mut(node) {
		node
	} else {
		return;
	};

	if let Some((sim, settings)) = sim {
		sim.update_logical_design();
		if settings.show_on_nodes {
			let lid = node.get_lid();
			let lnode = logd.get_node(lid);
			ui.vertical(|ui| {
				//
				match lnode.function {
					crate::logical_design::NodeFunction::Arithmetic { .. }
					| crate::logical_design::NodeFunction::Decider { .. } => {
						ui.add_space(16.0);
						ui.label("Outputs:");
						let state = sim.probe_red_out(lid);
						for (id, count) in state.iter().sorted() {
							ui.label(format!(
								"  {} = {}",
								signal_lookup_table::lookup_str(*id).0,
								count
							));
						}

						ui.add_space(16.0);
						ui.label("Inputs:");
						let (state_red, state_green) = sim.probe_input_state(lid);
						ui.label("  Red:");
						for id in state_red.keys().sorted() {
							ui.label(format!(
								"    {} = {}",
								signal_lookup_table::lookup_str(id).0,
								state_red[id]
							));
						}
						ui.label("  Green:");
						for id in state_green.keys().sorted() {
							ui.label(format!(
								"    {} = {}",
								signal_lookup_table::lookup_str(id).0,
								state_green[id]
							));
						}
					},
					crate::logical_design::NodeFunction::Constant { .. } => {
						//Do nothing
					},
					crate::logical_design::NodeFunction::Lamp { .. } => {
						ui.add_space(16.0);
						if let Some(true) = sim.probe_lamp_state(lid) {
							ui.colored_label(Color32::from_rgb(0xff, 0xff, 0xff), "ON");
						} else {
							ui.colored_label(Color32::from_rgb(0x40, 0x40, 0x40), "OFF");
						}
						ui.label("Inputs:");
						let (state_red, state_green) = sim.probe_input_state(lid);
						ui.label("  Red:");
						for id in state_red.keys().sorted() {
							ui.label(format!(
								"    {} = {}",
								signal_lookup_table::lookup_str(id).0,
								state_red[id]
							));
						}
						ui.label("  Green:");
						for id in state_green.keys().sorted() {
							ui.label(format!(
								"    {} = {}",
								signal_lookup_table::lookup_str(id).0,
								state_green[id]
							));
						}
					},
					crate::logical_design::NodeFunction::DisplayPanel { .. } => {
						ui.add_space(16.0);
					},
					crate::logical_design::NodeFunction::WireSum(WireColour::Red) => {},
					crate::logical_design::NodeFunction::WireSum(WireColour::Green) => {},
				}
			});
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

fn terminal_to_colour(terminal: usize) -> WireColour {
	match terminal {
		0 => WireColour::Red,
		1 => WireColour::Green,
		_ => unreachable!(),
	}
}

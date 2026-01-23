use std::{
	io,
	sync::{Arc, RwLock},
};

use ratatui::{
	buffer::Buffer,
	crossterm::{
		self,
		event::{self, Event, KeyCode, KeyEvent, KeyEventKind},
	},
	layout::Rect,
	style::Stylize,
	symbols::border,
	text::{Line, Text},
	widgets::{Block, Paragraph, Widget},
	DefaultTerminal, Frame,
};

use crate::{logical_design::LogicalDesign, sim::SimState};

pub fn run_tui(logd: Arc<RwLock<LogicalDesign>>, sim: Arc<RwLock<SimState>>) -> Result<(), String> {
	color_eyre::install().map_err(|e| e.to_string())?;
	ratatui::run(|terminal: &mut DefaultTerminal| app(terminal, logd, sim))
		.map_err(|e| e.to_string())?;
	Ok(())
}

fn app(
	terminal: &mut DefaultTerminal,
	logd: Arc<RwLock<LogicalDesign>>,
	phy: Arc<RwLock<SimState>>,
) -> std::io::Result<()> {
	loop {
		terminal.draw(render)?;
		if crossterm::event::read()?.is_key_press() {
			break Ok(());
		}
	}
}

fn render(frame: &mut Frame) {
	frame.render_widget("hello world", frame.area());
}

struct App {
	exit: bool,
	logd: Arc<RwLock<LogicalDesign>>,
	sim: Arc<RwLock<SimState>>,
}

impl App {
	fn draw(&self, frame: &mut Frame) {
		frame.render_widget(self, frame.area());
	}

	fn handle_events(&mut self) -> io::Result<()> {
		match event::read()? {
			// it's important to check that the event is a key press event as
			// crossterm also emits key release and repeat events on Windows.
			Event::Key(key_event) if key_event.kind == KeyEventKind::Press => {
				self.handle_key_event(key_event)
			},
			_ => {},
		};
		Ok(())
	}
	fn handle_key_event(&mut self, key_event: KeyEvent) {
		match key_event.code {
			KeyCode::Char('q') => self.exit = true,

			_ => {},
		}
	}
}

impl Widget for &App {
	fn render(self, area: Rect, buf: &mut Buffer) {
		let title = Line::from(" Counter App Tutorial ".bold());
		let instructions = Line::from(vec![
			" Decrement ".into(),
			"<Left>".blue().bold(),
			" Increment ".into(),
			"<Right>".blue().bold(),
			" Quit ".into(),
			"<Q> ".blue().bold(),
		]);
		let block = Block::bordered()
			.title(title.centered())
			.title_bottom(instructions.centered())
			.border_set(border::THICK);

		let counter_text = Text::from(vec![Line::from(vec!["Hello".into()])]);

		Paragraph::new(counter_text)
			.centered()
			.block(block)
			.render(area, buf);
	}
}

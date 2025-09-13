mod config;
mod design;
mod partition;
mod placement;
mod placement2;
mod route;

pub use design::*;

static CFG: std::sync::LazyLock<config::Config> = std::sync::LazyLock::new(|| {
	std::fs::read("cfg/phy.toml")
		.map(|bytes| {
			//
			toml::from_slice::<config::Config>(&bytes)
		})
		.unwrap_or_else(|_| Ok(config::Config::default()))
		.unwrap_or_else(|_| config::Config::default())
});

pub(crate) fn dump_phy_cfg() {
	let cfg = config::Config::default();
	let cfg = toml::to_string(&cfg).unwrap();
	std::fs::create_dir_all("cfg").unwrap();
	std::fs::write("cfg/phy_template.toml", cfg).unwrap();
}

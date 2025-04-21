use crate::lua;

#[test]
fn simple() {
	let lua = lua::get_lua().unwrap();
	lua.load(
		r#"
		local logd = get_empty_design()
		logd:print()
	"#,
	)
	.exec()
	.unwrap();
}

use mlua::AnyUserData;

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

#[test]
fn simple2() {
	let lua = lua::get_lua().unwrap();
	let logd = lua
		.load(
			r#"
		local logd = get_empty_design()
		logd:add_decider()
		return logd
	"#,
		)
		.eval::<AnyUserData>()
		.unwrap()
		.borrow::<lua::LogicalDesignAPI>()
		.unwrap();
	assert_eq!(logd.logd.borrow().nodes.len(), 1);
}

#[test]
fn single_decider() {
	let lua = lua::get_lua().unwrap();
	let logd = lua
		.load(
			r#"
		local logd = get_empty_design()
		d1 = logd:add_decider()
		d1:add_output(Each, nil, NET_GREEN)
		d1:add_condition(FirstRow, Expr(Each, ">", 0), NET_REDGREEN, NET_NONE)
		c1 = logd:add_constant({ "iron-plate", "signal-red", "crude-oil" }, {10, -10, 50000})
		l1 = logd:add_lamp(Expr("iron-plate", "==", 10))

		d1.input.red:connect(c1.output)
		d1.output.green:connect(l1.input)

		return logd
	"#,
		)
		.eval::<AnyUserData>()
		.unwrap()
		.borrow::<lua::LogicalDesignAPI>()
		.unwrap();
	assert_eq!(logd.logd.borrow().nodes.len(), 5);
}

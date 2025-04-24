-- A simple example showing all the common features.

local logd = get_empty_design()

local d1 = logd:add_decider()
d1:add_output(Each, nil, NET_GREEN)   -- Use input count
d1:add_output("signal-1", 1, NET_RED) -- Use a constant
d1:add_condition(FirstRow, Expr(Each, ">", 0), NET_REDGREEN, NET_NONE)
d1:add_condition(AND, Expr(Anything, ">", 0), NET_REDGREEN, NET_NONE)
d1:add_condition(OR, Expr("signal-0", "==", -100), NET_REDGREEN, NET_NONE)

local c1 = logd:add_constant({ "iron-plate", "signal-red", "crude-oil" }, { 10, -10, 50000 })
local l1 = logd:add_lamp(Expr("iron-plate", "==", 11))
d1.input.red:connect(c1.output)
d1.output.green:connect(l1.input)

local sig_0 = Signal("signal-0")
local a1 = logd:add_arithmetic(sig_0 - 1, sig_0, NET_REDGREEN, NET_NONE)
local c2 = logd:add_constant({ sig_0 }, { -99 })
connect(c2.output.red, a1.input)
connect(a1.output.green, l1.input)

logd:make_svg()
logd:print()

return logd

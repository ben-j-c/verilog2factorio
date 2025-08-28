-- A simple example showing how to load a design and test it.

rtl = yosys_load_rtl("alu.v", "alu")
logd = yosys_map_rtl(rtl)
logd:make_svg()

data_a = logd:find_in_port("data_a") or error()
data_b = logd:find_in_port("data_b") or error()
select = logd:find_in_port("select") or error()
result_y = logd:find_out_port("result_y") or error()

data_a:set_outputs({ Signal("signal_a") }, { 3 })
data_b:set_outputs({ Signal("signal_b") }, { 4 })
select:set_outputs({ Signal("signal_s") }, { 0 })

sim = logd:new_simulation()

sim:step(1)

return logd

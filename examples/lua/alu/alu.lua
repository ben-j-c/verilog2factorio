-- A simple example showing how to load a design and test it.
rtl = yosys_load_rtl("alu.v", "alu")
logd = yosys_map_rtl(rtl)
logd:make_svg()

data_a = logd:find_in_port("data_a") or error()
data_b = logd:find_in_port("data_b") or error()
select = logd:find_in_port("select") or error()
result_y = logd:find_out_port("result_y") or error()

a_sig = data_a:signals()[1]
b_sig = data_b:signals()[1]
s_sig = select:signals()[1]
y_sig = result_y:signals()[1]
data_a:set_outputs({ a_sig }, { 3 })
data_b:set_outputs({ b_sig }, { 4 })
select:set_outputs({ s_sig }, { 0 })

sim = logd:new_simulation()

sim:step(4)
if (sim:probe(result_y.input)[y_sig] ~= 7) then
	error()
end

select:set_outputs({ s_sig }, { 1 })
sim:step(4)
if (sim:probe(result_y.input)[y_sig] ~= -1) then
	error()
end

select:set_outputs({ s_sig }, { 2 })
sim:step(4)
if (sim:probe(result_y.input)[y_sig] ~= 12) then
	error()
end

select:set_outputs({ s_sig }, { 3 })
sim:step(4)
if (sim:probe(result_y.input)[y_sig] ~= 0) then
	error()
end

select:set_outputs({ s_sig }, { 4 })
sim:step(4)
if (sim:probe(result_y.input)[y_sig] ~= 3) then
	error()
end

select:set_outputs({ s_sig }, { 5 })
sim:step(4)
if (sim:probe(result_y.input)[y_sig] ~= 4) then
	error()
end

select:set_outputs({ s_sig }, { 6 })
sim:step(4)
if (sim:probe(result_y.input)[y_sig] ~= 81) then
	error()
end

select:set_outputs({ s_sig }, { 7 })
sim:step(4)
if (sim:probe(result_y.input)[y_sig] ~= -3) then
	error()
end

select:set_outputs({ s_sig }, { 8 })
sim:step(4)
if (sim:probe(result_y.input)[y_sig] ~= 7) then
	error()
end

select:set_outputs({ s_sig }, { 9 })
sim:step(4)
if (sim:probe(result_y.input)[y_sig] ~= 0) then
	error()
end

select:set_outputs({ s_sig }, { 10 })
sim:step(4)
if (sim:probe(result_y.input)[y_sig] ~= 7) then
	error()
end

select:set_outputs({ s_sig }, { 11 })
sim:step(4)
if (sim:probe(result_y.input)[y_sig] ~= 0) then
	error()
end

select:set_outputs({ s_sig }, { 12 })
sim:step(4)
if (sim:probe(result_y.input)[y_sig] ~= 0) then
	error()
end

select:set_outputs({ s_sig }, { 13 })
sim:step(4)
if (sim:probe(result_y.input)[y_sig] ~= 48) then
	--sim:inspect()
	error()
end

select:set_outputs({ s_sig }, { 14 })
sim:step(4)
if (sim:probe(result_y.input)[y_sig] ~= 0) then
	error()
end

select:set_outputs({ s_sig }, { 15 })
sim:step(4)
if (sim:probe(result_y.input)[y_sig] ~= 3) then
	error()
end

-- Now reset and test the VCD
inputs = {}
inputs["tb.data_a"] = data_a
inputs["tb.data_b"] = data_b
inputs["tb.select"] = select
outputs = {}
outputs["tb.result_y"] = result_y
os.execute("./makevcd")
if not sim:apply_vcd("alu_testbench.vcd", inputs, outputs, 4, true) then
	error()
end
print("ALU sim matches VCD")

return logd

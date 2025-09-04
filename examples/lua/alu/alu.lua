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
print(#{ a_sig })
data_a:set_outputs({ a_sig }, { 3 })
data_b:set_outputs({ b_sig }, { 4 })
select:set_outputs({ s_sig }, { 0 })

sim = logd:new_simulation()

sim:step(10)

if (sim:probe(result_y.input)[y_sig] ~= 7) then
	print("ERROR")
	print("Expected: 7")
	print("Got: " .. sim:probe(result_y.input)[y_sig])
	print(sim:probe(result_y.input))
	print("\nLogical design:")
	logd:print()
	print("\nSim:")
	sim:print()
	sim:inspect()
	sim:step(1)
	sim:probe(result_y.input)
	error()
end

return logd

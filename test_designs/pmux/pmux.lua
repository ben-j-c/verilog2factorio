name = "pmux"
module = name
module_file = "./" .. module .. ".v"
vcd_file = "." .. name .. "_tb.vcd"
delay = 6

rtl = yosys_load_rtl(module_file, module)
logd = yosys_map_rtl(rtl)


inputs = {}
for index, value in pairs(logd:in_ports()) do
	inputs["tb." .. index] = value
end

outputs = {}
for index, value in pairs(logd:out_ports()) do
	outputs["tb.dut." .. index] = value
end

if not os.execute("../makevcd") then
	error("makevcd failed")
end
sim = logd:new_simulation()
if not sim:apply_vcd(vcd_file, inputs, outputs, delay, true) then
	sim:save_svg(name .. "_failure.svg")
	sim:inspect()
	--error("apply vcd failed")
end
print(name .. " sim matches VCD")

logd:make_svg()
return logd

name = "ram"
module = name
module_file = module .. ".v"
vcd_file = name .. "_tb.vcd"
delay = 10

rtl = yosys_load_rtl(module_file, module, "../../core/riscv")
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
arst = logd:find_in_port("ARST") or error()
wr_data = logd:find_in_port("WR_DATA") or error()
wr_clk = logd:find_in_port("WR_CLK") or error()
wr_bsel = logd:find_in_port("BYTE_SELECT") or error()

--arst:set_ith_output_count(0, 1)
--sim:step(delay)
--arst:set_enabled(false)
--
--wr_clk:set_enabled(false)
--wr_clk:set_ith_output_count(0, 1)
--wr_data:set_ith_output_count(0, 100)

--sim:inspect()

if not sim:apply_vcd(vcd_file, inputs, outputs, delay, true) then
	--sim:inspect()
	error("apply vcd failed")
end
print(name .. " sim matches VCD")

logd:make_svg()
return logd

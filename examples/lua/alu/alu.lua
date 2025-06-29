-- A simple example showing how to load a design and test it.

rtl = yosys_load_rtl("alu.v", "alu")
logd = yosys_map_rtl(rtl)

return logd

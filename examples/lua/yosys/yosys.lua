-- A simple example showing how to use the lua flow with Yosys.

rtl = yosys_load_rtl("balancer.v", "balancer") -- Theres not much to do with RTL at the moment
logd = yosys_map_rtl(rtl)                      -- Simply pass it through yosys again and get the LogicalDesignAPI
logd:make_svg()

return logd

-- This example shows how to use expressions to set the inputs for deciders

-- new blank combinator design
logd = get_empty_design()

-- simple expression. Follows the sum-of-products that factorio expects.
d0 = logd:add_decider()
d0:set_conditions("signal-0 == 0 || signal-1 == 1 && iron-ore == copper-ore")

-- use [RG] to specify which colours to take signals from. Matches what the game expects.
d1 = logd:add_decider()
d1:set_conditions("signal-0[R] > 0")

-- Special signals are also supported (each, anything, everything, none)
d2 = logd:add_decider()
d2:set_conditions("each[G] == 0 || anything > 0 && everything[RG] < 0 || none == 0")


return logd

v2f_tracer = false
v2f_snapshots = { body = {} }


local function start_tracer()
	v2f_tracer = true
	v2f_snapshots = { body = {} }
end

local function stop_tracer(arg)
	v2f_tracer = false
	filename = "snapshot.json"
	text = helpers.table_to_json(v2f_snapshots)
	helpers.write_file(filename, text, false)
end

commands.add_command("trace", nil, start_tracer)
commands.add_command("strace", nil, stop_tracer)


local function take_snapshot(ev)
	if v2f_tracer ~= true then
		return
	end
	local snapshot = {}
	snapshot.event = ev
	local surface = game.surfaces[1]
	local entities = surface.find_entities()
	local sampled_entities = {}
	for _, ent in pairs(entities) do
		local behaviour = ent.get_control_behavior()
		if behaviour == nil
			or behaviour.type ~= defines.control_behavior.type.decider_combinator
			and behaviour.type ~= defines.control_behavior.type.arithmetic_combinator
		then
			goto continue
		end
		local description = ent.combinator_description
		local signals = behaviour.signals_last_tick
		sampled_entities[#sampled_entities + 1] = {
			description = description,
			signals = signals,
		}
		::continue::
	end
	local constants = {}
	for _, ent in pairs(entities) do
		local behaviour = ent.get_control_behavior()
		if behaviour == nil
			or behaviour.type ~= defines.control_behavior.type.constant_combinator
		then
			goto continue
		end
		local section = behaviour.get_section(1)
		local filter = section.filters[1]

		local constant = {
			enabled = behaviour.enabled,
			signal = filter.value.name,
			count = filter.min,
			description = ent.combinator_description,
		}
		constants[#constants + 1] = constant
		::continue::
	end
	snapshot.entities = sampled_entities
	snapshot.constants = constants
	v2f_snapshots.body[#v2f_snapshots.body + 1] = snapshot
end

script.on_event(defines.events.on_tick, take_snapshot)

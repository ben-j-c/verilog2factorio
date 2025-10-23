---@diagnostic disable: missing-return lowercase-global
---@class LogicalDesignAPI
---@field description string
---@field add_decider fun(self: LogicalDesignAPI): Decider
---@field add_constant fun(self: LogicalDesignAPI, sigs: (Signal|string)[], counts: integer[]): Constant
---@field add_arithmetic fun(self: LogicalDesignAPI, expr: ArithmeticExpr, out: Signal, network_left: Network, network_right: Network): Arithmetic
---@field add_lamp fun(self: LogicalDesignAPI, expr: DeciderExpr): Lamp
---@field print fun(self: LogicalDesignAPI)
---@field make_svg fun(self: LogicalDesignAPI)
---@field new_simulation fun(self: LogicalDesignAPI): SimStateAPI
---@field find_out_port fun(self: LogicalDesignAPI, name: string): Lamp|nil
---@field find_in_port fun(self: LogicalDesignAPI, name: string): Constant|nil
---@field in_ports fun(self: LogicalDesignAPI): table<string, Constant>
---@field out_ports fun(self: LogicalDesignAPI): table<string, Lamp>

---@class SimStateAPI
---@field step fun(self: SimStateAPI, n: integer)
---@field print fun(self: SimStateAPI)
---@field save_svg fun(self: SimStateAPI, filename: string)
---@field probe fun(self: SimStateAPI, loc: TerminalSide|Terminal): SignalTable
---@field probe_lamp_state fun(self: SimStateAPI, lamp: Lamp): boolean
---@field add_trace fun(self: SimStateAPI, node: Arithmetic|Decider|Lamp|Constant|number)
---@field inspect fun(self: SimStateAPI)
---@field apply_vcd fun(self: SimStateAPI, filename: string, inputs_lua: table, outputs_lua: table, propagation_delay: integer, reset: boolean): boolean

---@class Signal
---@field __add fun(self: Signal, other: Signal|string|integer): ArithmeticExpr
---@field __sub fun(self: Signal, other: Signal|string|integer): ArithmeticExpr
---@field __mul fun(self: Signal, other: Signal|string|integer): ArithmeticExpr
---@field __div fun(self: Signal, other: Signal|string|integer): ArithmeticExpr
---@field __mod fun(self: Signal, other: Signal|string|integer): ArithmeticExpr
---@field __shr fun(self: Signal, other: Signal|string|integer): ArithmeticExpr
---@field __shl fun(self: Signal, other: Signal|string|integer): ArithmeticExpr
---@field __band fun(self: Signal, other: Signal|string|integer): ArithmeticExpr
---@field __bor fun(self: Signal, other: Signal|string|integer): ArithmeticExpr
---@field __bxor fun(self: Signal, other: Signal|string|integer): ArithmeticExpr
---@field __eq fun(self: Signal, other: Signal): boolean

---@class Decider
---@field input TerminalSide
---@field output TerminalSide
---@field add_condition fun(self: Decider, row: DeciderRowConjDisj, expr: DeciderExpr, network_left: Network, network_right: Network)
---@field add_output fun(self: Decider, sig: Signal|string, constant_or_use_input_count: integer|nil, network: Network)
---@field signals fun(self: Decider): Signal[]

---@class Arithmetic
---@field input TerminalSide
---@field output TerminalSide
---@field signals fun(self: Arithmetic): Signal[]

---@class Lamp
---@field input TerminalSide
---@field signals fun(self: Lamp): Signal[]

---@class Constant
---@field output TerminalSide
---@field set_outputs fun(self: Constant, sigs: Signal[], values: integer[])
---@field set_ith_output_count fun(self: Constant, idx: integer, value: integer)
---@field set_enabled fun(self: Constant, status: boolean)
---@field signals fun(self: Constant): Signal[]

---@class Terminal
---@field connect fun(self: Terminal, other: TerminalSide)

---@param self Terminal
---@param other TerminalSide
function connect(self, other) end

---@class TerminalSide
---@field red Terminal
---@field green Terminal

---@class ArithmeticExpr

---@class DeciderRowConjDisj

---@class DeciderExpr

---@return DeciderExpr
---@param lhs Signal|string
---@param op string
---@param rhs Signal|string|integer
function Expr(lhs, op, rhs) end

---@class SignalTable
---@field __eq fun(self: SignalTable, other: SignalTable|table): boolean
---@field __index fun(self: SignalTable, idx: Signal): integer

Each = Signal("Each") ---@type Signal
Anything = Signal("Anything") ---@type Signal
Everything = Signal("Everything") ---@type Signal

---@return Signal
---@param name string
function Signal(name) end

---@class Network

NET_NONE = {} ---@type Network
NET_RED = {} ---@type Network
NET_GREEN = {} ---@type Network
NET_REDGREEN = {} ---@type Network

FirstRow = {} ---@type DeciderRowConjDisj
AND = {} ---@type DeciderRowConjDisj
OR = {} ---@type DeciderRowConjDisj

Terminal = {
	---@param a Terminal
	---@param b TerminalSide
	connect = function(a, b) end
}

--- Get a truely empty design
---@return LogicalDesignAPI
function get_empty_design() end

---@class RTL
---@field to_design fun(self: RTL): LogicalDesignAPI

--- Take verilog code and map it to RTL. Right now RTL doesn't do much.
---@param filename string|string[]
---@param top_mod string
---@param include_dir string|nil
---@return RTL
function yosys_load_rtl(filename, top_mod, include_dir) end

--- Take the RTL, or filename of the verilog code and turn it into a LogicalDesign.
---@param rtl RTL|string
---@return LogicalDesignAPI
function yosys_map_rtl(rtl) end

--- Enter an interactive terminal.
---@param filename string|nil
function enter_repl(filename) end

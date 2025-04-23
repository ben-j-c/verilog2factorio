---@class LogicalDesignAPI
---@field description string
---@field add_decider fun(self: LogicalDesignAPI): Decider
---@field add_constant fun(self: LogicalDesignAPI, sigs: (Signal|string)[], counts: integer[]): Constant
---@field add_arithmetic fun(self: LogicalDesignAPI, expr: ArithmeticExpr, out: Signal, network_left: Network, network_right: Network): Arithmetic
---@field add_lamp fun(self: LogicalDesignAPI, expr: DeciderExpr): Lamp
---@field print fun(self: LogicalDesignAPI)
---@field make_svg fun(self: LogicalDesignAPI)

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

---@class Decider
---@field input TerminalSide
---@field output TerminalSide
---@field add_condition fun(self: Decider, row: DeciderRowConjDisj, expr: DeciderExpr, network_left: Network, network_right: Network)
---@field add_output fun(self: Decider, sig: Signal|string, constant_or_use_input_count: integer|nil, network: Network)

---@class Arithmetic
---@field input TerminalSide
---@field output TerminalSide

---@class Lamp
---@field input TerminalSide

---@class Constant
---@field output TerminalSide

---@class Terminal

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

---@return LogicalDesignAPI
function get_empty_design() end

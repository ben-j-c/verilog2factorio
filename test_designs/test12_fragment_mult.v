module top(
	input signed [31:0] signal_0,
	input signed [31:0] signal_1,
	output [31:0] signal_Y_0,
	output [31:0] signal_Y_1,
	input signed [31:0] signal_0_2,
	input signed [31:0] signal_1_2,
	output [31:0] signal_Y_0_2,
);
	wire [63:0] tmp;
	assign tmp = signal_0*signal_1;
	assign signal_Y_0 = tmp[31:0];
	assign signal_Y_1 = tmp[63:32];
	assign signal_Y_0_2 = signal_0_2*signal_1_2;
endmodule
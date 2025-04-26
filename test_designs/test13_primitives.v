module top(
	input [31:0] signal_A_0,
	input [31:0] signal_B_0,
	output [32:0] signal_Y_0,

	input [31:0] signal_A_1,
	input [31:0] signal_B_1,
	output [31:0] signal_Y_1,

	input [31:0] signal_A_2,
	input [31:0] signal_B_2,
	output [31:0] signal_Y_2,

	input [31:0] signal_A_3,
	input [31:0] signal_B_3,
	output [31:0] signal_Y_3,

	input [31:0] signal_A_4,
	input [31:0] signal_B_4,
	output [31:0] signal_Y_4,

	input [31:0] signal_A_5,
	input [31:0] signal_B_5,
	output [31:0] signal_Y_5,

	input [31:0] signal_A_6,
	input [31:0] signal_B_6,
	output [31:0] signal_Y_6,

	input [31:0] signal_A_7,
	input [31:0] signal_B_7,
	output [31:0] signal_Y_7,

	input [31:0] signal_A_8,
	input [31:0] signal_B_8,
	output [31:0] signal_Y_8,

	input [31:0] signal_A_9,
	input [31:0] signal_B_9,
	output [31:0] signal_Y_9,
);
	assign signal_Y_0 = signal_A_0 + signal_B_0;
	assign signal_Y_1 = signal_A_1 - signal_B_1;
	assign signal_Y_2 = signal_A_2 * signal_B_2;
	assign signal_Y_3 = signal_A_3 / signal_B_3;
	assign signal_Y_4 = signal_A_4 % signal_B_4;
	assign signal_Y_5 = signal_A_5 ^ signal_B_5;
	assign signal_Y_6 = signal_A_6 & signal_B_6;
	assign signal_Y_7 = signal_A_7 | signal_B_7;
	assign signal_Y_8 = signal_A_8 >> signal_B_8;
	assign signal_Y_9 = signal_A_9 << signal_B_9;
endmodule
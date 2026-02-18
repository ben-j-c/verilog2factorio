

module sop(
	input [3:0] data_0_i,
	input [3:0] data_1_i,
	input [3:0] data_2_i,
	input [3:0] data_3_i,
	input [3:0] data_4_i,
	input [3:0] data_5_i,
	input [3:0] data_6_i,
	input [3:0] data_7_i,
	output q_0,
	output q_1
);
	wire d0 = data_0_i > 8;
	wire d1 = data_1_i < 8;
	wire d2 = data_2_i == 8;
	wire d3 = data_3_i != 8;
	wire d4 = data_4_i >= 8;
	wire d5 = data_5_i <= 8;
	wire d6 = data_6_i > data_7_i;
	wire d7 = data_7_i < data_0_i;
	assign q_0 = d1 && d2 || !d3 && d4 || !d5 && d6 || d7 && d0;
	assign q_1 = !q_0;
endmodule
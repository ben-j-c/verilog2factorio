
`ifndef cmp
`define cmp <
`endif


module compare(
	input [31:0] data_0_i,
	input [31:0] data_1_i,
	input [31:0] data_2_i,
	input [15:0] data_3_i,
	input [15:0] data_4_i,
	input [15:0] data_5_i,
	input [31:0] data_6_i,
	input [31:0] data_7_i,
	input [31:0] data_8_i,
	input [15:0] data_9_i,
	input [15:0] data_a_i,
	input [15:0] data_b_i,

	input signed [31:0] data_0_is,
	input signed [31:0] data_1_is,
	input signed [31:0] data_2_is,
	input signed [15:0] data_3_is,
	input signed [15:0] data_4_is,
	input signed [15:0] data_5_is,
	input signed [31:0] data_6_is,
	input signed [31:0] data_7_is,
	input signed [31:0] data_8_is,
	input signed [15:0] data_9_is,
	input signed [15:0] data_a_is,
	input signed [15:0] data_b_is,
	output q_01_o,
	output q_23_o,
	output q_45_o,
	output q_67_o,
	output q_89_o,
	output q_ab_o,
	
	output q_01_os,
	output q_23_os,
	output q_45_os,
	output q_67_os,
	output q_89_os,
	output q_ab_os
);
	assign q_01_o = data_0_i `cmp data_1_i;
	assign q_23_o = data_2_i `cmp data_3_i;
	assign q_45_o = data_4_i `cmp data_5_i;
	assign q_67_o = data_6_i `cmp data_7_i;
	assign q_89_o = data_8_i `cmp data_9_i;
	assign q_ab_o = data_a_i `cmp data_b_i;

	assign q_01_os = data_0_is `cmp data_1_is;
	assign q_23_os = data_2_is `cmp data_3_is;
	assign q_45_os = data_4_is `cmp data_5_is;
	assign q_67_os = data_6_is `cmp data_7_is;
	assign q_89_os = data_8_is `cmp data_9_is;
	assign q_ab_os = data_a_is `cmp data_b_is;
endmodule
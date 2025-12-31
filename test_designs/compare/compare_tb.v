`include "compare.v"

module tb;
	reg [31:0] data_0_i;
	reg [31:0] data_1_i;
	reg [31:0] data_2_i;
	reg [15:0] data_3_i;
	reg [15:0] data_4_i;
	reg [15:0] data_5_i;
	reg [31:0] data_6_i;
	reg [31:0] data_7_i;
	reg [31:0] data_8_i;
	reg [15:0] data_9_i;
	reg [15:0] data_a_i;
	reg [15:0] data_b_i;
	reg [31:0] data_0_is;
	reg [31:0] data_1_is;
	reg [31:0] data_2_is;
	reg [15:0] data_3_is;
	reg [15:0] data_4_is;
	reg [15:0] data_5_is;
	reg [31:0] data_6_is;
	reg [31:0] data_7_is;
	reg [31:0] data_8_is;
	reg [15:0] data_9_is;
	reg [15:0] data_a_is;
	reg [15:0] data_b_is;
	wire q_01_o;
	wire q_23_o;
	wire q_45_o;
	wire q_67_o;
	wire q_89_o;
	wire q_ab_o;
	wire q_01_os;
	wire q_23_os;
	wire q_45_os;
	wire q_67_os;
	wire q_89_os;
	wire q_ab_os;

	compare dut (
		.data_0_i(data_0_i),
		.data_1_i(data_1_i),
		.data_2_i(data_2_i),
		.data_3_i(data_3_i),
		.data_4_i(data_4_i),
		.data_5_i(data_5_i),
		.data_6_i(data_6_i),
		.data_7_i(data_7_i),
		.data_8_i(data_8_i),
		.data_9_i(data_9_i),
		.data_a_i(data_a_i),
		.data_b_i(data_b_i),
		.data_0_is(data_0_is),
		.data_1_is(data_1_is),
		.data_2_is(data_2_is),
		.data_3_is(data_3_is),
		.data_4_is(data_4_is),
		.data_5_is(data_5_is),
		.data_6_is(data_6_is),
		.data_7_is(data_7_is),
		.data_8_is(data_8_is),
		.data_9_is(data_9_is),
		.data_a_is(data_a_is),
		.data_b_is(data_b_is),
		.q_01_o(q_01_o),
		.q_23_o(q_23_o),
		.q_45_o(q_45_o),
		.q_67_o(q_67_o),
		.q_89_o(q_89_o),
		.q_ab_o(q_ab_o),
		.q_01_os(q_01_os),
		.q_23_os(q_23_os),
		.q_45_os(q_45_os),
		.q_67_os(q_67_os),
		.q_89_os(q_89_os),
		.q_ab_os(q_ab_os)
	);
	integer seed;
	integer i;
	initial begin
		seed = 123;
		$dumpfile("compare_tb.vcd");
		$dumpvars(0, tb);

		for (i = 0; i < 1000; i+= 1) begin
			data_0_i = $random(seed);
			data_1_i = $random(seed);
			data_2_i = $random(seed);
			data_3_i = $random(seed);
			data_4_i = $random(seed);
			data_5_i = $random(seed);
			data_6_i = $random(seed);
			data_7_i = $random(seed);
			data_8_i = $random(seed);
			data_9_i = $random(seed);
			data_a_i = $random(seed);
			data_b_i = $random(seed);
			data_0_is = $random(seed);
			data_1_is = $random(seed);
			data_2_is = $random(seed);
			data_3_is = $random(seed);
			data_4_is = $random(seed);
			data_5_is = $random(seed);
			data_6_is = $random(seed);
			data_7_is = $random(seed);
			data_8_is = $random(seed);
			data_9_is = $random(seed);
			data_a_is = $random(seed);
			data_b_is = $random(seed);
			#1;
		end
	end

endmodule
`include "pmux.v"

module tb;
	reg [2:0]     sel_i;
	reg [15:0] data_0_i;
	reg [15:0] data_1_i;
	reg [15:0] data_2_i;
	reg [15:0] data_3_i;
	reg [15:0] data_4_i;
	reg [15:0] data_5_i;
	reg [15:0] data_6_i;
	reg [15:0] data_7_i;
	wire [15:0]   q_o;

	pmux dut (
		.sel_i(sel_i),
		.data_0_i(data_0_i),
		.data_1_i(data_1_i),
		.data_2_i(data_2_i),
		.data_3_i(data_3_i),
		.data_4_i(data_4_i),
		.data_5_i(data_5_i),
		.data_6_i(data_6_i),
		.data_7_i(data_7_i),
		.q_o(q_o)
	);
	integer seed;
	integer i;
	initial begin
		seed = 123;
		$dumpfile(".pmux_tb.vcd");
		$dumpvars(0, tb);

		for (i = 0; i < 1000; i+= 1) begin
			sel_i = $random(seed);
			data_0_i = $random(seed);
			data_1_i = $random(seed);
			data_2_i = $random(seed);
			data_3_i = $random(seed);
			data_4_i = $random(seed);
			data_5_i = $random(seed);
			data_6_i = $random(seed);
			data_7_i = $random(seed);
			#1;
		end
	end

endmodule
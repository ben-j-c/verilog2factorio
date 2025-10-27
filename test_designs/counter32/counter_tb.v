`include "counter.v"

module tb;
	reg           clk_i;
	reg           rst_i;
	reg           en_i;
	wire [31:0]   q_o;

	counter dut (
		.clk_i(clk_i),
		.rst_i(rst_i),
		.en_i(en_i),
		.q_o(q_o)
	);
	integer seed;
	integer i;
	initial begin
		seed = 123;
		$dumpfile(".counter_tb.vcd");
		$dumpvars(0, tb);
		clk_i = 0;
		rst_i = 1;
		en_i = 0;

		#1;
		rst_i = 0;

		for (i = 0; i < 1000; i+= 1) begin
			en_i = $random(seed);
			clk_i = 0;
			#1;
			clk_i = 1;
			#1;
		end
	end

endmodule
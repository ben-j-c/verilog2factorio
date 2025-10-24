`include "pmux.v"

module tb;
	reg [1:0] sel;
	reg signed [31:0] data_i;
	wire signed [31:0] q_o;


	pmux dut (
		.sel(sel),
		.data_i(data_i),
		.q_o(q_o)
	);
	integer seed;
	integer i;
	initial begin
		seed = 123;
		$dumpfile(".pmux_tb.vcd");
		$dumpvars(0, tb);

		for (i = 0; i < 2_000; i+= 1) begin
			sel = i & 2'b11;
			data_i = $random(seed);
			#1;
		end
	end

endmodule
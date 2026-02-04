`include "mux.v"

module tb;
	reg [31:0] A;
	reg [31:0] B;
	reg S;
	wire [31:0] Y;

	mux dut (
		.A(A),
		.B(B),
		.S(S),
		.Y(Y)
	);
	integer seed;
	integer i;
	initial begin
		seed = 123;
		$dumpfile(".mux_tb.vcd");
		$dumpvars(0, tb);
		A = 0;
		B = 0;
		S = 0;
		#1;

		for (i = 0; i < 128; i+= 1) begin
			A = $random(seed);
			B = $random(seed);
			S = i;
			#1;
		end
	end

endmodule
`include "mux.v"

module tb;
	reg clk;
	reg arst;
	reg [31:0] A;
	reg [31:0] B;
	reg [31:0] C;
	reg [1:0] S;
	wire [31:0] Y;

	mux dut (
		.clk(clk),
		.arst(arst),
		.A(A),
		.B(B),
		.C(C),
		.S(S),
		.Y(Y)
	);
	integer seed;
	integer i;
	initial begin
		seed = 123;
		$dumpfile(".mux_tb.vcd");
		$dumpvars(0, tb);
		arst = 1;
		A = 0;
		B = 0;
		C = 0;
		S = 0;
		#1;
		arst = 0;
		#1;

		for (i = 0; i < 128; i+= 1) begin
			A = $random(seed);
			B = $random(seed);
			C = $random(seed);
			S = i;
			clk = 0;
			#1;
			clk = 1;
			#1;
		end
	end

endmodule
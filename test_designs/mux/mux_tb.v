`include "mux.v"

module tb;
	reg [31:0] A;
	reg [31:0] B;
	reg [31:0] C;
	reg [1:0] S;
	wire [31:0] Y;

	mux dut (
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

		for (i = 0; i < 4; i+= 1) begin
			A = $random(seed);
			B = $random(seed);
			C = $random(seed);
			S = i;
			#1;
		end
	end

endmodule
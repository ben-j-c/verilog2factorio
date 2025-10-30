`include "div.v"

module tb;
	reg signed [31:0] A;
	reg signed [31:0] B;
	reg [1:0] S;
	wire [31:0] Y;

	div dut (
		.A(A),
		.B(B),
		.S(S),
		.Y(Y)
	);
	integer seed;
	integer i;
	initial begin
		seed = 123;
		$dumpfile("div_tb.vcd");
		$dumpvars(0, tb);
		for (i = 0; i < 4; i+= 1) begin
			S = i;
			A = 25;
			B = 5;
			#1;
			A = -25;
			B = 5;
			#1;
			A = 32'hFFFFFFFF;
			B = -5;
			#1;
		end
	end
endmodule
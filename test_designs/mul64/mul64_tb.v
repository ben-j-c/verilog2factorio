`include "mul64.v"

module tb;
	reg [31:0] A1;
	reg [31:0] B1;
	reg [31:0] A2;
	reg [31:0] B2;
	wire [31:0] Y1;
	wire [31:0] Y2;

	mul64 dut (
		.A1(A1),
		.B1(B1),
		.A2(A2),
		.B2(B2),
		.Y1(Y1),
		.Y2(Y2)
	);
	integer seed;
	integer i;
	initial begin
		seed = 123;
		$dumpfile("mul64_tb.vcd");
		$dumpvars(0, tb);

		A1 = 3;
		A2 = 0;
		B1 = 4;
		B2 = 0;
		#1;

		A1 = -1;
		A2 = -1;
		B1 = -1;
		B2 = -1;
		#1;

		A1 = -1;
		A2 = -1;
		B1 = 4;
		B2 = 0;
		#1;

		A1 = 0;
		A2 = 1;
		B1 = 1;
		B2 = 1;
		#1;

		A1 = 1;
		A2 = 1;
		B1 = 1;
		B2 = 1;
		#1;

		A1 = -1;
		A2 = -1;
		B1 = 0;
		B2 = 0;
		#1;

		A1 = -2;
		A2 = -2;
		B1 = -2;
		B2 = -2;
		#1;
	end

endmodule
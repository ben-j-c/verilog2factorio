
module v2f_sop_not (A, Y);
	parameter WIDTH = 0;
	parameter DEPTH = 0;
	parameter TABLE = 0;

	input [WIDTH-1:0] A;
	wire Y_internal;
	output Y;

	\$sop #(.WIDTH(WIDTH), .DEPTH(DEPTH), .TABLE(TABLE))
		u0(.A(A), .Y(Y_internal));
	$_NOT_ u1(.A(Y_internal), .Y(Y));
endmodule

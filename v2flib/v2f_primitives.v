module v2f_reduce_or(A, Y);
	parameter A_SIGNED = 0;
	parameter A_WIDTH = 0;
	parameter Y_WIDTH = 0;
	input [A_WIDTH-1:0] A;
	output [Y_WIDTH-1:0] Y;
	// Blackbox
endmodule

module v2f_reduce_and(A, Y);
	parameter A_SIGNED = 0;
	parameter A_WIDTH = 0;
	parameter Y_WIDTH = 0;
	input [A_WIDTH-1:0] A;
	output [Y_WIDTH-1:0] Y;
	// Blackbox
endmodule

(*whitebox*)
module v2f_mul #( parameter A_WIDTH = 1, parameter B_WIDTH = 1, parameter Y_WIDTH = 1) (
	input [A_WIDTH-1:0] A,
	input [B_WIDTH-1:0] B,
	output [Y_WIDTH-1:0] Y,
);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	assign Y = A*B;
endmodule

module v2f_add (A, B, Y);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 0;
	parameter B_WIDTH = 0;
	parameter Y_WIDTH = 0;

	input [A_WIDTH-1:0] A;
	input [B_WIDTH-1:0] B;
	output [Y_WIDTH-1:0] Y;
	// Blackbox
endmodule

module v2f_div (A, B, Y);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 0;
	parameter B_WIDTH = 0;
	parameter Y_WIDTH = 0;

	input [A_WIDTH-1:0] A;
	input [B_WIDTH-1:0] B;
	output [Y_WIDTH-1:0] Y;
	// Blackbox
endmodule

module v2f_sub (A, B, Y);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 0;
	parameter B_WIDTH = 0;
	parameter Y_WIDTH = 0;

	input [A_WIDTH-1:0] A;
	input [B_WIDTH-1:0] B;
	output [Y_WIDTH-1:0] Y;
	// Blackbox
endmodule

module v2f_mod (A, B, Y);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 0;
	parameter B_WIDTH = 0;
	parameter Y_WIDTH = 0;

	input [A_WIDTH-1:0] A;
	input [B_WIDTH-1:0] B;
	output [Y_WIDTH-1:0] Y;
	// Blackbox
endmodule

module v2f_xor (A, B, Y);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 0;
	parameter B_WIDTH = 0;
	parameter Y_WIDTH = 0;

	input [A_WIDTH-1:0] A;
	input [B_WIDTH-1:0] B;
	output [Y_WIDTH-1:0] Y;
	// Blackbox
endmodule

module v2f_and (A, B, Y);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 0;
	parameter B_WIDTH = 0;
	parameter Y_WIDTH = 0;

	input [A_WIDTH-1:0] A;
	input [B_WIDTH-1:0] B;
	output [Y_WIDTH-1:0] Y;
	// Blackbox
endmodule

module v2f_or (A, B, Y);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 0;
	parameter B_WIDTH = 0;
	parameter Y_WIDTH = 0;

	input [A_WIDTH-1:0] A;
	input [B_WIDTH-1:0] B;
	output [Y_WIDTH-1:0] Y;
	// Blackbox
endmodule

module v2f_pow (A, B, Y);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 0;
	parameter B_WIDTH = 0;
	parameter Y_WIDTH = 0;

	input [A_WIDTH-1:0] A;
	input [B_WIDTH-1:0] B;
	output [Y_WIDTH-1:0] Y;
	// Blackbox
endmodule

(*whitebox*)
module v2f_shl #( parameter A_WIDTH = 1, parameter B_WIDTH = 1, parameter Y_WIDTH = 1) (
	input [A_WIDTH-1:0] A,
	input [B_WIDTH-1:0] B,
	output [Y_WIDTH-1:0] Y,
);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	assign Y = A << B;
endmodule

(*whitebox*)
module v2f_shr #( parameter A_WIDTH = 1, parameter B_WIDTH = 1, parameter Y_WIDTH = 1) (
	input [A_WIDTH-1:0] A,
	input [B_WIDTH-1:0] B,
	output [Y_WIDTH-1:0] Y,
);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	assign Y = A >> B;
endmodule

module v2f_gt (A, B, Y);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 0;
	parameter B_WIDTH = 0;
	parameter Y_WIDTH = 0;

	input [A_WIDTH-1:0] A;
	input [B_WIDTH-1:0] B;
	output [Y_WIDTH-1:0] Y;
	// Blackbox
endmodule

module v2f_lt (A, B, Y);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 0;
	parameter B_WIDTH = 0;
	parameter Y_WIDTH = 0;

	input [A_WIDTH-1:0] A;
	input [B_WIDTH-1:0] B;
	output [Y_WIDTH-1:0] Y;
	// Blackbox
endmodule

module v2f_ge (A, B, Y);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 0;
	parameter B_WIDTH = 0;
	parameter Y_WIDTH = 0;

	input [A_WIDTH-1:0] A;
	input [B_WIDTH-1:0] B;
	output [Y_WIDTH-1:0] Y;
	// Blackbox
endmodule

module v2f_le (A, B, Y);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 0;
	parameter B_WIDTH = 0;
	parameter Y_WIDTH = 0;

	input [A_WIDTH-1:0] A;
	input [B_WIDTH-1:0] B;
	output [Y_WIDTH-1:0] Y;
	// Blackbox
endmodule


module v2f_eq (A, B, Y);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 0;
	parameter B_WIDTH = 0;
	parameter Y_WIDTH = 0;

	input [A_WIDTH-1:0] A;
	input [B_WIDTH-1:0] B;
	output [Y_WIDTH-1:0] Y;
	// Blackbox
endmodule

module v2f_ne (A, B, Y);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 0;
	parameter B_WIDTH = 0;
	parameter Y_WIDTH = 0;

	input [A_WIDTH-1:0] A;
	input [B_WIDTH-1:0] B;
	output [Y_WIDTH-1:0] Y;
	// Blackbox
endmodule

(* techmap_celltype = "$reduce_and" *)
module v2f_rule_reduce_and(A, Y);
	parameter A_SIGNED = 0;
	parameter A_WIDTH = 0;
	parameter Y_WIDTH = 0;
	input [A_WIDTH-1:0] A;
	output [Y_WIDTH-1:0] Y;
	wire _TECHMAP_FAIL_ = A_WIDTH > 32;
	v2f_reduce_and impl(A, Y);
endmodule

(* techmap_celltype = "$reduce_or" *)
module v2f_rule_reduce_or(A, Y);
	parameter A_SIGNED = 0;
	parameter A_WIDTH = 0;
	parameter Y_WIDTH = 0;
	input [A_WIDTH-1:0] A;
	output [Y_WIDTH-1:0] Y;
	wire _TECHMAP_FAIL_ = A_WIDTH > 32;
	v2f_reduce_or impl(A, Y);
endmodule

(* techmap_celltype = "$add" *)
module v2f_rule_add (A, B, Y);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 1;
	parameter B_WIDTH = 1;
	parameter Y_WIDTH = 1;

	input [A_WIDTH-1:0] A;
	input [B_WIDTH-1:0] B;
	output [Y_WIDTH-1:0] Y;

	wire _TECHMAP_FAIL_ = A_WIDTH > 32 || B_WIDTH > 32 || Y_WIDTH > 32;
	v2f_add impl(A, B, Y);
endmodule

(* techmap_celltype = "$sub" *)
module v2f_rule_sub (A, B, Y);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 1;
	parameter B_WIDTH = 1;
	parameter Y_WIDTH = 1;

	input [A_WIDTH-1:0] A;
	input [B_WIDTH-1:0] B;
	output [Y_WIDTH-1:0] Y;

	wire _TECHMAP_FAIL_ = A_WIDTH > 32 || B_WIDTH > 32 || Y_WIDTH > 32;
	v2f_sub impl(A, B, Y);
endmodule

(* techmap_celltype = "$mul" *)
module v2f_rule_mul (A, B, Y);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 1;
	parameter B_WIDTH = 1;
	parameter Y_WIDTH = 1;

	input [A_WIDTH-1:0] A;
	input [B_WIDTH-1:0] B;
	output [Y_WIDTH-1:0] Y;

	wire _TECHMAP_FAIL_ = A_WIDTH > 32 || B_WIDTH > 32 || Y_WIDTH > 32;
	v2f_mul impl(A, B, Y);
endmodule

(* techmap_celltype = "$div" *)
module v2f_rule_div (A, B, Y);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 1;
	parameter B_WIDTH = 1;
	parameter Y_WIDTH = 1;

	input [A_WIDTH-1:0] A;
	input [B_WIDTH-1:0] B;
	output [Y_WIDTH-1:0] Y;

	wire _TECHMAP_FAIL_ = A_WIDTH > 32 || B_WIDTH > 32 || Y_WIDTH > 32;
	v2f_div impl(A, B, Y);
endmodule

(* techmap_celltype = "$mod" *)
module v2f_rule_mod (A, B, Y);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 1;
	parameter B_WIDTH = 1;
	parameter Y_WIDTH = 1;

	input [A_WIDTH-1:0] A;
	input [B_WIDTH-1:0] B;
	output [Y_WIDTH-1:0] Y;

	wire _TECHMAP_FAIL_ = A_WIDTH > 32 || B_WIDTH > 32 || Y_WIDTH > 32;
	v2f_mod impl(A, B, Y);
endmodule

(* techmap_celltype = "$and" *)
module v2f_rule_and (A, B, Y);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 1;
	parameter B_WIDTH = 1;
	parameter Y_WIDTH = 1;

	input [A_WIDTH-1:0] A;
	input [B_WIDTH-1:0] B;
	output [Y_WIDTH-1:0] Y;

	wire _TECHMAP_FAIL_ = A_WIDTH > 32 || B_WIDTH > 32 || Y_WIDTH > 32;
	v2f_and impl(A, B, Y);
endmodule

(* techmap_celltype = "$or" *)
module v2f_rule_or (A, B, Y);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 1;
	parameter B_WIDTH = 1;
	parameter Y_WIDTH = 1;

	input [A_WIDTH-1:0] A;
	input [B_WIDTH-1:0] B;
	output [Y_WIDTH-1:0] Y;

	wire _TECHMAP_FAIL_ = A_WIDTH > 32 || B_WIDTH > 32 || Y_WIDTH > 32;
	v2f_or impl(A, B, Y);
endmodule

(* techmap_celltype = "$xor" *)
module v2f_rule_xor (A, B, Y);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 1;
	parameter B_WIDTH = 1;
	parameter Y_WIDTH = 1;

	input [A_WIDTH-1:0] A;
	input [B_WIDTH-1:0] B;
	output [Y_WIDTH-1:0] Y;

	wire _TECHMAP_FAIL_ = A_WIDTH > 32 || B_WIDTH > 32 || Y_WIDTH > 32;
	v2f_xor impl(A, B, Y);
endmodule

(* techmap_celltype = "$shr" *)
module v2f_rule_shr (A, B, Y);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 1;
	parameter B_WIDTH = 1;
	parameter Y_WIDTH = 1;

	input [A_WIDTH-1:0] A;
	input [B_WIDTH-1:0] B;
	output [Y_WIDTH-1:0] Y;

	wire _TECHMAP_FAIL_ = A_WIDTH > 32 || B_WIDTH > 32 || Y_WIDTH > 32;
	v2f_shr impl(A, B, Y);
endmodule

(* techmap_celltype = "$shl" *)

module v2f_rule_ (A, B, Y);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 1;
	parameter B_WIDTH = 1;
	parameter Y_WIDTH = 1;

	input [A_WIDTH-1:0] A;
	input [B_WIDTH-1:0] B;
	output [Y_WIDTH-1:0] Y;

	wire _TECHMAP_FAIL_ = A_WIDTH > 32 || B_WIDTH > 32 || Y_WIDTH > 32;
	v2f_shl impl(A, B, Y);
endmodule

(* techmap_celltype = "$not" *)
module v2f_rule_ (A, Y);
	parameter A_SIGNED = 0;
	parameter A_WIDTH = 1;
	parameter Y_WIDTH = 1;

	input [A_WIDTH-1:0] A;
	output [Y_WIDTH-1:0] Y;

	wire _TECHMAP_FAIL_ = A_WIDTH > 32 || Y_WIDTH > 32;
	v2f_not impl(A, B, Y);
endmodule

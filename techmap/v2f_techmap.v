(* techmap_celltype = "$not" *)
module v2f_rule_ (A, Y);
	parameter A_SIGNED = 0;
	parameter A_WIDTH = 1;
	parameter Y_WIDTH = 1;

	input [A_WIDTH-1:0] A;
	output [Y_WIDTH-1:0] Y;

	wire _TECHMAP_FAIL_ = A_WIDTH > 32 || Y_WIDTH > 32 || A_WIDTH == 1;
	v2f_not #(.A_SIGNED(A_SIGNED), .A_WIDTH(A_WIDTH), .Y_WIDTH(Y_WIDTH))
		_TECHMAP_REPLACE_(.A(A), .Y(Y));
endmodule

(* techmap_celltype = "$reduce_and" *)
module v2f_rule_reduce_and(A, Y);
	parameter A_SIGNED = 0;
	parameter A_WIDTH = 0;
	parameter Y_WIDTH = 0;
	input [A_WIDTH-1:0] A;
	output [Y_WIDTH-1:0] Y;
	wire _TECHMAP_FAIL_ = A_WIDTH > 32;
	v2f_reduce_and #(.A_SIGNED(A_SIGNED), .A_WIDTH(A_WIDTH), .Y_WIDTH(Y_WIDTH))
		_TECHMAP_REPLACE_(.A(A), .Y(Y));
endmodule

(* techmap_celltype = "$reduce_or" *)
module v2f_rule_reduce_or(A, Y);
	parameter A_SIGNED = 0;
	parameter A_WIDTH = 0;
	parameter Y_WIDTH = 0;
	input [A_WIDTH-1:0] A;
	output [Y_WIDTH-1:0] Y;
	wire _TECHMAP_FAIL_ = A_WIDTH > 32;
	v2f_reduce_or #(.A_SIGNED(A_SIGNED), .A_WIDTH(A_WIDTH), .Y_WIDTH(Y_WIDTH))
		_TECHMAP_REPLACE_(.A(A), .Y(Y));
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
	v2f_add #(.A_SIGNED(A_SIGNED), .A_WIDTH(A_WIDTH), .B_SIGNED(B_SIGNED), .B_WIDTH(B_WIDTH), .Y_WIDTH(Y_WIDTH))
		_TECHMAP_REPLACE_(.A(A), .B(B), .Y(Y));
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
	v2f_sub  #(.A_SIGNED(A_SIGNED), .A_WIDTH(A_WIDTH), .B_SIGNED(B_SIGNED), .B_WIDTH(B_WIDTH), .Y_WIDTH(Y_WIDTH))
		_TECHMAP_REPLACE_(.A(A), .B(B), .Y(Y));
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
	v2f_mul #(.A_SIGNED(A_SIGNED), .A_WIDTH(A_WIDTH), .B_SIGNED(B_SIGNED), .B_WIDTH(B_WIDTH), .Y_WIDTH(Y_WIDTH))
		_TECHMAP_REPLACE_(.A(A), .B(B), .Y(Y));
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
	v2f_div #(.A_SIGNED(A_SIGNED), .A_WIDTH(A_WIDTH), .B_SIGNED(B_SIGNED), .B_WIDTH(B_WIDTH), .Y_WIDTH(Y_WIDTH))
		_TECHMAP_REPLACE_(.A(A), .B(B), .Y(Y));
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
	v2f_mod #(.A_SIGNED(A_SIGNED), .A_WIDTH(A_WIDTH), .B_SIGNED(B_SIGNED), .B_WIDTH(B_WIDTH), .Y_WIDTH(Y_WIDTH))
		_TECHMAP_REPLACE_(.A(A), .B(B), .Y(Y));
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

	wire _TECHMAP_FAIL_ = A_WIDTH > 32 || B_WIDTH > 32 || Y_WIDTH > 32 || A_WIDTH == 1 || B_WIDTH == 1;
	v2f_and #(.A_SIGNED(A_SIGNED), .A_WIDTH(A_WIDTH), .B_SIGNED(B_SIGNED), .B_WIDTH(B_WIDTH), .Y_WIDTH(Y_WIDTH))
		_TECHMAP_REPLACE_(.A(A), .B(B), .Y(Y));
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

	wire _TECHMAP_FAIL_ = A_WIDTH > 32 || B_WIDTH > 32 || Y_WIDTH > 32 || A_WIDTH == 1 || B_WIDTH == 1;
	v2f_or #(.A_SIGNED(A_SIGNED), .A_WIDTH(A_WIDTH), .B_SIGNED(B_SIGNED), .B_WIDTH(B_WIDTH), .Y_WIDTH(Y_WIDTH))
		_TECHMAP_REPLACE_(.A(A), .B(B), .Y(Y));
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

	wire _TECHMAP_FAIL_ = A_WIDTH > 32 || B_WIDTH > 32 || Y_WIDTH > 32 || A_WIDTH == 1 || B_WIDTH == 1;
	v2f_xor #(.A_SIGNED(A_SIGNED), .A_WIDTH(A_WIDTH), .B_SIGNED(B_SIGNED), .B_WIDTH(B_WIDTH), .Y_WIDTH(Y_WIDTH))
		_TECHMAP_REPLACE_(A, B, Y);
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
	v2f_shr #(.A_SIGNED(A_SIGNED), .A_WIDTH(A_WIDTH), .B_SIGNED(B_SIGNED), .B_WIDTH(B_WIDTH), .Y_WIDTH(Y_WIDTH))
		_TECHMAP_REPLACE_(.A(A), .B(B), .Y(Y));
endmodule

(* techmap_celltype = "$shl" *)
module v2f_rule_shl (A, B, Y);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 1;
	parameter B_WIDTH = 1;
	parameter Y_WIDTH = 1;

	input [A_WIDTH-1:0] A;
	input [B_WIDTH-1:0] B;
	output [Y_WIDTH-1:0] Y;

	wire _TECHMAP_FAIL_ = A_WIDTH > 32 || B_WIDTH > 32 || Y_WIDTH > 32;
	v2f_shl #(.A_SIGNED(A_SIGNED), .A_WIDTH(A_WIDTH), .B_SIGNED(B_SIGNED), .B_WIDTH(B_WIDTH), .Y_WIDTH(Y_WIDTH))
		_TECHMAP_REPLACE_(.A(A), .B(B), .Y(Y));
endmodule

(* techmap_celltype = "$gt" *)
module v2f_rule_gt (A, B, Y);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 1;
	parameter B_WIDTH = 1;
	parameter Y_WIDTH = 1;

	input [A_WIDTH-1:0] A;
	input [B_WIDTH-1:0] B;
	output [Y_WIDTH-1:0] Y;

	wire _TECHMAP_FAIL_ = A_WIDTH > 32 || B_WIDTH > 32 || A_SIGNED != B_SIGNED;
	v2f_gt #(.A_SIGNED(A_SIGNED), .A_WIDTH(A_WIDTH), .B_SIGNED(B_SIGNED), .B_WIDTH(B_WIDTH), .Y_WIDTH(Y_WIDTH))
		_TECHMAP_REPLACE_(.A(A), .B(B), .Y(Y));
endmodule

(* techmap_celltype = "$lt" *)
module v2f_rule_lt (A, B, Y);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 1;
	parameter B_WIDTH = 1;
	parameter Y_WIDTH = 1;

	input [A_WIDTH-1:0] A;
	input [B_WIDTH-1:0] B;
	output [Y_WIDTH-1:0] Y;

	wire _TECHMAP_FAIL_ = A_WIDTH > 32 || B_WIDTH > 32 || A_SIGNED != B_SIGNED;
	v2f_lt #(.A_SIGNED(A_SIGNED), .A_WIDTH(A_WIDTH), .B_SIGNED(B_SIGNED), .B_WIDTH(B_WIDTH), .Y_WIDTH(Y_WIDTH))
		_TECHMAP_REPLACE_(.A(A), .B(B), .Y(Y));
endmodule

(* techmap_celltype = "$ge" *)
module v2f_rule_lt (A, B, Y);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 1;
	parameter B_WIDTH = 1;
	parameter Y_WIDTH = 1;

	input [A_WIDTH-1:0] A;
	input [B_WIDTH-1:0] B;
	output [Y_WIDTH-1:0] Y;

	wire _TECHMAP_FAIL_ = A_WIDTH > 32 || B_WIDTH > 32 || A_SIGNED != B_SIGNED;
	v2f_ge #(.A_SIGNED(A_SIGNED), .A_WIDTH(A_WIDTH), .B_SIGNED(B_SIGNED), .B_WIDTH(B_WIDTH), .Y_WIDTH(Y_WIDTH))
		_TECHMAP_REPLACE_(.A(A), .B(B), .Y(Y));
endmodule

(* techmap_celltype = "$le" *)
module v2f_rule_le (A, B, Y);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 1;
	parameter B_WIDTH = 1;
	parameter Y_WIDTH = 1;

	input [A_WIDTH-1:0] A;
	input [B_WIDTH-1:0] B;
	output [Y_WIDTH-1:0] Y;

	wire _TECHMAP_FAIL_ = A_WIDTH > 32 || B_WIDTH > 32 || A_SIGNED != B_SIGNED;
	v2f_le #(.A_SIGNED(A_SIGNED), .A_WIDTH(A_WIDTH), .B_SIGNED(B_SIGNED), .B_WIDTH(B_WIDTH), .Y_WIDTH(Y_WIDTH))
		_TECHMAP_REPLACE_(.A(A), .B(B), .Y(Y));
endmodule

(* techmap_celltype = "$ne" *)
module v2f_rule_ne (A, B, Y);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 1;
	parameter B_WIDTH = 1;
	parameter Y_WIDTH = 1;

	input [A_WIDTH-1:0] A;
	input [B_WIDTH-1:0] B;
	output [Y_WIDTH-1:0] Y;

	wire _TECHMAP_FAIL_ = A_WIDTH > 32 || B_WIDTH > 32;
	v2f_ne #(.A_SIGNED(A_SIGNED), .A_WIDTH(A_WIDTH), .B_SIGNED(B_SIGNED), .B_WIDTH(B_WIDTH), .Y_WIDTH(Y_WIDTH))
		_TECHMAP_REPLACE_(.A(A), .B(B), .Y(Y));
endmodule

(* techmap_celltype = "$eq" *)
module v2f_rule_eq (A, B, Y);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 1;
	parameter B_WIDTH = 1;
	parameter Y_WIDTH = 1;

	input [A_WIDTH-1:0] A;
	input [B_WIDTH-1:0] B;
	output [Y_WIDTH-1:0] Y;

	wire _TECHMAP_FAIL_ = A_WIDTH > 32 || B_WIDTH > 32;
	v2f_eq #(.A_SIGNED(A_SIGNED), .A_WIDTH(A_WIDTH), .B_SIGNED(B_SIGNED), .B_WIDTH(B_WIDTH), .Y_WIDTH(Y_WIDTH))
		_TECHMAP_REPLACE_(.A(A), .B(B), .Y(Y));
endmodule

(* techmap_celltype = "$pmux"*)
module v2f_rule_pmux (A, B, S, Y);
	parameter S_WIDTH = 0;
	parameter WIDTH = 0;

	input [WIDTH-1:0] A;
	input [WIDTH*S_WIDTH*-1:0] B;
	input [S_WIDTH-1:0] S;
	output [WIDTH-1:0] Y;
	wire _TECHMAP_FAIL_ = WIDTH > 32;
	v2f_pmux #(.WIDTH(WIDTH), .S_WIDTH(S_WIDTH))
		_TECHMAP_REPLACE_(.A(A), .B(B), .S(S), .Y(Y));
	// Blackbox
endmodule
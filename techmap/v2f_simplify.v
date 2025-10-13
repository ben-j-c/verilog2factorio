
(* techmap_celltype = "$mul" *)
module v2f_rule_32x32_64_mul_narrowing (A, B, Y);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 32;
	parameter B_WIDTH = 32;
	parameter Y_WIDTH = 64;

	input [A_WIDTH-1:0] A;
	input [B_WIDTH-1:0] B;
	output [Y_WIDTH-1:0] Y;

	wire _TECHMAP_FAIL_ = A_WIDTH != 32 || B_WIDTH != 32 || Y_WIDTH != 64;

	wire [31:0] LL;
	wire [31:0] HL;
	wire [31:0] LH;
	wire [31:0] HH;
	assign LL = A[15:0] * B[15:0];
	assign HL = A[31:16] * B[15:0];
	assign LH = A[15:0] * B[31:16];
	assign HH = A[31:16] * B[31:16];

	assign Y[15:0] = LL[15:0];
	wire [15:0] c1;
	assign {c1, Y[31:16]} = {16'b0, LL[31:16]} + {16'b0, HL[15:0]} + {16'b0, LH[15:0]};
	wire [15:0] c2;
	assign {c2, Y[47:32]} = {16'b0, HH[15:0]} + {16'b0, HL[31:16]} + {16'b0, LH[31:16]} + {16'b0, c1};
	assign Y[63:48] = HH[31:16] + c2;
	assign Y[63:48] = HH[31:16] + c2;
endmodule


(* techmap_celltype = "$mul" *)
module v2f_rule_64x64_64_mul_narrowing (A, B, Y);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 64;
	parameter B_WIDTH = 64;
	parameter Y_WIDTH = 64;

	input [A_WIDTH-1:0] A;
	input [B_WIDTH-1:0] B;
	output [Y_WIDTH-1:0] Y;

	wire _TECHMAP_FAIL_ = A_WIDTH != 64 || B_WIDTH != 64 || Y_WIDTH != 64;

	wire [31:0] prod [15:0];

	generate
		genvar i;
		genvar j;
		for (i = 0; i < 4; i = i + 1) begin
			for (j = 0; j < 4; j = j + 1) begin
				assign prod[i*4 + j] = A[16*(i+1) - 1:16*i] * B[16*(j+1) - 1:16*j];
			end
		end
	endgenerate

	wire [31:0] sum0;
	wire [31:0] sum1;
	wire [31:0] sum2;
	wire [31:0] sum3;

	assign sum0 = prod[0];
	assign sum1 = prod[1] + prod[4] + sum0[31:0];
	assign sum2 = prod[2] + prod[5] + prod[8] + sum1[31:0];
	assign sum3 = prod[3] + prod[6] + prod[9] + prod[12] + sum2[31:0];

	assign Y[15:00] = sum0[15:0];
	assign Y[31:16] = sum1[15:0];
	assign Y[47:32] = sum2[15:0];
	assign Y[63:48] = sum3[15:0];
endmodule

(* techmap_celltype = "$lt" *)
module v2f_rule_lt_unsigned_to_signed(A, B, Y);
parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 32;
	parameter B_WIDTH = 32;
	parameter Y_WIDTH = 32;

	input [31:0] A;
	input [31:0] B;
	output [Y_WIDTH-1:0] Y;

	wire [31:0] A_pr;
	wire [31:0] B_pr;

	assign A_pr = A + 32'h80000000;
	assign B_pr = B + 32'h80000000;

	wire _TECHMAP_FAIL_ = A_SIGNED || B_SIGNED || A_WIDTH != 32 || B_WIDTH != 32;
	$lt #(.A_SIGNED(1), .A_WIDTH(32), .B_SIGNED(1), .B_WIDTH(32), .Y_WIDTH(Y_WIDTH))
		_TECHMAP_REPLACE_(.A(A_pr), .B(B_pr), .Y(Y));
endmodule

(* techmap_celltype = "$gt" *)
module v2f_rule_lt_unsigned_to_signed(A, B, Y);
parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 32;
	parameter B_WIDTH = 32;
	parameter Y_WIDTH = 32;

	input [31:0] A;
	input [31:0] B;
	output [Y_WIDTH-1:0] Y;

	wire [31:0] A_pr;
	wire [31:0] B_pr;

	assign A_pr = A + 32'h80000000;
	assign B_pr = B + 32'h80000000;

	wire _TECHMAP_FAIL_ = A_SIGNED || B_SIGNED || A_WIDTH != 32 || B_WIDTH != 32;
	$gt #(.A_SIGNED(1), .A_WIDTH(32), .B_SIGNED(1), .B_WIDTH(32), .Y_WIDTH(Y_WIDTH))
		_TECHMAP_REPLACE_(.A(A_pr), .B(B_pr), .Y(Y));
endmodule

(* techmap_celltype = "$le" *)
module v2f_rule_lt_unsigned_to_signed(A, B, Y);
parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 32;
	parameter B_WIDTH = 32;
	parameter Y_WIDTH = 32;

	input [31:0] A;
	input [31:0] B;
	output [Y_WIDTH-1:0] Y;

	wire [31:0] A_pr;
	wire [31:0] B_pr;

	assign A_pr = A + 32'h80000000;
	assign B_pr = B + 32'h80000000;

	wire _TECHMAP_FAIL_ = A_SIGNED || B_SIGNED || A_WIDTH != 32 || B_WIDTH != 32;
	$le #(.A_SIGNED(1), .A_WIDTH(32), .B_SIGNED(1), .B_WIDTH(32), .Y_WIDTH(Y_WIDTH))
		_TECHMAP_REPLACE_(.A(A_pr), .B(B_pr), .Y(Y));
endmodule

(* techmap_celltype = "$ge" *)
module v2f_rule_lt_unsigned_to_signed(A, B, Y);
parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 32;
	parameter B_WIDTH = 32;
	parameter Y_WIDTH = 32;

	input [31:0] A;
	input [31:0] B;
	output [Y_WIDTH-1:0] Y;

	wire [31:0] A_pr;
	wire [31:0] B_pr;

	assign A_pr = A + 32'h80000000;
	assign B_pr = B + 32'h80000000;

	wire _TECHMAP_FAIL_ = A_SIGNED || B_SIGNED || A_WIDTH != 32 || B_WIDTH != 32;
	$ge #(.A_SIGNED(1), .A_WIDTH(32), .B_SIGNED(1), .B_WIDTH(32), .Y_WIDTH(Y_WIDTH))
		_TECHMAP_REPLACE_(.A(A_pr), .B(B_pr), .Y(Y));
endmodule
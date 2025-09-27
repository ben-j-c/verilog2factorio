
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

	integer i;
	integer j;
	
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


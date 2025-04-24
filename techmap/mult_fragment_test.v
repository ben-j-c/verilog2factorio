(* techmap_celltype = "$mul" *)
module _mul_fragment (A, B, Y);
	parameter A_SIGNED = 0;
	parameter B_SIGNED = 0;
	parameter A_WIDTH = 1;
	parameter B_WIDTH = 1;
	parameter Y_WIDTH = 1;

	input [A_WIDTH-1:0] A;
	input [B_WIDTH-1:0] B;
	output [Y_WIDTH-1:0] Y;

	wire _TECHMAP_FAIL_ = A_WIDTH != 32 && B_WIDTH != 32 && Y_WIDTH != 64;

	integer i;
	integer j;
	
	wire [31:0] tmp0;
	wire [31:0] tmp1;
	wire [31:0] tmp2;
	wire [31:0] tmp3;
	assign tmp0 = A[15:0] * B[15:0];
	assign tmp1 = A[31:16] * B[15:0];
	assign tmp2 = A[15:0] * B[31:16];
	assign tmp3 = A[31:16] * B[31:16];

	wire c;
	assign Y[31:0] = tmp0 + {tmp1[15:0], 16'b0} + {tmp2[15:0], 16'b0};

endmodule
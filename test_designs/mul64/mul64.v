module mul64(
	input [31:0] A1,
	input [31:0] B1,
	input [31:0] A2,
	input [31:0] B2,
	output [31:0] Y1,
	output [31:0] Y2
);
	wire [63:0] Y = {A2, A1} * {B2, B1};
	assign Y1 = Y[31:0];
	assign Y2 = Y[63:32];

endmodule
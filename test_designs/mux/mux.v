module mux(
	input [31:0] A,
	input [31:0] B,
	input [31:0] C,
	input [1:0] S,
	output reg [31:0] Y
);
	always @* begin
		Y = S[0]? (|A) : (S[1]? (|B) : (&C));
	end
endmodule
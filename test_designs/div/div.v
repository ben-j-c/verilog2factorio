module div(
	input [31:0] A,
	input [31:0] B,
	input [1:0] S,
	output reg [31:0] Y
);
	always @* begin
		if (S[0]) begin
			if (S[1]) begin
				Y <= $signed(A) / $signed(B);
			end else begin
				Y <= $unsigned(A) / $unsigned(B);
			end
		end else begin
			if (S[1]) begin
				Y <= $signed(A) % $signed(B);
			end else begin
				Y <= $unsigned(A) % $unsigned(B);
			end
		end
	end
endmodule
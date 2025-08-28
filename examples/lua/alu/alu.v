module alu(
	input signed [31:0] data_a,
	input signed [31:0] data_b,
	input [31:0] select,
	output signed [31:0] result_y,
);
	wire signed [31:0] A;
	wire signed [31:0] B;
	reg signed [31:0] Y;
	assign result_y = Y;
	assign A = data_a;
	assign B = data_b;
	always @(*) begin
		case (select[3:0])
			4'b0000: begin
				Y <= A + B;
			end
			4'b0001: begin
				Y <= A - B;
			end
			4'b0010: begin
				Y <= A * B;
			end
			4'b0011: begin
				Y <= A / B;
			end
			4'b0100: begin
				Y <= A % B;
			end
			4'b0101: begin
				Y <= B;
			end
			4'b0110: begin
				Y <= A ** B;
			end
			4'b0111: begin
				Y <= -A;
			end
			4'b1000: begin
				Y <= A | B;
			end
			4'b1001: begin
				Y <= A & B;
			end
			4'b1010: begin
				Y <= A ^ B;
			end
			4'b1011: begin
				Y <= A > B;
			end
			4'b1100: begin
				Y <= A == B;
			end
			4'b1101: begin
				Y <= A << B;
			end
			4'b1110: begin
				Y <= A >> B;
			end
			4'b1111: begin
				Y <= A;
			end
		endcase
	end
endmodule
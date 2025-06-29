module alu(
	input signed [31:0] signal_A,
	input signed [31:0] signal_B,
	input [31:0] signal_S_op_select,
	output signed [31:0] signal_Y,
);
	wire signed [31:0] A;
	wire signed [31:0] B;
	reg signed [31:0] Y;
	assign signal_Y = Y;
	assign A = signal_A;
	assign B = signal_B;
	always @(*) begin
		case (signal_S_op_select[3:0])
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
				Y <= A % B;
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
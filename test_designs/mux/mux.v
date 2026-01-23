module mux(
	input clk,
	input arst,
	input [31:0] A,
	input [31:0] B,
	input [31:0] C,
	input [1:0] S,
	output reg [31:0] Y
	
);
	reg [31:0] A_pr;
	(*keep*)reg [31:0] B_pr;
	reg [31:0] C_pr;
	reg [1:0] S_pr;
	always @(posedge clk or posedge arst) begin
		if (arst) begin
			Y <= 0;
			A_pr <= 0;
			B_pr <= 0;
			C_pr <= 0;
			S_pr <= 0;
		end else begin
			A_pr <= A;
			B_pr <= B;
			C_pr <= C;
			S_pr <= S;
			Y = S_pr[0]? (|{A_pr[31:6], B_pr[5:0]}) : (S_pr[1]? (|B_pr[5:0]) : (&C_pr[3:0]));
		end
	end
endmodule
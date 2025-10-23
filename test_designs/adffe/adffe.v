

module adffe(
	input clk_i,
	input rst_i,
	input en_i,
	input [31:0] data_i,
	output reg [31:0] q_o
);
	always @(posedge clk_i or posedge rst_i) begin
		if (rst_i) begin
			q_o <= 0;
		end else if (en_i) begin
			q_o <= data_i;
		end
	end
endmodule
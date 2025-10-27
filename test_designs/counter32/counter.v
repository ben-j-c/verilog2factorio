

module counter(
	input clk_i,
	input rst_i,
	input en_i,
	output reg [31:0] q_o
);
	always @(posedge clk_i or posedge rst_i) begin
		if (rst_i) begin
			q_o <= 0;
		end else if (en_i) begin
			q_o <= q_o + 32'd1;
		end
	end
endmodule
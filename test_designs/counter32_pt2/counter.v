

module counter(
	input clk_i,
	input rst_i,
	input en_i,
	output reg [31:0] q_o
);
	reg [31:0] q_internal;
	always @(posedge clk_i or posedge rst_i) begin
		if (rst_i) begin
			q_internal <= 0;
			q_o <= 0;
		end else if (en_i) begin
			q_internal <= q_internal + 32'd1;
			q_o <= {q_internal[31:2],2'b0};
		end
	end
endmodule
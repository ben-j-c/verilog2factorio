module memories(
	input clk,
	input arst,
	input [31:0] rd_addr,
	input rd_en,
	input [31:0] wr_addr,
	input [31:0] wr_data,
	input [3:0] wr_bsel,
	input wr_en,

	output reg [31:0] rd_data,
	output reg rd_valid,

	output reg wr_ack
	);

	reg [31:0] data[255:0];
	wire [31:0] rd_data_internal = data[rd_addr];

	always @(posedge clk or posedge arst) begin
		if (arst) begin
			rd_valid <= 0;
			rd_data <= 0;
			wr_ack <= 0;
		end else if(clk) begin
			rd_valid <= 0;
			if (rd_en) begin
				rd_data <= rd_data_internal;
				rd_valid <= 1'b1;
			end
			wr_ack <= 0;
			if (wr_en) begin
				wr_ack <= 1'b1;
			end
		end
	end

	always @(posedge clk) begin
		if (wr_en && !arst) begin
			if (wr_bsel[0]) data[wr_addr][7:0]   <= wr_data[7:0];
			if (wr_bsel[1]) data[wr_addr][15:8]  <= wr_data[15:8];
			if (wr_bsel[2]) data[wr_addr][23:16] <= wr_data[23:16];
			if (wr_bsel[3]) data[wr_addr][31:24] <= wr_data[31:24];
		end
	end
endmodule
`include "memories.v"

module tb;
	reg clk;
	reg arst;
	reg [31:0] rd_addr;
	reg rd_en;
	reg [31:0] wr_addr;
	reg [31:0] wr_data;
	reg [3:0] wr_bsel;
	reg wr_en;
	wire [31:0] rd_data;
	wire rd_valid;
	wire wr_ack;

	memories dut (
		.clk(clk),
		.arst(arst),
		.rd_addr(rd_addr),
		.rd_en(rd_en),
		.wr_addr(wr_addr),
		.wr_data(wr_data),
		.wr_bsel(wr_bsel),
		.wr_en(wr_en),
		.rd_data(rd_data),
		.rd_valid(rd_valid),
		.wr_ack(wr_ack)
	);
	integer seed;
	integer i;
	initial begin
		seed = 123;
		$dumpfile("memories_tb.vcd");
		$dumpvars(0, tb);
		clk = 0;
		arst = 1;
		rd_addr = 0;
		rd_en = 0;
		wr_addr = 0;
		wr_data = 0;
		wr_bsel = 0;
		wr_en = 0;

		#1;
		arst = 0;

		for (i = 0; i < 1000; i+= 1) begin
			rd_addr = $random(seed);
			rd_en = $random(seed);
			wr_addr = $random(seed);
			wr_data = $random(seed);
			wr_bsel = $random(seed);
			wr_en = $random(seed);
			clk = 0;
			#1;
			clk = 1;
			#1;
		end
	end

endmodule
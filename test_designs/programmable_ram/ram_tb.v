`include "ram.v"

module tb;
	wire [31:0] RD_DATA;
	reg [31:0] RD_ADDR;
	reg WR_CLK;
	reg ARST;
	reg [31:0] WR_ADDR;
	reg [31:0] WR_DATA;
	reg WR_EN;
	reg [3:0] BYTE_SELECT;

	ram dut(
		.RD_ADDR(RD_ADDR[2:0]),
		.RD_DATA(RD_DATA),
		.WR_CLK(WR_CLK),
		.ARST(ARST),
		.WR_ADDR(WR_ADDR[2:0]),
		.WR_DATA(WR_DATA),
		.WR_EN(WR_EN),
		.BYTE_SELECT(BYTE_SELECT)
	);

	integer seed;
	integer i, j, k;
	initial begin
		seed = 123;
		$dumpfile("ram_tb.vcd");
		$dumpvars(0, tb);
		RD_ADDR = 0;
		WR_CLK = 0;
		ARST = 1;
		WR_ADDR = 0;
		WR_DATA = 0;
		WR_EN = 0;
		BYTE_SELECT = 0;

		#1;
		ARST = 0;
		#1;

		for (i = 0; i < 8; i+= 1) begin
			RD_ADDR = i;
			#1;
		end

		for (i = 0; i < 16; i+= 1) begin
			BYTE_SELECT = i;
			for (j = 0; j < 16; j+= 1) begin
				WR_ADDR = j & 3'b111;
				WR_EN = j > 7;
				WR_DATA = 32'h01010101 * (j + i * 16);
				#1;
				WR_CLK = 1;
				#1;
				for (k = 0; k < 8; k+= 1) begin
					RD_ADDR = k;
					#1;
				end
				#1;
				WR_CLK = 0;
			end
		end
	end

endmodule
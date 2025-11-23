(*whitebox*)
module v2f_programmable_ram(
		RD_CLK,
		RD_EN,
		RD_ARST,
		RD_SRST,
		RD_ADDR,
		RD_DATA,
		WR_CLK,
		WR_EN,
		WR_ADDR,
		WR_DATA,
		input [3:0] BYTE_SELECT,
		input ARST
	);
	parameter MEMID = "";
	parameter signed SIZE = 4;
	parameter signed OFFSET = 0;
	parameter signed ABITS = 2;
	localparam signed WIDTH = 32;
	localparam signed BYTE_WIDTH = 8;
	localparam signed INIT = 32'b0;

	parameter signed RD_PORTS = 1;
	parameter RD_CLK_ENABLE = 1'b1;
	parameter RD_CLK_POLARITY = 1'b1;
	parameter RD_TRANSPARENCY_MASK = 1'b0;
	parameter RD_COLLISION_X_MASK = 1'b0;
	parameter RD_WIDE_CONTINUATION = 1'b0;
	parameter RD_CE_OVER_SRST = 1'b0;
	localparam RD_ARST_VALUE = 1'b0;
	localparam RD_SRST_VALUE = 1'b0;
	localparam RD_INIT_VALUE = 1'b0;

	localparam signed WR_PORTS = 1;
	localparam WR_CLK_ENABLE = 1'b1;
	localparam WR_CLK_POLARITY = 1'b1;
	parameter WR_PRIORITY_MASK = 1'b0;
	parameter WR_WIDE_CONTINUATION = 1'b0;

	input [RD_PORTS-1:0] RD_CLK;
	input [RD_PORTS-1:0] RD_EN;
	input [RD_PORTS-1:0] RD_ARST;
	input [RD_PORTS-1:0] RD_SRST;
	input [RD_PORTS*ABITS-1:0] RD_ADDR;
	output reg [RD_PORTS*WIDTH-1:0] RD_DATA;

	input [WR_PORTS-1:0] WR_CLK;
	input [WR_PORTS*WIDTH-1:0] WR_EN;
	input [WR_PORTS*ABITS-1:0] WR_ADDR;
	input [WR_PORTS*WIDTH-1:0] WR_DATA;

	reg [WIDTH-1:0] memory [SIZE-1:0];

	integer i, j, k;
	reg [WR_PORTS-1:0] LAST_WR_CLK;
	reg [RD_PORTS-1:0] LAST_RD_CLK;

	function port_active;
		input clk_enable;
		input clk_polarity;
		input last_clk;
		input this_clk;
		begin
			casez ({clk_enable, clk_polarity, last_clk, this_clk})
				4'b0???: port_active = 1;
				4'b1101: port_active = 1;
				4'b1010: port_active = 1;
				default: port_active = 0;
			endcase

		end

	endfunction


	initial begin
		for (i = 0; i < SIZE; i = i+1)
			memory[i] = INIT >>> (i*WIDTH);
		RD_DATA = RD_INIT_VALUE;
	end


	always @(RD_CLK, RD_ARST, RD_ADDR, RD_DATA, WR_CLK, WR_EN, WR_ADDR, WR_DATA, ARST) begin
	`ifdef SIMLIB_MEMDELAY
		#`SIMLIB_MEMDELAY;
	`endif

		for (i = 0; i < RD_PORTS; i = i+1) begin
			if (RD_CLK_ENABLE[i] && RD_EN[i] && port_active(RD_CLK_ENABLE[i], RD_CLK_POLARITY[i], LAST_RD_CLK[i], RD_CLK[i])) begin
				// $display("Read from %s: addr=%b data=%b", MEMID, RD_ADDR[i*ABITS +: ABITS],  memory[RD_ADDR[i*ABITS +: ABITS] - OFFSET]);
				RD_DATA[i*WIDTH +: WIDTH] <= memory[RD_ADDR[i*ABITS +: ABITS] - OFFSET];
				for (j = 0; j < WR_PORTS; j = j+1) begin
					if (RD_TRANSPARENCY_MASK[i*WR_PORTS + j] && port_active(WR_CLK_ENABLE[j], WR_CLK_POLARITY[j], LAST_WR_CLK[j], WR_CLK[j]) && RD_ADDR[i*ABITS +: ABITS] == WR_ADDR[j*ABITS +: ABITS])
						for (k = 0; k < WIDTH; k = k+1)
							if (WR_EN[j*WIDTH+k])
								RD_DATA[i*WIDTH+k] <= WR_DATA[j*WIDTH+k];
					if (RD_COLLISION_X_MASK[i*WR_PORTS + j] && port_active(WR_CLK_ENABLE[j], WR_CLK_POLARITY[j], LAST_WR_CLK[j], WR_CLK[j]) && RD_ADDR[i*ABITS +: ABITS] == WR_ADDR[j*ABITS +: ABITS])
						for (k = 0; k < WIDTH; k = k+1)
							if (WR_EN[j*WIDTH+k])
								RD_DATA[i*WIDTH+k] <= 1'bx;
				end
			end
		end


		for (i = 0; i < WR_PORTS; i = i+1) begin
			if (port_active(WR_CLK_ENABLE[i], WR_CLK_POLARITY[i], LAST_WR_CLK[i], WR_CLK[i]))
				for (j = 0; j < WIDTH; j = j+1)
					if (WR_EN[i*WIDTH+j]) begin
						if (BYTE_SELECT[j/BYTE_WIDTH]) begin
							memory[WR_ADDR][j] = WR_DATA[i*WIDTH+j];
						end
						// $display("Write to %s: addr=%b data=%b", MEMID, WR_ADDR[i*ABITS +: ABITS], WR_DATA[i*WIDTH+j]);
					end
		end


		for (i = 0; i < RD_PORTS; i = i+1) begin
			if (!RD_CLK_ENABLE[i]) begin
				// $display("Combinatorial read from %s: addr=%b data=%b", MEMID, RD_ADDR[i*ABITS +: ABITS],  memory[RD_ADDR[i*ABITS +: ABITS] - OFFSET]);
				RD_DATA[i*WIDTH +: WIDTH] <= memory[RD_ADDR[i*ABITS +: ABITS] - OFFSET];
			end
		end


		for (i = 0; i < RD_PORTS; i = i+1) begin
			if (RD_SRST[i] && port_active(RD_CLK_ENABLE[i], RD_CLK_POLARITY[i], LAST_RD_CLK[i], RD_CLK[i]) && (RD_EN[i] || !RD_CE_OVER_SRST[i]))
				RD_DATA[i*WIDTH +: WIDTH] <= RD_SRST_VALUE[i*WIDTH +: WIDTH];
			if (RD_ARST[i])
				RD_DATA[i*WIDTH +: WIDTH] <= RD_ARST_VALUE[i*WIDTH +: WIDTH];
		end
		LAST_RD_CLK <= RD_CLK;
		LAST_WR_CLK <= WR_CLK;
	end
endmodule
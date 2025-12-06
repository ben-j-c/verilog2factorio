(* blackbox *)
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
		BYTE_SELECT,
		ARST
	);
	parameter MEMID = "";
	parameter PROGRAM_FILE = "program.mem";
	parameter signed SIZE = 4;
	localparam signed OFFSET = 0;
	parameter signed ABITS = 2;
	localparam signed WIDTH = 32;
	localparam signed BYTE_WIDTH = 8;
	localparam signed INIT = 32'b0;

	parameter signed RD_PORTS = 1;
	parameter RD_CLK_ENABLE = 1'b1;
	parameter RD_CLK_POLARITY = 1'b1;
	localparam RD_TRANSPARENCY_MASK = 0;
	parameter RD_CE_OVER_SRST = 1'b0;

	localparam signed WR_PORTS = 1;
	localparam WR_CLK_ENABLE = 1'b1;
	localparam WR_CLK_POLARITY = 1'b1;

	input [RD_PORTS-1:0] RD_CLK;
	input [RD_PORTS-1:0] RD_EN;
	input [RD_PORTS-1:0] RD_ARST;
	input [RD_PORTS-1:0] RD_SRST;
	input [RD_PORTS*ABITS-1:0] RD_ADDR;
	output reg [RD_PORTS*WIDTH-1:0] RD_DATA;

	input [WR_PORTS-1:0] WR_CLK;
	input [WR_PORTS-1:0] WR_EN;
	input [WR_PORTS*ABITS-1:0] WR_ADDR;
	input [WR_PORTS*WIDTH-1:0] WR_DATA;
	input ARST;
	input [3:0] BYTE_SELECT;

`ifndef YOSYS

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
			memory[i] = 0;
		RD_DATA = 0;
	end


	always @(RD_CLK, RD_ARST, RD_ADDR, RD_DATA, WR_CLK, WR_EN, WR_ADDR, WR_DATA, ARST) begin
	`ifdef SIMLIB_MEMDELAY
		#`SIMLIB_MEMDELAY;
	`endif

		for (i = 0; i < RD_PORTS; i = i+1) begin
			if (RD_CLK_ENABLE[i] && RD_EN[i] && port_active(RD_CLK_ENABLE[i], RD_CLK_POLARITY[i], LAST_RD_CLK[i], RD_CLK[i])) begin
				// $display("Read from %s: addr=%b data=%b", MEMID, RD_ADDR[i*ABITS +: ABITS],  memory[RD_ADDR[i*ABITS +: ABITS] - OFFSET]);
				RD_DATA[i*WIDTH +: WIDTH] <= memory[RD_ADDR[i*ABITS +: ABITS] - OFFSET];
			end
		end


		if (ARST) begin
			$readmemh(PROGRAM_FILE, memory, 0, SIZE-1);
		end
		else begin
			for (i = 0; i < WR_PORTS; i = i+1) begin
				if (port_active(WR_CLK_ENABLE[i], WR_CLK_POLARITY[i], LAST_WR_CLK[i], WR_CLK[i]))
					for (j = 0; j < WIDTH; j = j+1)
						if (WR_EN[i]) begin
							//$display("j=%d", j);
							if (BYTE_SELECT[j/BYTE_WIDTH]) begin
								memory[WR_ADDR][j] = WR_DATA[i*WIDTH+j];
								//$display("Write to %s: addr=%b data=%b, bs=%b, j=%d", MEMID, WR_ADDR[i*ABITS +: ABITS], WR_DATA[i*WIDTH+j], BYTE_SELECT, j);
							end
						end
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
				RD_DATA[i*WIDTH +: WIDTH] <= 0;
			if (RD_ARST[i])
				RD_DATA[i*WIDTH +: WIDTH] <= 0;
		end
		LAST_RD_CLK <= RD_CLK;
		LAST_WR_CLK <= WR_CLK;
	end
`endif
endmodule
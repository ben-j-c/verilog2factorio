`include "../../v2flib/v2f_programmable_ram.v"

module ram(
	input [2:0] RD_ADDR,
	output [31:0] RD_DATA,
	input WR_CLK,
	input ARST,
	input [2:0] WR_ADDR,
	input [31:0] WR_DATA,
	input WR_EN,
	input [3:0] BYTE_SELECT
);
	v2f_programmable_ram #(
			.SIZE(8),
			.ABITS(3),
			.PROGRAM_FILE("program.mem"),
			.ABITS(3),
			.RD_PORTS(1),
			.RD_CLK_ENABLE(0),
			.RD_CLK_POLARITY(1)
		) dut(
		.RD_CLK(1'bx),
		.RD_EN(1'bx),
		.RD_ARST(1'b0),
		.RD_SRST(1'b0),
		.RD_ADDR(RD_ADDR),
		.RD_DATA(RD_DATA),
		.WR_CLK(WR_CLK),
		.WR_EN(WR_EN),
		.WR_ADDR(WR_ADDR),
		.WR_DATA(WR_DATA),
		.BYTE_SELECT(BYTE_SELECT),
		.ARST(ARST)
	);
endmodule
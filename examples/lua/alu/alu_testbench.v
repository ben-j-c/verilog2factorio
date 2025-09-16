`include "alu.v"

module tb;
	reg signed [31:0] data_a;
	reg signed [31:0] data_b;
	reg [31:0] select;
	wire signed [31:0] result_y;

	alu dut (
		.data_a(data_a),
		.data_b(data_b),
		.select(select),
		.result_y(result_y)
	);

	integer i;
	integer op_select;
	integer seed;
	initial begin
		seed = 123;
		$dumpfile("alu_testbench.vcd");
		$dumpvars(1, tb);

		for (i = 0; i < 50; i = i + 1) begin
			data_a = $random(seed);
			data_b = $random(seed);

			for (op_select = 0; op_select < 16; op_select = op_select + 1) begin
				select = op_select;
				#1;
			end
		end
		$finish;
	end
endmodule
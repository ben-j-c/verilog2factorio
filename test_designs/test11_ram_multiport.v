module top(
	input [31:0] signal_1_1,
	input [31:0] signal_1_2,
	output reg [31:0] signal_2_1,
	output reg [31:0] signal_2_2,
	input signal_C,
	input signal_E,
	input [31:0] signal_D,
	input [31:0] signal_A,
	);

	reg [31:0] memory [255:0];
	always @(*) begin
		signal_2_1 = memory[signal_1_1[7:0]];
		signal_2_2 = memory[signal_1_2[7:0]];
	end

	always @(posedge signal_C) begin
		if (signal_E) begin
			memory[signal_A[7:0]] <= signal_D;
		end
	end
endmodule
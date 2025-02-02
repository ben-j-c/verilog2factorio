module top(
	input [31:0] signal_1_1,
	input [31:0] signal_1_2,
	output reg [31:0] signal_2_1,
	output reg [31:0] signal_2_2,
	);

	(* speed *) reg [31:0] memory [255:0];
	initial $readmemh("test8_memory_hex.txt", memory);
	always @(*) begin
		signal_2_1 = memory[signal_1_1[7:0]];
		signal_2_2 = memory[signal_1_2[7:0]];
	end
endmodule
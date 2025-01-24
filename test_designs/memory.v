module top(
	input [31:0] signal_1,
	output reg [31:0] signal_2
	);

	reg [31:0] memory [255:0];
	initial $readmemh("test8_memory_hex.txt", memory);
	always @(*) begin
		signal_2 = memory[signal_1[7:0]];
	end
endmodule
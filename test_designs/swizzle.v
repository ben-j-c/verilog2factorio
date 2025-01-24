module top(
	input [31:0] signal_0,
	output reg [31:0] signal_1
	);
	always @(*) begin
		signal_1 = {signal_0[7:0], 8'hF0, signal_0[16:8], 4'hF, signal_0[30:27]};
	end
endmodule
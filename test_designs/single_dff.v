module top(
	input [31:0] signal_0_in,
	input [31:0] signal_red,
	output reg [31:0] signal_0_out
	);
	always @(posedge signal_red[0]) begin
		signal_0_out <= signal_0_in;
	end
endmodule
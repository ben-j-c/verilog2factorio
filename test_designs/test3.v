module top(
	input signed [31:0] signal_0,
	input signed [31:0] signal_1,
	output reg [31:0] signal_2,
	output reg [31:0] signal_4,
);
	always @(*) begin
		signal_2 = signal_0*signal_1;
		signal_4 = signal_2*signal_1;
	end
endmodule
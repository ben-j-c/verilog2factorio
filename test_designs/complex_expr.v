module top(
	input signed [31:0] signal_0,
	input signed [31:0] signal_1,
	input signed [31:0] signal_2,
	input signed [31:0] signal_3,
	output reg [31:0] signal_4,
	output reg [31:0] signal_5,
	output reg [31:0] signal_6
);
	wire signed [31:0] tmp;
	always @(*) begin
		tmp = (signal_0 << signal_2) + signal_1;
		signal_4 = tmp;
		signal_5 = signal_1/signal_2 + signal_1;
		signal_6 = (signal_0 + signal_1 + (signal_2 * signal_3))/signal_4 + signal_1;
	end
endmodule
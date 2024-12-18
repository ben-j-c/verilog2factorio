module test2(
	input [31:0] signal_0,
	input [31:0] signal_1,
	output reg [31:0] signal_2
);
	always @(*) begin
		signal_2 = 32'b1001001;
	end
endmodule
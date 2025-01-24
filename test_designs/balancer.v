module top(
	input signed [31:0] iron_plate_0,
	input signed [31:0] iron_plate_1,
	output reg [31:0] signal_A,
	output reg [31:0] signal_B,
);
	wire signed [31:0] tmp1;
	wire signed [31:0] tmp2;
	v2f_rolling_accumulate iron0(iron_plate_0, tmp1);
	v2f_rolling_accumulate iron1(iron_plate_1, tmp2);
	always @(*) begin
		signal_A = tmp1 > tmp2 + 10;
		signal_B = tmp2 > tmp1 + 10;
	end
endmodule
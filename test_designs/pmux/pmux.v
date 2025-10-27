

module pmux(
	input [2:0]     sel_i,
	input [15:0] data_0_i,
	input [15:0] data_1_i,
	input [15:0] data_2_i,
	input [15:0] data_3_i,
	input [15:0] data_4_i,
	input [15:0] data_5_i,
	input [15:0] data_6_i,
	input [15:0] data_7_i,
	output reg [15:0]   q_o
);
	always @(*) begin
		case (sel_i)
			3'b000: q_o = data_0_i;
			3'b001: q_o = data_1_i;
			3'b010: q_o = data_2_i;
			3'b011: q_o = data_3_i;
			3'b100: q_o = data_4_i;
			3'b101: q_o = data_5_i;
			3'b110: q_o = data_6_i;
			3'b111: q_o = data_7_i;
		endcase
	end
endmodule
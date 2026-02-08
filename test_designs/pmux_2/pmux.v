

module pmux(
	input [31:0] sel_0_i,
	input [31:0] sel_1_i,
	input [31:0] sel_2_i,
	input [31:0] sel_3_i,
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
		if (sel_0_i == 0) begin
			q_o <= data_0_i + data_1_i;
		end else if (sel_1_i == 10) begin
			q_o <= data_0_i;
		end else if (sel_2_i == 21) begin
			q_o <= data_1_i;
		end else if (sel_3_i == 77) begin
			q_o <= data_2_i;
		end else begin
			if (sel_0_i == 50) begin
				q_o <= data_3_i;
			end else if (sel_1_i == 60) begin
				q_o <= data_4_i;
			end else if (sel_2_i == 70) begin
				q_o <= data_5_i;
			end else if (sel_3_i == 80) begin
				q_o <= data_6_i;
			end else begin
				q_o <= data_7_i;
			end
		end
	end
endmodule
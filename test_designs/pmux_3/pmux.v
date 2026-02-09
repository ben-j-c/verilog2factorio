

module pmux(
	input [2:0] sel_0_i,
	input [2:0] sel_1_i,
	input [2:0] sel_2_i,
	input [2:0] sel_3_i,
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
		case (sel_0_i[1:0])
			0: q_o <= data_0_i + 1;
			1: q_o <= data_1_i + 2;
			2: q_o <= data_2_i + 3;
			default: begin 
				case (sel_1_i[1:0])
					0: q_o <= data_0_i + 8;
					1: q_o <= data_1_i + 9;
					2: q_o <= data_2_i + 10;
					default: begin
						case (sel_2_i[1:0])
							0: q_o <= data_0_i + 80;
							1: q_o <= data_1_i + 90;
							2: q_o <= data_2_i + 100;
							default: begin
								case (sel_2_i[1:0])
									0: q_o <= data_0_i + 800;
									1: q_o <= data_1_i + 900;
									2: q_o <= data_2_i + 1000;
									3: q_o <= data_3_i + 1100;
								endcase
							end
						endcase
					end
				endcase
			end
		endcase
	end
endmodule
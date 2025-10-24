module pmux(
	input [1:0] sel,
	input [31:0] data_i,
	output reg [31:0] q_o
);
	always @* begin
		case (sel[1:0])
			2'h3: q_o = {24'hFFFFFF, data_i[31:24]};
			2'h2: q_o = {24'hFFFFFF, data_i[23:16]};
			2'h1: q_o = {24'hFFFFFF, data_i[15:8]};
			2'h0: q_o = {24'hFFFFFF, data_i[7:0]};
		endcase
	end
endmodule
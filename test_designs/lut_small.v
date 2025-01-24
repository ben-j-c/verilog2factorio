module top(
	input [31:0] signal_0,
	input [31:0] signal_1,
	output reg [31:0] signal_A
	);
	wire [31:0] tmp;
	assign tmp = signal_0*signal_1;
	always @(*) begin
		if (tmp[0] && tmp[1] && tmp[2]) begin
			signal_A <= 1;
		end else if (!tmp[0] && !tmp[1] && tmp[2]) begin 
			signal_A <= 1;
		end else if (tmp[0] && !tmp[1] && !tmp[2]) begin 
			signal_A <= 1;
		end else if (!tmp[0] && tmp[1] && !tmp[2]) begin 
			signal_A <= 1;
		end else begin
			signal_A <= 0;
		end
	end
endmodule
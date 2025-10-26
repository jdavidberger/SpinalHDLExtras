module ClockGenerator #(
    parameter real FREQ_HZ = 100000000.0,  // Default 100 MHz
    parameter real PHASE_NS = 0.0           // Default 0 ns phase offset
) (
    input  wire async_reset,
    output reg  clk
);

    // Calculate period in ns
    localparam real PERIOD_NS = 1000000000.0 / FREQ_HZ;
    localparam real HALF_PERIOD_NS = PERIOD_NS / 2.0;

    initial begin
        clk = 1'b0;
        // Apply phase offset
        if (PHASE_NS > 0) begin
            #(PHASE_NS);
        end

        // Generate clock
        forever begin
            if (async_reset) begin
                clk = 1'b0;
                @(negedge async_reset);  // Wait for reset to deassert
                if (PHASE_NS > 0) begin
                    #(PHASE_NS);
                end
            end else begin
                #(HALF_PERIOD_NS) clk = ~clk;
            end
        end
    end

endmodule
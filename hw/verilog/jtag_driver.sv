`timescale 1ns/1ps

module jtag_driver #(
    parameter int IR_WIDTH = 8
)(
    input  wire reset,        // active high
    output reg  jtag_tck,
    output reg  jtag_tms,
    output reg  jtag_tdi,
    input  wire jtag_tdo
);

    // ----------------------------
    // 1 MHz TCK generation
    // ----------------------------
    initial jtag_tck = 0;
    always #500 jtag_tck = ~jtag_tck; // 1000ns period

    // ----------------------------
    // JTAG primitives
    // ----------------------------
    task automatic jtag_clock(input bit tms_val, input bit tdi_val);
    begin
        jtag_tms = tms_val;
        jtag_tdi = tdi_val;
        @(posedge jtag_tck);
    end
    endtask

    task automatic goto_test_logic_reset;
        int i;
        begin
            for (i = 0; i < 6; i++) begin
                jtag_clock(1, 0);
            end
        end
    endtask

    task automatic goto_shift_ir;
    begin
        // TLR -> RTI -> Select-DR -> Select-IR -> Capture-IR -> Shift-IR
        jtag_clock(0, 0);
        jtag_clock(1, 0);
        jtag_clock(1, 0);
        jtag_clock(0, 0);
        jtag_clock(0, 0);
    end
    endtask

    task automatic goto_shift_dr;
    begin
        // Update-IR -> RTI -> Select-DR -> Capture-DR -> Shift-DR
        jtag_clock(0, 0);
        jtag_clock(1, 0);
        jtag_clock(0, 0);
        jtag_clock(0, 0);
    end
    endtask

    task automatic shift_ir(input logic [IR_WIDTH-1:0] value);
        int i;
        begin
            // LSB-first shifting
            for (i = 0; i < IR_WIDTH; i++) begin
                if (i == IR_WIDTH-1)
                    jtag_clock(1, value[i]); // Exit1-IR
                else
                    jtag_clock(0, value[i]);
            end

            // Update-IR
            jtag_clock(1, 0);
        end
    endtask

    task automatic shift_dr_forever;
    begin
        forever begin
            jtag_clock(0, 0); // stay in Shift-DR, shift zeros
        end
    end
    endtask

    // ----------------------------
    // Main sequence
    // ----------------------------
    initial begin
        jtag_tms = 1'b1;
        jtag_tdi = 1'b0;

        // Wait for reset deassertion
        wait (reset == 1'b0);

        // Ensure TAP is in known state
        goto_test_logic_reset();

        // shift_ir
        goto_shift_ir();
        shift_ir(8'b00100100);

        // shift_dr
        goto_shift_dr();
        shift_dr_forever();
    end

endmodule
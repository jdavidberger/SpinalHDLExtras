`timescale 1ns/1ps

module Ram_1w_1rs #(
    parameter wordCount = 640,
    parameter wordWidth = 128,
    parameter clockCrossing = 1'b0,
    parameter technology = "auto",
    parameter readUnderWrite = "dontCare",
    parameter wrAddressWidth = 10,
    parameter wrDataWidth = 128,
    parameter wrMaskWidth = 1,
    parameter wrMaskEnable = 1'b0,
    parameter rdAddressWidth = 10,
    parameter rdDataWidth = 128
)(
    input wr_clk,
    input wr_en,
    input wr_mask,
    input [wrAddressWidth-1:0] wr_addr,
    input [wrDataWidth-1:0] wr_data,
    input rd_clk,
    input rd_en,
    input [rdAddressWidth-1:0] rd_addr,
    output [rdDataWidth-1:0] rd_data
);

if (technology == "distributedLut") begin
   //assert(wrMaskEnable == 1'b0)
   
   lscc_distributed_dpram # (
       .WADDR_DEPTH(wordCount),
       .WADDR_WIDTH(wrAddressWidth),
       .WDATA_WIDTH(wrDataWidth),
       .RADDR_DEPTH(wordCount),
       .RADDR_WIDTH(rdAddressWidth),
       .RDATA_WIDTH(rdDataWidth),
       .REGMODE("reg"),
       .MODULE_TYPE("lscc_distributed_dpram"),
       .BYTE_SIZE(8),
       .ECC_ENABLE("")
   ) dpram_instance(
       .wr_clk_i(wr_clk),
       .rd_clk_i(rd_clk),
       .rst_i(1'b0),
       .wr_clk_en_i(1'b1),
       .rd_clk_en_i(1'b1),

       .wr_en_i(wr_en),
       .wr_data_i(wr_data),
       .wr_addr_i(wr_addr),
       .rd_en_i(rd_en),
       .rd_addr_i(rd_addr),

       .rd_data_o(rd_data)
   );

end else begin
    lscc_ram_dp_true # (
        .FAMILY("LIFCL"),
        .ADDR_DEPTH_A(wordCount),
        .ADDR_WIDTH_A(wrAddressWidth),
        .DATA_WIDTH_A(wrDataWidth),
        .ADDR_DEPTH_B(wordCount),
        .ADDR_WIDTH_B(rdAddressWidth),
        .DATA_WIDTH_B(rdDataWidth),
        .GSR("enable"),
        .MODULE_TYPE("ram_dp_true"),
        .BYTE_ENABLE_A(wrMaskEnable),
        .BYTE_SIZE_A(wrMaskWidth),
        .BYTE_EN_POL_A("active-high"),
        .WRITE_MODE_A("normal"),
        .BYTE_ENABLE_B(wrMaskEnable),
        .BYTE_SIZE_B(wrMaskWidth),
        .MEM_ID("MEM0")
    ) RAM_instance(
        .addr_a_i(wr_addr),
        .addr_b_i(rd_addr),
        .wr_data_a_i(wr_data),
        .wr_data_b_i(0),
        .clk_a_i(wr_clk),
        .clk_b_i(rd_clk),
        .clk_en_a_i(wr_en),
        .clk_en_b_i(rd_en),
        .wr_en_a_i(wr_en),
        .wr_en_b_i(0),
        .rst_a_i(1'b0),
        .rst_b_i(1'b0),
        .ben_a_i(wr_mask),
        .ben_b_i(0),
        .rd_data_a_o(),
        .rd_data_b_o(rd_data),
        .ecc_one_err_a_o(),
        .ecc_two_err_a_o(),
        .ecc_one_err_b_o(),
        .ecc_two_err_b_o()
    );
end


endmodule

module Ram_2wrs #(
    parameter wordCount = 256,
    parameter wordWidth = 16,
    parameter clockCrossing = 1'b0,
    parameter technology = "auto",
    parameter portA_readUnderWrite = "dontCare",
    parameter portA_duringWrite = "dontCare",
    parameter portA_addressWidth = 8,
    parameter portA_dataWidth = 16,
    parameter portA_maskWidth = 1,
    parameter portA_maskEnable = 1'b0,
    parameter portB_readUnderWrite = "dontCare",
    parameter portB_duringWrite = "dontCare",
    parameter portB_addressWidth = 8,
    parameter portB_dataWidth = 16,
    parameter portB_maskWidth = 1,
    parameter portB_maskEnable = 1'b0
)(
    input portA_clk,
    input portA_en,
    input portA_wr,
    input portA_mask,
    input [portA_addressWidth-1:0] portA_addr,
    input [portA_dataWidth-1:0] portA_wrData,
    output [portA_dataWidth-1:0] portA_rdData,
    input portB_clk,
    input portB_en,
    input portB_wr,
    input portB_mask,
    input [portB_addressWidth-1:0] portB_addr,
    input [portB_dataWidth-1:0] portB_wrData,
    output [portB_dataWidth-1:0] portB_rdData
);


lscc_ram_dp_true # (
    .FAMILY("LIFCL"),
    .ADDR_DEPTH_A(wordCount),
    .ADDR_WIDTH_A(portA_addressWidth),
    .DATA_WIDTH_A(portA_dataWidth),
    .ADDR_DEPTH_B(wordCount),
    .ADDR_WIDTH_B(portB_addressWidth),
    .DATA_WIDTH_B(portB_dataWidth),
    .GSR("enable"),
    .MODULE_TYPE("ram_dp_true"),
    .BYTE_ENABLE_A(portA_maskEnable),
    .BYTE_SIZE_A(portA_maskWidth),
    .BYTE_EN_POL_A("active-high"),
    .WRITE_MODE_A("normal"),
    .BYTE_ENABLE_B(portB_maskEnable),
    .BYTE_SIZE_B(portB_maskWidth),
    .MEM_ID("MEM0")
) RAM_instance(
    .addr_a_i(portA_addr),
    .addr_b_i(portB_addr),
    .wr_data_a_i(portA_wrData),
    .wr_data_b_i(portB_wrData),
    .clk_a_i(portA_clk),
    .clk_b_i(portB_clk),
    .clk_en_a_i(portA_en),
    .clk_en_b_i(portB_en),
    .wr_en_a_i(portA_wr),
    .wr_en_b_i(portB_wr),
    .rst_a_i(1'b0),
    .rst_b_i(1'b0),
    .ben_a_i(portA_mask),
    .ben_b_i(portB_mask),
    .rd_data_a_o(portA_rdData),
    .rd_data_b_o(portB_rdData),
    .ecc_one_err_a_o(),
    .ecc_two_err_a_o(),
    .ecc_one_err_b_o(),
    .ecc_two_err_b_o()
);
endmodule

module Ram_1wrs #(
    parameter wordCount = 128,
    parameter wordWidth = 64,
    parameter readUnderWrite = "",
    parameter duringWrite = "",
    parameter technology = "",
    parameter maskWidth = 8,
    parameter maskEnable = 1
)(
    input clk,
    input en,
    input wr,
    input [$clog2(wordCount)-1:0] addr,
    input [(wordWidth/8-1):0] mask,
    input [(wordWidth-1):0] wrData,
    output [(wordWidth-1):0] rdData
);


if (technology == "LRAM") begin

LRAM #(
	// Parameters.
	.ECC_BYTE_SEL ("BYTE_EN")
) LRAM_instance (
	.CEA       (1'd1),
	.CEB       (1'd1),
	.CSA       (1'd1),
	.CSB       (1'd1),
	.OCEA      (1'd1),
	.OCEB      (1'd1),
    .RSTA      (1'd0),
    .RSTB      (1'd0),
	.CLK       (clk),

    .DPS       (1'd0),
	// Inputs.
	.ADA       (addr << 1),
	.DIA       (wrData[wordWidth/2-1:0]),
	.WEA       (wr),
    .BENA_N (~mask[maskWidth/2-1:0]),
	.DOA      (rdData[wordWidth/2-1:0]),

	.ADB       ((addr << 1) | 1'd1),
	.DIB       (wrData[wordWidth-1:wordWidth/2]),
	.WEB       (wr),
	.BENB_N (~mask[maskWidth-1:maskWidth/2]),
	.DOB      (rdData[wordWidth-1:wordWidth/2])
);

/*
    lscc_lram_sp # (
        .FAMILY("LIFCL"),
        .MEM_ID("MEM0"),
        .MEM_SIZE(wordWidth + "," + wordCount),
        .ADDR_DEPTH(wordCount),
        .DATA_WIDTH(wordWidth),
        .REGMODE("noreg"),
        .RESETMODE("sync"),
        .RESET_RELEASE("sync"),
        .BYTE_ENABLE(maskEnable),
        .ECC_ENABLE(0),
        .WRITE_MODE("normal"),
        .UNALIGNED_READ(0),
        .PRESERVE_ARRAY(0),
        .ADDR_WIDTH($clog2(wordCount)),
        .BYTE_WIDTH(wordWidth/8),
        .GSR_EN(0)
    ) LRAM_instance(
        .clk_i(clk),
        .dps_i(0),
        .rst_i(1'b0),

        .errdet_o(),
        .lramready_o(),

        .clk_en_i(en),
        .rdout_clken_i(1'b1),
        .wr_en_i(wr),
        .wr_data_i(wrData),
        .addr_i(addr),
        .ben_i(mask),

        .rd_data_o(rdData),
        .rd_datavalid_o(),
        .ecc_errdet_o()
    );

        lscc_lram_dp_true #(
            .MEM_ID("MEM0"),
            //.MEM_SIZE(wordWidth + "," + wordCount),
            .FAMILY("LIFCL"),
            .ADDR_DEPTH_A(wordCount),
            .ADDR_WIDTH_A($clog2(wordCount) + 1),
            .DATA_WIDTH_A(wordWidth / 2),
            .ADDR_DEPTH_B(wordCount),
            .ADDR_WIDTH_B($clog2(wordCount) + 1),
            .DATA_WIDTH_B(wordWidth / 2),
            .REGMODE_A("noreg"),
            .REGMODE_B("noreg"),
            .GSR_EN(0),
            .RESETMODE("sync"),
            .RESET_RELEASE("sync"),
            .BYTE_ENABLE_A(1),
            .BYTE_ENABLE_B(1),
            .ECC_ENABLE(0)
        ) LRAM_instance(
            
                .clk_i(clk),
                .dps_i(0),

                .errdet_o(),
                .lramready_o(),
                // --------------------------
                // ----- Port A signals -----
                // --------------------------
                .rst_a_i(1'b0),
                .clk_en_a_i(1'b1),
                .rdout_clken_a_i(1'b1),
                .wr_en_a_i(wr),
                .wr_data_a_i(wrData[wordWidth/2-1:0]),
                .addr_a_i(addr << 1),
                .ben_a_i(mask[maskWidth/2-1:0]),

                .rd_data_a_o(rdData[wordWidth/2-1:0]),
                .rd_datavalid_a_o(),
                .ecc_errdet_a_o(),

                // --------------------------
                // ----- Port B signals -----
                // --------------------------

                .rst_b_i(1'b0),
                .clk_en_b_i(1'b1),
                .rdout_clken_b_i(1'b1),
                .wr_en_b_i(wr),
                .wr_data_b_i(wrData[wordWidth-1:wordWidth/2]),
                .addr_b_i((addr << 1) | 1),
                .ben_b_i(mask[maskWidth-1:maskWidth/2]),

                .rd_data_b_o(rdData[wordWidth-1:wordWidth/2]),
                .rd_datavalid_b_o(),
                .ecc_errdet_b_o()
        );
        */
end else begin
    wire wr_clk_i = clk;
    wire rd_clk_i = clk;
    wire rst_i = 1'b0;
    wire wr_en_i = en & wr;
    wire rd_en_i = en & ~wr;

    lscc_ram_dp #(
        .MEM_ID("MEM0"),
        .MEM_SIZE(wordWidth + "," + wordCount),
        .FAMILY("LIFCL"),
        .WADDR_DEPTH(wordCount),
        .WADDR_WIDTH($clog2(wordCount)),
        .WDATA_WIDTH(wordWidth),
        .RADDR_DEPTH(wordCount),
        .RADDR_WIDTH($clog2(wordCount)),
        .RDATA_WIDTH(wordWidth),
        .REGMODE("noreg"),
        .GSR("enable"),
        .RESETMODE("sync"),
        .RESET_RELEASE("sync"),
        .MODULE_TYPE("ram_dp"),
        .INIT_MODE("none"),
        .BYTE_ENABLE(1),
        .BYTE_SIZE(8),
        .BYTE_WIDTH(wordWidth/8),
        .PIPELINES(0),
        .ECC_ENABLE(0),
        .OUTPUT_CLK_EN(0),
        .BYTE_ENABLE_POL("active-high")
    ) RAM_instance(
        .wr_clk_i(wr_clk_i),                 
        .rd_clk_i(rd_clk_i),                 
        .rst_i(rst_i),                    
        .wr_clk_en_i(1'b1),              
        .rd_clk_en_i(1'b1),              
        .rd_out_clk_en_i(1'b1),
        .wr_en_i(wr_en_i),                  
        .wr_data_i(wrData),
        .wr_addr_i(addr),
        .rd_en_i(rd_en_i),                  
        .rd_addr_i(addr),
        .ben_i(mask),
        .rd_data_o(rdData),
        .one_err_det_o(),
        .two_err_det_o()                 
    );
end

endmodule

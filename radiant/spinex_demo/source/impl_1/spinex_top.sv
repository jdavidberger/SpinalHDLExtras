/**
* @page fpga_top_som
* @brief Top level file for the FPGA
*/
`default_nettype wire


module spinex_top (
	input  wire       button_n            , // Reset button from devboard
	input  wire       clk_2               , // Clock from PLL, usually set to 24MHz as this is a common value
	input  wire       uart_rxd            ,
	output wire       uart_txd            ,
	inout  wire       scl                 ,
	inout  wire       sda                 ,
	output wire       spiflash_clk        ,
	output wire       spiflash_cs_n       ,
	inout  wire [3:0] spiflash_dq         ,


	input wire jtag_tck,
	input wire jtag_tdi,
	output wire jtag_tdo,
	input wire jtag_tms,

	output wire          phy_CLK,
	output wire          phy_CEn,
	inout  wire [1:0]    phy_DQSDM,
	inout  wire [15:0]   phy_DQ,
	
	output wire       camera_led0
  
);

WDT wdt(
	.WDTRELOAD(0),
	.WDT_CLK(0),
	.WDT_RST(0)
);

SpinexWithClock spinal_top(
    .clk(clk_2),
	.reset(!button_n),
	 .led(camera_led0),
	.i2c0_scl(scl),
	.i2c0_sda(sda),
				 
	 //.serial_tx(uart_txd),
	 //.serial_rx(uart_rxd),
	 //.spiflash4x_clk(spiflash_clk),
     //.spiflash4x_cs_n(spiflash_cs_n),
     //.spiflash4x_dq(spiflash_dq),
	 
	 
	.*
);

endmodule
`define nettype wire
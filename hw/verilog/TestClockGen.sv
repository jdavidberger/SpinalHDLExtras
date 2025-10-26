
`timescale 1ns/1ps

module TestClockGen(output reg eclk, output reg eclk_90, output reg eclk_270, output reg sclk, output reg reset);

  parameter ECLK_PERIOD;
  parameter SCLK_MULT;
  parameter PHASE;

  initial begin
    eclk_90 = 1;
    #(PHASE + (3*ECLK_PERIOD/4))
    forever #(ECLK_PERIOD/2) eclk_90 = ~eclk_90; // 60 MHz clock
  end

  initial begin
    eclk_270 = 1;
    #(PHASE + (ECLK_PERIOD/4))
    forever #(ECLK_PERIOD/2) eclk_270 = ~eclk_270; // 60 MHz clock
  end

  initial begin
    eclk = 1;
    #PHASE
    forever #(ECLK_PERIOD/2) eclk = ~eclk; // 60 MHz clock
  end

  initial begin
    sclk = 1;
    #PHASE
    forever #(ECLK_PERIOD*SCLK_MULT/2) sclk = ~sclk; // 60 MHz clock
  end

  initial begin
    reset = 1;
    #166.6
    reset = 0;
  end
 endmodule

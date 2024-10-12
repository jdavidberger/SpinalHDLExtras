#!/bin/bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
#GEN_DIR=$SCRIPT_DIR/../hw/gen
GEN_DIR=$PWD/hw/gen
VERILOG_DIR=$SCRIPT_DIR/../hw/verilog

TESTS=${1:-LatticeODDRSim LatticeIDDRSim LatticeFifoTest LatticeMemoryTest}

VLOG="docker run -it --rm -v$HOME/.config/LatticeSemi:/root/.config/LatticeSemi -v$HOME:$HOME -v /tmp/.X11-unix:/tmp/.X11-unix --workdir $GEN_DIR -e DISPLAY=$DISPLAY -h $HOSTNAME -v $HOME/.Xauthority:/home/justin/.Xauthority cr:radiant2023.2 vlog"
VSIM="docker run -it --rm -v$HOME/.config/LatticeSemi:/root/.config/LatticeSemi -v$HOME:$HOME -v /tmp/.X11-unix:/tmp/.X11-unix --workdir $GEN_DIR -e DISPLAY=$DISPLAY -h $HOSTNAME -v $HOME/.Xauthority:/home/justin/.Xauthority cr:radiant2023.2 vsim"


for test_name in $TESTS;
do
  sbt "runMain spinalextras.lib.tests.$test_name"
  $VLOG \
  -work $test_name $GEN_DIR/$test_name.v \
  $VERILOG_DIR/TestClockGen.sv

  time $VSIM -L lifcl $test_name.$test_name -do "run 100us;exit -code 1"
done

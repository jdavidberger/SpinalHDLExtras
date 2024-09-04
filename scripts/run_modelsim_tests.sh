#!/bin/bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
#GEN_DIR=$SCRIPT_DIR/../hw/gen
GEN_DIR=$PWD/hw/gen
VERILOG_DIR=$SCRIPT_DIR/../hw/verilog

export RADIANT_ROOT=/data/justin/lscc/radiant/2023.2/
export LM_LICENSE_FILE=$RADIANT_ROOT/license/license.dat
SIMPATH=$RADIANT_ROOT/modeltech/linuxloem/

TESTS=${1:-LatticeODDRSim LatticeIDDRSim LatticeFifoTest LatticeMemoryTest}

for test_name in $TESTS;
do
  sbt "runMain spinalextras.lib.tests.$test_name"
  $SIMPATH/vlog \
  -work $test_name $GEN_DIR/$test_name.v \
  $VERILOG_DIR/TestClockGen.sv

  time $SIMPATH/vsim -L lifcl $test_name.$test_name -do "run 100us;"
done

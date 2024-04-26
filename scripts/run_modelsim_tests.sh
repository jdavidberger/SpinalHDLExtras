#!/bin/bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
GEN_DIR=$SCRIPT_DIR/../hw/gen
VERILOG_DIR=$SCRIPT_DIR/../hw/verilog

export RADIANT_ROOT=/data/justin/lscc/radiant/2023.2/
export LM_LICENSE_FILE=$RADIANT_ROOT/license/license.dat
SIMPATH=$RADIANT_ROOT/modeltech/linuxloem/

$SIMPATH/vlog -help

$SIMPATH/vlog \
-work LatticeODDRSim $GEN_DIR/LatticeODDRSim.v \
$VERILOG_DIR/TestClockGen.sv

# Asserts will exit before we exit; so exit code 1 is passing
$SIMPATH/vsim -c -L lifcl LatticeODDRSim.LatticeODDRSim -do "run 10us;exit -code 1"

$SIMPATH/vlog \
-work LatticeIDDRSim $GEN_DIR/LatticeIDDRSim.v \
$VERILOG_DIR/TestClockGen.sv

$SIMPATH/vsim -L lifcl LatticeIDDRSim.LatticeIDDRSim -do "run 10us" #;exit -code 1"

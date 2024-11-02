
yosys read_verilog /data/justin/lscc/radiant/2024.1/cae_library/synthesis/verilog/lifcl.v
yosys read_verilog $::env(FILE).v
yosys hierarchy -check -auto-top

yosys proc
yosys flatten
yosys opt_expr
yosys opt_clean
yosys check
yosys opt


yosys write_verilog -nodec -noattr obfuscated/$::env(FILE).v

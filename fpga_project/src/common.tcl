#!/usr/bin/env gw_sh
# A simple gw_sh build script
# by andelf

add_file -type verilog "src/Testspinal.v"
add_file -type cst "src/tang-nano-9k.cst"

# NOTE: Tang Nano 9k is a GW1NR-9C device! not GW1NR-9
set_device GW1NR-LV9QN88PC6/I5 -name GW1NR-9C

set_option -synthesis_tool gowinsynthesis
set_option -output_base_name Testspinal
set_option -cst_warn_to_error 0
set_option -bit_security 0
set_option -use_mspi_as_gpio 1
set_option -use_sspi_as_gpio 1

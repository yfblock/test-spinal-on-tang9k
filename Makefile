# Genreated by project template

# GOWINHOME=

ifneq ($(GOWINHOME),)
GW_SH := $(GOWINHOME)/IDE/bin/gw_sh
else
GW_SH := gw_sh
endif

SOURCES = src/main/scala/io/github/yfblock/*.scala
VERILOG_FILE := fpga_project/src/Testspinal.v
BITSTREAM_FILE := fpga_project/impl/pnr/Testspinal.fs
REPORT_FILE := fpga_project/impl/pnr/Testspinal.rpt.txt

all: $(BITSTREAM_FILE)

$(BITSTREAM_FILE) $(REPORT_FILE): $(VERILOG_FILE) fpga_project/src/*.tcl
	cd fpga_project && $(GW_SH) src/synth.tcl
	@cat $(REPORT_FILE) | grep -A32 "Resource Usage Summary"

$(VERILOG_FILE): $(SOURCES) build.sbt fpga_project/src/tang-nano-9k.cst
	sbt "runMain io.github.yfblock.Main"

verilog: $(VERILOG_FILE)

sim: $(SOURCES) build.sbt
	sbt "runMain io.github.yfblock.TestSpinalSim"
	gtkwave ./simWorkspace/Testspinal/test.vcd

clean:
	rm -rf impl $(VERILOG_FILE)

load: $(BITSTREAM_FILE)
	openFPGALoader -b tangnano9k $<

flash: $(BITSTREAM_FILE)
	openFPGALoader -b tangnano9k $< -f

.PHONY: all clean flash sim verilog

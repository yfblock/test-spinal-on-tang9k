# Genreated by project template

# GOWINHOME=

ifneq ($(GOWINHOME),)
GW_SH := $(GOWINHOME)/IDE/bin/gw_sh
else
GW_SH := gw_sh
endif

SOURCES = src/main/scala/io/github/yfblock/*.scala
VERILOG_FILE := rtl/Testspinal.v
BITSTREAM_FILE := impl/pnr/Testspinal.fs
REPORT_FILE := impl/pnr/Testspinal.rpt.txt

all: $(BITSTREAM_FILE)

$(BITSTREAM_FILE) $(REPORT_FILE): $(VERILOG_FILE) rtl/*.tcl
	$(GW_SH) rtl/synth.tcl
	@cat $(REPORT_FILE) | grep -A32 "Resource Usage Summary"

$(VERILOG_FILE): $(SOURCES) build.sbt rtl/tang-nano-9k.cst
	sbt "runMain io.github.yfblock.Main"

sim: $(SOURCES) build.sbt
	sbt "runMain io.github.yfblock.TestSpinalSim"

clean:
	rm -rf impl $(VERILOG_FILE)

flash: $(BITSTREAM_FILE)
	openFPGALoader -b tangnano9k $<

.PHONY: all clean flash

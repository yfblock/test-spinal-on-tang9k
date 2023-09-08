package io.github.yfblock.Gowin

import spinal.core._

// IDIV_SEL 0~63
// FBDIV_SEL 0~63
// ODIV_SEL 2,4,8,16,32,48,64,80,96,112,128
// DYN_SDIV_SEL 2~128
class rPLL(idivSel: Int, fbdivSel: Int, odivSel: Int) extends BlackBox {
  val generic = new Generic {
    val FCLKIN           = "27"
    val DYN_IDIV_SEL     = "false"
    val IDIV_SEL         = idivSel
    val DYN_FBDIV_SEL    = "false"
    val FBDIV_SEL        = fbdivSel
    val DYN_ODIV_SEL     = "false"
    val ODIV_SEL         = odivSel
    val PSDA_SEL         = "0000"
    val DYN_DA_EN        = "true"
    val DUTYDA_SEL       = "1000"
    val CLKOUT_FT_DIR    = B"1"
    val CLKOUTP_FT_DIR   = B"1"
    val CLKOUT_DLY_STEP  = 0
    val CLKOUTP_DLY_STEP = 0
    val CLKFB_SEL        = "internal"
    val CLKOUT_BYPASS    = "false"
    val CLKOUTP_BYPASS   = "false"
    val CLKOUTD_BYPASS   = "false"

    val DYN_SDIV_SEL = 2
    val CLKOUTD_SRC  = "CLKOUT"
    val CLKOUTD3_SRC = "CLKOUT"
    val DEVICE       = "GW1NR-9C"
  }
  val io = new Bundle {
    val CLKIN    = in Bool ()
    val CLKFB    = in Bool ()
    val RESET    = in Bool ()
    val RESET_P  = in Bool ()
    val FBDSEL   = in Bits (6 bits)
    val IDSEL    = in Bits (6 bits)
    val ODSEL    = in Bits (6 bits)
    val PSDA     = in Bits (4 bits)
    val DUTYDA   = in Bits (4 bits)
    val FDLY     = in Bits (4 bits)
    val CLKOUT   = out Bool ()
    val LOCK     = out Bool ()
    val CLKOUTP  = out Bool ()
    val CLKOUTD  = out Bool ()
    val CLKOUTD3 = out Bool ()
  }
  noIoPrefix()
  setBlackBoxName("rPLL")

  def assignDefaults = {
    io.RESET   := False
    io.RESET_P := False
    io.CLKFB   := False
    io.FBDSEL  := B(0, 6 bits)
    io.IDSEL   := B(0, 6 bits)
    io.ODSEL   := B(0, 6 bits)
    io.DUTYDA  := B(0, 4 bits)
    io.PSDA    := B(0, 4 bits)
    io.FDLY    := B(0, 4 bits)
  }
}

package io.github.yfblock.Gowin

import spinal.core._

import scala.language.postfixOps

// 250MHz
// Reference: http://cdn.gowinsemi.com.cn/UG286-1.9.6_Gowin%E6%97%B6%E9%92%9F%E8%B5%84%E6%BA%90(Clock)%E7%94%A8%E6%88%B7%E6%8C%87%E5%8D%97.pdf
class Osc(freqDiv: Int) extends BlackBox {
  val generic = new Generic {
    val FREQ_DIV = freqDiv
    val DEVICE   = "GW1NR-9C"
  }
  val io = new Bundle {
    val OSCOUT = out Bool ()
  }
  noIoPrefix()
  setBlackBoxName("OSC")
}

object OscClockDomain {
  def apply(freqDiv: Int, reset_button: Bool): ClockDomain = {
    val osc = new Osc(freqDiv);
    ClockDomain(
      clock = osc.io.OSCOUT,
      reset = reset_button,
      frequency = FixedFrequency(250.0/freqDiv MHz),
      config = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = SYNC,
        resetActiveLevel = LOW
      )
    )
  }
}
package io.github.yfblock

import spinal.core._

object RealClock {
    def apply(tm: TimeDisplay): RealClock = {
        val real_clock = new RealClock()
        real_clock.io.tm <> tm
        real_clock
    }
}

class RealClock extends Component {
  val io = new Bundle {
    val tm = out(TimeDisplay()).setAsReg()
  }

  val counter = Reg(UInt(40 bits)) init(0)

  counter := counter + 1
  when(counter === 100000) {
    counter := 0
    io.tm.rt4 := io.tm.rt4 + 1
  }
  when(io.tm.rt4 === 9) {
    io.tm.rt4 := 0
    io.tm.rt3 := io.tm.rt3 + 1
  }

  when(io.tm.rt3 === 5) {
    io.tm.rt3 := 0
    io.tm.rt2 := io.tm.rt2 + 1
  }

  when(io.tm.rt2 === 9) {
    io.tm.rt2 := 0
    io.tm.rt1 := io.tm.rt1 + 1
  }

  when(io.tm.rt1 === 5) {
    io.tm.rt1 := 0
  }
}

package io.github.yfblock

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.com.uart.UartCtrl

class UartRx extends Component {
  val io = new Bundle {
    val rxp = in(Bool())

  }

  noIoPrefix()
  
  UartCtrl
}

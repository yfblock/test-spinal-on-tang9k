package io.github.yfblock

import spinal.core._
import spinal.lib._
import spinal.lib.com.uart.UartCtrl
import spinal.lib.com.uart.UartCtrlGenerics
import spinal.lib.com.uart.UartCtrlInitConfig
import spinal.lib.com.uart.UartParityType
import spinal.lib.com.uart.UartStopType

object UartController {
  def apply(rx: Bool, rxd: Stream[Bits], tx: Bool, txd: Stream[Bits]) = {
    val uart = UartCtrl(
      // baudrate 9600 and mode is 8N1
      UartCtrlInitConfig(
        baudrate = 9600,
        dataLength = 7,
        parity = UartParityType.NONE,
        stop = UartStopType.ONE
      ),
      false
    )

    uart.io.uart.rxd <> rx
    rxd << uart.io.read
    uart.io.uart.txd <> tx
    uart.io.write << txd
  }
}

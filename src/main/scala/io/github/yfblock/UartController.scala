package io.github.yfblock

import spinal.core._
import spinal.lib._
import spinal.lib.com.uart.UartCtrl
import spinal.lib.com.uart.UartCtrlGenerics
import spinal.lib.com.uart.UartCtrlInitConfig
import spinal.lib.com.uart.UartParityType
import spinal.lib.com.uart.UartStopType
import spinal.lib.com.uart.Uart

object UartController {
  def apply(uartPort: Uart, rxd: Stream[Bits], txd: Stream[Bits]) = {
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

    uart.io.uart <> uartPort
    rxd << uart.io.read
    uart.io.write << txd
  }
}

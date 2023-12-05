package io.github.yfblock

import spinal.core._
import scala.language.postfixOps

/// url: https://github.com/xiaotianbc/spinalHDL-uart-demo.git
object UartTx {
    def apply(txp: Bool): UartTx = {
        val uartTx = new UartTx()
        uartTx.io.txp <> txp
        uartTx.io.tx_en := True
        uartTx.io.tx_data := '3'
        uartTx
    }
}

class UartTx(baudrate: Int = 115200) extends Component {
//   val BAUD_CNT_MAX = (clk_freq / baudrate) - 1
  val BAUD_CNT_MAX = (this.clockDomain.frequency.getValue.toInt / baudrate) - 1
  val baud_cnt = Reg(UInt(log2Up(BAUD_CNT_MAX) bits)) init 0

  val io = new Bundle {
    val txp = out(Reg(Bool())) init True //发送端口
    val tx_data = in(Bits(8 bits))
    val tx_en = in(Bool()) //使能发送
    val is_busy = out(Bool())
  }

  noIoPrefix()

  val tx_data_r = Reg(Bits(10 bits)) init 0
  val tx_bits_counter = Reg(UInt(log2Up(10) bits)) init 0 //定义一个宽度为log2Up(8)的无符号多位寄存器

  object State extends SpinalEnum {
    val idle, working = newElement()
  }

  val state = RegInit(State.idle)

  //这里如果tx_en被置位则也让busy置位
  io.is_busy := (!(state === State.idle)) || io.tx_en

  switch(state) {
    is(State.idle) {
      io.txp := True
      when(io.tx_en) {
        //使用 ## 对Bits类型进行拼接，前面的是MSB，后面的是LSB。因为最先发送起始位，所以最低位为0
        tx_data_r := B"1" ## io.tx_data ## B"0"
        baud_cnt := 0
        tx_bits_counter := 0
        state := State.working
      }
    }
    is(State.working) {
      io.txp := tx_data_r(tx_bits_counter)
      baud_cnt := baud_cnt + 1
      when(baud_cnt === BAUD_CNT_MAX) {
        baud_cnt := 0
        tx_bits_counter := tx_bits_counter + 1
        when(tx_bits_counter === 9) {
          state := State.idle
        }
      }
    }
  }
}
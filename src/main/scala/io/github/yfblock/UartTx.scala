package io.github.yfblock

import spinal.core._
import scala.language.postfixOps
import spinal.lib.fsm.StateMachine
import spinal.lib.fsm.EntryPoint
import spinal.lib.fsm.State
import spinal.lib.fsm.StateDelay
import spinal.lib.Counter

/// url: https://github.com/xiaotianbc/spinalHDL-uart-demo.git
object UartTx {
    def apply(txp: Bool): UartTx = {
        val uartTx = new UartTx()
        val datas = Vec("Hello\r\n".map(c => B(c.toInt, 8 bits)))
        val index = Reg(UInt(log2Up(datas.length) bits)).init(0)
        uartTx.io.tx_en := index < datas.length
        when(~uartTx.io.is_busy && uartTx.io.tx_en)(index := index + 1)
        uartTx.io.tx_data := datas(index)
        uartTx.io.txp <> txp
        // uartTx.io.tx_en := True
        // uartTx.io.tx_data := '3'
        val cnt = UInt(32 bits).setAsReg().init(0)
        cnt := cnt + 1
        when(cnt === 27000000) {
          when(index === datas.length) {
            index := 0
          }
          cnt := 0
        }
        Counter
        uartTx
    }
}

class UartTx(baudrate: Int = 115200) extends Component {
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

  io.is_busy := True

  val fsm = new StateMachine {
    val IDLE: State = new State with EntryPoint {
      whenIsActive {
        io.is_busy := False
        io.txp := True
        when(io.tx_en) {
          tx_data_r := B"1" ## io.tx_data ## B"0"
          baud_cnt := 0
          tx_bits_counter := 0
          goto(WORKING)
        }
      }
    }

    val WORKING = new State {
      whenIsActive {
        io.txp := tx_data_r(tx_bits_counter)
        baud_cnt := baud_cnt + 1
        when(baud_cnt === BAUD_CNT_MAX) {
          baud_cnt := 0
          tx_bits_counter := tx_bits_counter + 1
          when(tx_bits_counter === 9) {
            goto(END)
          }
        }
      }
    }

    val END = new StateDelay(5) {
      whenCompleted {
        goto(IDLE)
      }
    }
  }
}
package io.github.yfblock

import spinal.core._
import spinal.lib.com.spi._
import spinal.lib.fsm._
import spinal.lib.master

object P25Q32 {
  def apply(port: SpiMaster, data: Bits): P25Q32 = {
    val p25q32 = new P25Q32(ssWidth = 1)
    // port <> p25q32.io
    port <> p25q32.io.port
    data <> p25q32.io.export
    p25q32
  }
}

class P25Q32(ssWidth: Int) extends Component {
  val io = new Bundle {
    val port = master(SpiMaster(ssWidth))
    val export = out(Bits(8 bits)).setAsReg().init(0)
  }

  noIoPrefix()
  
  io.port.sclk.setAsReg().init(False)
  io.port.mosi.setAsReg().init(False)

  val bitIndex = Reg(UInt(3 bits)).init(7)
  val step = Reg(UInt(1 bits)).init(0)
  val command = Reg(Bits(8 bits)).init(0x9F)

  val fsm = new StateMachine {
    val IDLE = new State with EntryPoint {
      whenIsActive {
        goto(WR)
      }
    }

    val WR: State = new State {
      whenIsActive {
        step := step + 1
        when(step === 0) {
          io.port.sclk := False
          io.port.mosi := command(bitIndex)
        } otherwise {
          io.port.sclk := True
          io.export(bitIndex) := io.port.miso
          bitIndex := bitIndex - 1
          when(bitIndex === 0)(goto(END))
        }
      }
    }

    val END = new State {
      whenIsActive {
        when(command === 0x9F) {
          command := 0xff
          goto(WR)
        } otherwise {
          io.port.ss := 1
        }
      }
    }
  }

  io.port.ss := fsm.isActive(fsm.IDLE).asBits
}

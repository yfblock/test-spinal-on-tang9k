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
    // port.miso <> p25q32.io.port.miso
    // port.mosi <> p25q32.io.port.mosi
    // port.sclk <> p25q32.io.port.sclk
    // port.ss := 0
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

  val spi = new SpiMasterCtrl(SpiMasterCtrlGenerics(
    ssWidth,
    timerWidth = 12,
    dataWidth = 8
  ))

  spi.io.spi <> io.port
  spi.io.config.kind.cpha := False
  spi.io.config.kind.cpol := False
  spi.io.config.sclkToogle := 4
  spi.io.config.ss.activeHigh := 0
  spi.io.config.ss.setup := 1
  spi.io.config.ss.hold := 1
  spi.io.config.ss.disable := 1

  val command = Reg(Bits(8 bits)).init(0x03)
  val address = Reg(Bits(24 bits)).init(0)
  val aIndex = Reg(UInt(3 bits)).init(0)
  spi.io.cmd.setIdle()

  val fsm = new StateMachine {
    val IDLE = new State with EntryPoint {
      whenIsActive {
        goto(PSEL)
      }
    }

    val PSEL = new State {
      whenIsActive {
        spi.io.cmd.valid := True
        spi.io.cmd.payload.mode := SpiMasterCtrlCmdMode.SS
        spi.io.cmd.payload.args := 1
        when(spi.io.cmd.ready)(goto(WR))
      }
    }

    val WR: State = new State {
      whenIsActive {
        spi.io.cmd.valid := True
        // The highest bit in payload means that whether reading
        spi.io.cmd.payload.args := True ## command
        spi.io.cmd.payload.mode := SpiMasterCtrlCmdMode.DATA
        when(spi.io.cmd.ready) {
          address := address |<< 8
          when(aIndex < 3) {
            command := address(23 downto 16)
            aIndex  := aIndex + 1
          } elsewhen(aIndex === 3) {
            command := 0xff
            aIndex  := aIndex + 1
          } otherwise {
            goto(END)
          }
        }
      }
    }

    val END = new State {
      whenIsActive {
        spi.io.cmd.valid := True
        spi.io.cmd.payload.mode := SpiMasterCtrlCmdMode.SS
        spi.io.cmd.payload.args := 0
      }
    }
  }

  when(spi.io.rsp.valid) {
    io.export := spi.io.rsp.payload
  }

  
  // io.port.sclk.setAsReg().init(False)
  // io.port.mosi.setAsReg().init(False)

  // val bitIndex = Reg(UInt(3 bits)).init(7)
  // val step = Reg(UInt(1 bits)).init(0)
  // val command = Reg(Bits(8 bits)).init(0x03)
  // val address = Reg(Bits(24 bits)).init(0)
  // val aIndex = Reg(UInt(3 bits)).init(0)

  // val fsm = new StateMachine {
  //   val IDLE = new State with EntryPoint {
  //     whenIsActive {
  //       goto(WR)
  //     }
  //   }

  //   val WR: State = new State {
  //     whenIsActive {
  //       step := step + 1
  //       when(step === 0) {
  //         io.port.sclk := False
  //         io.port.mosi := command(bitIndex)
  //       } otherwise {
  //         io.port.sclk := True
  //         io.export(bitIndex) := io.port.miso
  //         bitIndex := bitIndex - 1
  //         when(bitIndex === 0)(goto(END))
  //       }
  //     }
  //   }

  //   val END = new State {
  //     whenIsActive {
  //       address := address |<< 8
  //       when(aIndex < 3) {
  //         command := address(23 downto 16)
  //         aIndex  := aIndex + 1
  //         goto(WR)
  //       } elsewhen(aIndex === 3) {
  //         command := 0xff
  //         aIndex  := aIndex + 1
  //         goto(WR)
  //       } otherwise {
  //         io.port.ss := 1
  //       }
  //     }
  //   }
  // }

  // io.port.ss := fsm.isActive(fsm.IDLE).asBits
}

package io.github.yfblock

import spinal.core._
import spinal.lib.com.spi._
import spinal.lib.fsm._
import spinal.lib.master

object P25Q32 {
  def apply(port: SpiMaster, data: Bits): P25Q32 = {
    val p25q32 = new P25Q32(ssWidth = 0)
    // port <> p25q32.io
    port.miso <> p25q32.io.port.miso
    port.mosi <> p25q32.io.port.mosi
    port.sclk <> p25q32.io.port.sclk
    port.ss := 0
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
  
  val spi = SpiMasterCtrl(SpiMasterCtrlGenerics(
    ssWidth,
    timerWidth = 32,
    dataWidth = 8
  ))

  val READ_JEDEC = B"8'h9F"
  val READ_COMMAND = B"8'h3"
  spi.io.spi <> io.port
  spi.io.cmd.setIdle()
  // spi.io.cmd.payload.mode := SpiMasterCtrlCmdMode.DATA
  spi.io.cmd.payload.args := 0

  spi.io.config.kind.cpol := False
  spi.io.config.kind.cpha := False
  // spi.io.config.ss.activeHigh := 0
  spi.io.config.sclkToogle := 4

  val addrIndex = Reg(UInt(2 bits)).init(0)
  val address = Reg(UInt(24 bits)).init(0)
  // spi.io.cmd.
  new StateMachine {
    val IDLE = new State with EntryPoint {
      whenIsActive {
        addrIndex := 0
        spi.io.cmd.valid := True
        // spi.io.cmd.payload.mode := SpiMasterCtrlCmdMode.DATA
        spi.io.cmd.payload.args := READ_JEDEC.resized
        when(spi.io.cmd.ready)(goto(END))
      }
    }

    val SEND_ADDRESS = new State {
      whenIsActive {
        spi.io.cmd.valid := True
        // spi.io.cmd.payload.mode := SpiMasterCtrlCmdMode.DATA
        spi.io.cmd.payload.args := address(7 downto 0).asBits.resized
        when(spi.io.cmd.ready) {
          addrIndex := addrIndex + 1
          when(addrIndex === 3) {
            goto(END)
          }
        }
      }
    }

    val END = new State {
      whenIsActive {
        spi.io.cmd.valid := True
        when(spi.io.rsp.valid) {
          io.export := spi.io.rsp.payload
        }
      }
    }
  }
  
  // // 创建 SPI 主设备
  // val spi = new SpiMaster(
  //   ctrlGenerics = SpiMasterCtrlGenerics(
  //     dataWidthMax    = 8,
  //     timerWidth      = 32,
  //     ssWidth         = 1
  //   ),
  //   spiGenerics = SpiMasterGenerics(
  //     ssToSclkDelays  = List(0, 0)
  //   )
  // )

  // // 连接 SPI 设备
  // io.spiMosi := spi.io.mosi
  // spi.io.miso := io.spiMiso
  // io.spiSclk := spi.io.sclk
  // io.spiSs := spi.io.ss

  // // 在状态机中控制 SPI 传输
  // val fsm = new StateMachine {
  //   val idle = new State with EntryPoint
  //   val sendData = new State
  //   val receiveData = new State

  //   idle
  //     .whenIsActive {
  //       // 发送数据到 SPI 设备
  //       spi.write(0xAB)
  //       goto(sendData)
  //     }

  //   sendData
  //     .whenIsActive {
  //       // 等待 SPI 传输完成
  //       when(spi.io.busy) {
  //         goto(sendData)
  //       } otherwise {
  //         goto(receiveData)
  //       }
  //     }

  //   receiveData
  //     .whenIsActive {
  //       // 从 SPI 设备接收数据
  //       val receivedData = spi.read()
  //       // 处理接收到的数据
  //       // ...

  //       goto(idle)
  //     }
  // }
}

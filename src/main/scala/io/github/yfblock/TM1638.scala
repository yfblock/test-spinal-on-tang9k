package io.github.yfblock

import spinal.core._
import spinal.lib._
import spinal.lib.io.TriState
import spinal.lib.fsm._
import org.jline.console.impl.Builtins.Command

case class TM1638Port() extends Bundle with IMasterSlave {
  val stb = Bool()
  val clk = Bool()
  val dio = TriState(Bool())

  def asMaster(): Unit = {
    out(stb)
    out(clk)
    master(dio)
  }

  override def setAsReg(): this.type = {
    this.stb.setAsReg().init(True)
    this.clk.setAsReg().init(True)
    this.dio.write.setAsReg().init(True)
    this
  }
}

object TM1638 {
  def apply(port: TM1638Port, ds: DataStore): TM1638 = {
    val tm1638 = new TM1638()
    port <> tm1638.io.port
    tm1638.io.display(0) := ds.getYear1.resized
    tm1638.io.display(1) := ds.getYear0
    tm1638.io.display(2) := ds.getMonth1.resized
    tm1638.io.display(3) := ds.getMonth0
    tm1638.io.display(4) := ds.getDay1.resized
    tm1638.io.display(5) := ds.getDay0
    tm1638.io.display(6) := ds.getHour1.resized
    tm1638.io.display(7) := ds.getHour0
    tm1638.io.leds       := tm1638.io.keys
    tm1638
  }
}

class TM1638 extends Component {
  val io = new Bundle {
    val port    = master(TM1638Port()).setAsReg()
    val display = in(Vec.fill(8)(UInt(4 bits)))
    val leds    = in(UInt(8 bits))
    val keys    = out(UInt(8 bits)).setAsReg().init(0)
  }

  io.port.dio.writeEnable := True
  noIoPrefix()
  val bitLoop = Reg(UInt(3 bits)) init (0);
  val step    = Reg(UInt(2 bits)) init (0);
  val dots    = Reg(UInt(8 bits)) init (0)
  val rdatas  = Reg(Vec(UInt(8 bits), 4))

  val SHOW_DISPLAY = B"8'b10001001"
  val INCRE_MODE   = B"8'b01000000"
  val WRITE_DATA   = B"8'b11000000"
  val READ_DATA    = B"8'b01000010"

  val COMMAND   = Reg(Bits(8 bits)) init (SHOW_DISPLAY)
  val dataIndex = Reg(UInt(4 bits)) init (0)
  val readIndex = Reg(UInt(2 bits)) init (0)
  val DATA = Mux(
    dataIndex(0),
    B"7'h0" ## io.leds(dataIndex >> 1),
    TMData.displayCore(io.display(dataIndex >> 1), False)
  )

  for (i <- 0 until 4) {
    // io.keys((i << 1))     := io.keys((i << 1)) | rdatas(i)(0)
    // io.keys((i << 1) + 1) := io.keys((i << 1) + 1) | rdatas(i)(4)
    io.keys(i)     := io.keys(i) | rdatas(i)(0)
    io.keys(i + 4) := io.keys(i + 4) | rdatas(i)(4)
  }

  new StateMachine {
    val START = new State with EntryPoint {
      whenIsActive {
        io.port.clk       := True
        io.port.dio.write := True
        io.port.stb       := False
        goto(GAP)
      }
    }

    val GAP: State = new State {
      whenIsActive {
        bitLoop   := 0
        dataIndex := 0
        goto(COMMAND1)
      }
    }

    val COMMAND1 = new State {
      whenIsActive {
        step := step + 1
        switch(step) {
          is(0, 2)(io.port.clk    := step(1))
          is(1)(io.port.dio.write := COMMAND(bitLoop.resized))
          is(3) {
            bitLoop := bitLoop + 1
            when(bitLoop === 7) {
              when(COMMAND === WRITE_DATA) {
                goto(WDATA)
              } elsewhen (COMMAND === READ_DATA) {
                goto(RDATA)
              } otherwise {
                goto(END)
              }
            }
          }
        }
      }
    }

    val WDATA = new State {
      whenIsActive {
        step := step + 1
        switch(step) {
          is(0, 2)(io.port.clk    := step(1))
          is(1)(io.port.dio.write := DATA(bitLoop))
          is(3) {
            bitLoop := bitLoop + 1
            when(bitLoop === 7) {
              dataIndex := dataIndex + 1
              when(dataIndex === 15)(goto(END))
            }
          }
        }
      }
    }

    val RDATA = new State {
      whenIsActive {
        io.port.dio.writeEnable := False
        step                    := step + 1
        io.port.clk             := step(1)
        when(step === 2) {
          rdatas(readIndex)(bitLoop) := io.port.dio.read
        } elsewhen (step === 3) {
          bitLoop := bitLoop + 1
          when(bitLoop === 7) {
            readIndex := readIndex + 1
            when(readIndex === 3)(goto(END))
          }
        }
      }
    }

    val END = new State {
      whenIsActive {
        io.port.stb := True
        goto(IDLE)
      }
    }

    val IDLE = new StateDelay(200) {
      whenCompleted {
        COMMAND := COMMAND.mux(
          SHOW_DISPLAY -> INCRE_MODE,
          INCRE_MODE   -> WRITE_DATA,
          WRITE_DATA   -> READ_DATA,
          READ_DATA    -> WRITE_DATA,
          default      -> WRITE_DATA
        )
        goto(START)
      }
    }
    setEncoding(binaryOneHot)
  }
}

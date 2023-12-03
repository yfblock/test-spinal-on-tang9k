package io.github.yfblock;

import spinal.core._
import spinal.lib._
import spinal.lib.fsm.StateMachine
import spinal.lib.fsm.State
import spinal.lib.io.TriState
import spinal.lib.fsm.EntryPoint

case class DSPort() extends Bundle with IMasterSlave {
  val rst = Bool()
  val clk = Bool()
  val dat = TriState(Bool())

  override def asMaster(): Unit = {
    out(rst)
    out(clk)
    master(dat)
  }

  override def setAsReg(): this.type = {
    this.clk.setAsReg().init(False)
    this.rst.setAsReg().init(False)
    this.dat.write.setAsReg().init(True)
    this.dat.writeEnable.setAsReg().init(True)
    this
  }
}

case class DSTimeBCD() extends Bundle {
  val year = Vec(UInt(4 bits))
  val month = Vec(UInt(1 bits), UInt(3 bits))
  val date = Vec(UInt(2 bits), UInt(4 bits))
  val days = UInt(3 bits)
}

object DS1302 {
  def apply(port: DSPort, tm: TimeDisplay): DS1302 = {
    val ds1302 = new DS1302();
    ds1302.io.port <> port
    ds1302.io.tm <> tm
    ds1302
  }
}

class DS1302 extends Component {
  val io = new Bundle {
    val port = master(DSPort())
    val tm   = out(TimeDisplay()).setAsReg()
  }

  io.port.setAsReg()
  noIoPrefix()

  val ds1302 = new Area {
    val stateCounter = Reg(UInt(2 bits)) init (0)
    val bitIndex     = Reg(UInt(3 bits)) init (0)

    val DATA    = Reg(UInt(8 bits)) init (0)
    val tmIndex = Reg(UInt(8 bits)) init (0)
    val ADDRESS = tmIndex |<< 1 | U(0x81, 8 bits)

    val state = new StateMachine {
      val START, WRITE_ADDRESS, READ, END = new State

      setEntry(START)

      START
        .whenIsActive {
          stateCounter := 0
          io.port.clk  := False
          io.port.rst  := True
          goto(WRITE_ADDRESS)
        }

      WRITE_ADDRESS
        .onEntry {
          bitIndex := 0
        }
        .whenIsActive {
          stateCounter      := stateCounter + 1
          io.port.dat.write := ADDRESS(bitIndex.resized)
          when(stateCounter === 1) {
            io.port.clk := True
          } elsewhen (stateCounter === 3) {
            bitIndex    := bitIndex + 1
            io.port.clk := False
            when(bitIndex === 7) {
              io.port.dat.writeEnable := False
              goto(READ)
            }
          }
        }

      READ
        .whenIsActive {
          stateCounter           := stateCounter + 1
          DATA(bitIndex.resized) := io.port.dat.read
          when(stateCounter === 2) {
            io.port.clk := True
            bitIndex    := bitIndex + 1
            when(bitIndex === 7) {
              io.port.dat.writeEnable := True
              goto(END)
            }
          } elsewhen (stateCounter === 3) {
            io.port.clk := False
          }
        }

      END
        .whenIsActive {
          io.port.rst := False
          switch(tmIndex) {
            is(0) {
              io.tm.rt4 := DATA(3 downto 0)
              io.tm.rt3 := DATA(6 downto 4).resized
              tmIndex   := 1
            }
            is(1) {
              io.tm.rt2 := DATA(3 downto 0)
              io.tm.rt1 := DATA(6 downto 4).resized
              tmIndex   := 0
            }
            default(tmIndex := 0)
          }
          goto(START)
        }

      setEncoding(binaryOneHot)
    }
  }
}

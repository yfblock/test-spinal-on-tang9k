package io.github.yfblock;

import spinal.core._
import spinal.lib._
import spinal.lib.fsm.StateMachine
import spinal.lib.fsm.State
import spinal.lib.io.TriState
import spinal.lib.fsm.EntryPoint
import spinal.lib.fsm.StateDelay

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

// This is a bundle to store date information with bcd.
case class DataStore() extends Bundle {
  val DATA = Vec.fill(8)(UInt(8 bits))

  def getSec0 = DATA(0)(3 downto 0)
  def getSec1 = DATA(0)(6 downto 4)
  def getMin0 = DATA(1)(3 downto 0)
  def getMin1 = DATA(1)(6 downto 4)
  // TIPS: This only be used for 24 hours format
  def getHour0 = DATA(2)(3 downto 0)
  def getHour1 = DATA(2)(4 downto 3)
  
  def getDay0 = DATA(3)(3 downto 0)
  def getDay1 = DATA(3)(5 downto 4)
  def getMonth0 = DATA(4)(3 downto 0)
  def getMonth1 = DATA(4)(4 downto 4)
  def getYear0 = DATA(6)(3 downto 0)
  def getYear1 = DATA(6)(7 downto 4)
}

/// read date time through burst mode
object DS1302 {
  def apply(port: DSPort, ds: DataStore): DS1302 = {
    val ds1302 = new DS1302();
    ds1302.io.port <> port
    ds1302.io.ds <> ds
    ds1302
  }
}

class DS1302 extends Component {
  val io = new Bundle {
    val port = master(DSPort())
    val ds   = out(DataStore()).setAsReg()
  }

  io.port.setAsReg()
  noIoPrefix()

  val ds1302 = new Area {
    val stateCounter = Reg(UInt(2 bits)) init (0)
    val bitIndex     = Reg(UInt(3 bits)) init (0)

    val tmIndex = Reg(UInt(3 bits)) init (0)
    val ADDRESS = U(0xbf, 8 bits)

    val state = new StateMachine {
      val START, WRITE_ADDRESS, READ, END = new State
      val WAIT                            = new StateDelay(5)

      setEntry(START)

      START
        .whenIsActive {
          stateCounter := 0
          io.port.clk  := False
          io.port.rst  := True
          goto(WRITE_ADDRESS)
        }

      WRITE_ADDRESS
        .onEntry(bitIndex := 0)
        .whenIsActive {
          stateCounter      := stateCounter + 1
          io.port.dat.write := ADDRESS(bitIndex)
          when(stateCounter(0)) {
            io.port.clk := ~stateCounter(1)
            when(stateCounter(1)) {
              bitIndex    := bitIndex + 1
              io.port.clk := False
              when(bitIndex === 7)(goto(READ))
            }
          }
        }

      READ
        .whenIsActive {
          io.port.dat.writeEnable := False
          stateCounter            := stateCounter + 1
          when(stateCounter === 2) {
            io.port.clk                   := True
            io.ds.DATA(tmIndex)(bitIndex) := io.port.dat.read
          } elsewhen (stateCounter === 3) {
            io.port.clk := False
            bitIndex    := bitIndex + 1
            when(bitIndex === 7)(goto(END))
          }
        }

      END
        .whenIsActive {
          tmIndex := tmIndex + 1
          goto(READ)
          when(tmIndex === 7) {
            io.port.dat.writeEnable := True
            io.port.rst             := False
            goto(WAIT)
          }
        }

      WAIT.whenCompleted {
        goto(START)
      }

      setEncoding(binaryOneHot)
    }
  }
}

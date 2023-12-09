package io.github.yfblock

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.io.TriState

case class I2CPort(outOnly: Boolean = false) extends Bundle with IMasterSlave {
    val scl = Bool()
    val sda = TriState(Bool())

    def asMaster(): Unit = {
        out(scl)
        master(sda)
    }
}

object SSD1306 {
    def apply(i2c: I2CPort): SSD1306 = {
        val ssd1306 = new SSD1306()
        ssd1306.io.port <> i2c
        ssd1306
    }
}

class SSD1306 extends Component {
    val io = new Bundle {
        val port = master(I2CPort())
    }

    noIoPrefix()

    object I2cCmdMode extends SpinalEnum{
        val START, SLAVE_ADDR, STOP, ACK, END = newElement()
    }

    val commandList = Vec.fill(9)(UInt(8 bits))
    commandList(0) := 0x78
    commandList(1) := 0x0
    commandList(2) := 0x8d
    commandList(3) := 0x0
    commandList(4) := 0x14
    commandList(5) := 0x0
    commandList(6) := 0xaf
    commandList(7) := 0x0
    commandList(8) := 0xa5
    val cIndex = Reg(UInt(4 bits)).init(0)
    val state = Reg(I2cCmdMode).init(I2cCmdMode.START)
    val step = Reg(UInt(2 bits)).init(0)
    // val command = Reg(UInt(8 bits)).init(0x78)
    val command = commandList(cIndex)
    val bitIndex = Reg(UInt(3 bits)).init(7)
    step := step + 1
    io.port.sda.writeEnable := True
    io.port.scl.setAsReg().init(True)
    io.port.sda.write.setAsReg().init(True)

    switch(state) {
        is(I2cCmdMode.START) {
            io.port.scl := True
            io.port.sda.write := ~step(1)
            when(step === 3) {
                state := I2cCmdMode.SLAVE_ADDR
            }
        }
        is(I2cCmdMode.SLAVE_ADDR) {
            io.port.scl := step(1)
            when(step === 1) {
                io.port.sda.write := command(bitIndex)
            }
            when(step === 3) {
                bitIndex := bitIndex - 1
                when(bitIndex === 0)(state := I2cCmdMode.ACK)
            }
        }
        is(I2cCmdMode.ACK) {
            io.port.scl := step(1)
            io.port.sda.writeEnable := False
            when(step === 3) {
                state := I2cCmdMode.SLAVE_ADDR
                cIndex := cIndex + 1
                when(cIndex === 8) {
                    state := I2cCmdMode.STOP
                }
            }
        }
        is(I2cCmdMode.STOP) {
            io.port.scl := True
            io.port.sda.write := step(1)
            when(step === 3)(state := I2cCmdMode.END)
        }
        is(I2cCmdMode.END) {

        }
    }
}

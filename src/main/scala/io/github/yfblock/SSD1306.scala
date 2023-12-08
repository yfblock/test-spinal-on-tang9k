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
        val START, SLAVE_ADDR, STOP, DROP, END = newElement()
    }

    val state = Reg(I2cCmdMode).init(I2cCmdMode.START)
    val step = Reg(UInt(2 bits)).init(0)
    val command = Reg(UInt(8 bits)).init(0)
    val bitIndex = Reg(UInt(3 bits)).init(0)
    step := step + 1
    io.port.sda.writeEnable := True
    io.port.sda.write := True
    io.port.scl := True

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
            // io.port.sda.write := 
            when(step === 3) {
                bitIndex := bitIndex + 1
                when(bitIndex === 7)(state := I2cCmdMode.DROP)
            }
        }
        is(I2cCmdMode.DROP) {

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

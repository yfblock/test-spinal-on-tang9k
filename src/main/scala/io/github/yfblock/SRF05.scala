package io.github.yfblock

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

case class SRF05Port() extends Bundle with IMasterSlave {
    val trig = Bool()
    val echo = Bool()

    def asMaster(): Unit = {
        out(trig)
        in(echo)
    }
}

object SRF05 {
    def apply(port: SRF05Port, distance: UInt): SRF05 = {
        val srf05 = new SRF05()
        srf05.io.port <> port
        srf05.io.distance <> distance
        srf05
    }
}

class SRF05 extends Component {
    val io = new Bundle {
        val port = master(SRF05Port())
        val distance = out(UInt(16 bits)).setAsReg().init(0)
    }
    io.port.trig := False

    val cnt = Reg(UInt(32 bits)) init (0)
    cnt := cnt + 1
    new StateMachine {
        val IDLE: State = new State with EntryPoint {
            onEntry(cnt := 0)
            whenIsActive {
                when(cnt === 135)(goto(SEND))
            }
        }

        val SEND = new State {
            whenIsActive {
                io.port.trig := True
                when(cnt === 450) {
                    goto(WAIT_END)
                }
            }
        }

        // val WAIT = new State {
        //     whenIsActive {
        //         when(io.port.echo === True){
        //             goto(WAIT_END)
        //         }
        //     }
        // }

        val WAIT_END = new State {
            onEntry(cnt := 0)
            whenIsActive {
                when(io.port.echo === False) {
                    // result is time * voice_speed(340m/s) / 2
                    // cnt is the number of voice time
                    // cnt / frequency * voice_speed(340m/s) / 2 * 1000(to make result unit as mm)
                    // cnt / 27000000 * 340 / 2 * 1000
                    // = cnt * 340 * 1000 / 54000000
                    // = cnt * 34 / 5400
                    // = cnt * 17 / 2700
                    // the equation below is optimized and unit is mm
                    // io.distance := (cnt * 17 / 2700).resized
                    io.distance := cnt.resized
                    goto(END)
                }
            }
        }

        val END = new State {
            whenIsActive {
                goto(IDLE)
            }
        }

    }
}

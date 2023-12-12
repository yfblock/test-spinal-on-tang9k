package io.github.yfblock

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

case class SRF04Port() extends Bundle with IMasterSlave {
    val trig = Bool()
    val echo = Bool()

    def asMaster(): Unit = {
        out(trig)
        in(echo)
    }
}

object SRF04 {
    def apply(port: SRF04Port): SRF04 = {
        val srf05 = new SRF04()
        srf05.io.port <> port
        srf05
    }
}

class SRF04 extends Component {
    val io = new Bundle {
        val port = master(SRF04Port())
        val distance = out(UInt(16 bits)).setAsReg().init(0)
    }
    io.port.trig := False

    def read(): UInt = io.distance

    val timer = Reg(UInt(32 bits)).init(0)
    def isTimeEnd = timer === 27_000_000 / 5
    timer := timer + 1
    when(isTimeEnd)(timer := 0)
    val cnt = Reg(UInt(21 bits)) init (0)
    cnt := cnt + 1
    
//    object FSMSTate extends SpinalEnum {
//        val IDLE, SEND, WAIT, WAIT_END = newElement()
//    }


    val fsm = new StateMachine {

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
                    goto(WAIT)
                }
            }
        }

         val WAIT = new State {
             whenIsActive {
                when(io.port.echo === True){
                     goto(WAIT_END)
                }
                when(isTimeEnd)(goto(IDLE))
             }
         }

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
                    io.distance := (cnt * 17 / 27000).resized
                    // io.distance := cnt.resized
                    goto(END)
                }
            }
        }

        val END = new State {
            whenIsActive {
                when(isTimeEnd){
                    goto(IDLE)
                }
//                goto(IDLE)
            }
        }

        always {
            when(isTimeEnd) {
                goto(IDLE)
            }
        }
    }
}

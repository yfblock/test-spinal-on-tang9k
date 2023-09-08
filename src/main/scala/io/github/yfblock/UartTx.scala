package io.github.yfblock

import spinal.core._
import spinal.lib.fsm.StateMachine
import spinal.lib.fsm.EntryPoint
import spinal.lib.fsm.State

class UartTx (baud_rate: Int) extends Component {
  val io = new Bundle {
    val tx_data = in UInt(8 bits);
    val tx_data_valid = in Bool();
    val tx_data_ready = out.Bool().setAsReg();
    val tx_pin = out Bool();
  }

  val CYCLE = this.clockDomain.frequency.getValue.toInt * 1000_000 / baud_rate;

  val tx_state = new StateMachine {
    val INIT_IDLE = new State with EntryPoint;
    val INIT_START = new State;
    val INIT_SEND_BYTE = new State;
    val INIT_STOP = new State;

    val cycle_cnt = Reg(UInt(16 bits)) init 0;
    val bit_cnt = Reg(UInt(3 bits)) init 0;
    val tx_data_latch = Reg(UInt(8 bits)) init 0;
    val tx_reg = Reg(Bool()) init False;

    INIT_IDLE.whenIsActive {
        when(io.tx_data_valid) {
            goto(INIT_START)
        }

        io.tx_data_ready := ~io.tx_data_valid;
    }

    INIT_START.whenIsActive {
        when(cycle_cnt === CYCLE - 1) {
            goto(INIT_SEND_BYTE)
        }
    }

    INIT_SEND_BYTE.whenIsActive {
        when(cycle_cnt === CYCLE - 1 && bit_cnt === 7) {
            goto(INIT_STOP)
        }
    }

    INIT_STOP.whenIsActive {
        when(cycle_cnt === CYCLE - 1) {
            goto(INIT_IDLE)
        }
    }
  }
}

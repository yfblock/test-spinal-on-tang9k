package io.github.yfblock

import spinal.core._
import spinal.lib.fsm._
import spinal.lib.io.ReadableOpenDrain
import spinal.lib.io.TriState

class TM1637 extends Component {
  val io = new Bundle {
    val clk      = out(Bool().setAsReg().init(True));
    val dio      = inout(Analog(Bool()));
    val tip_led  = out(Bool().setAsReg().init(True));
    val test_led = out(Bool().setAsReg().init(True));
  }
  val dio_tri_state = TriState(Bool());
  dio_tri_state.read := io.dio
  when(dio_tri_state.writeEnable) {
    io.dio := dio_tri_state.write
  }

  val data     = Reg(UInt(8 bits)) init (0);
  val bit_loop = Reg(UInt(4 bits)) init (0);
  val STEP     = Reg(UInt(3 bits)) init (0);
  dio_tri_state.write       := True;
  dio_tri_state.writeEnable := True;

  val state_machine = new StateMachine {
    val INIT = new State with EntryPoint {
      whenIsActive {
        io.clk := True;
        io.dio := True;
        when(STEP <= 2) {
          goto(START)
        }
      }
    }
    val START = new State {
      whenIsActive {
        when(io.clk) {
          dio_tri_state.write := False;
          goto(COMMAND)
        }
      }
    }
    val COMMAND = new State {
      onEntry {
        bit_loop := 0;
        data := STEP.mux(
          0       -> U"8'b01000000",
          1       -> U"8'b11000000",
          2       -> U"8'b10001111",
          default -> U"8'b10001111"
        )
      }
      whenIsActive {
        io.clk              := ~io.clk;
        io.tip_led          := ~io.clk;
        dio_tri_state.write := data(bit_loop.resized);
        when(io.clk === True) {
          when(bit_loop === 8) {
            goto(WAIT_ACK);
          } otherwise {
            bit_loop := bit_loop + 1;
          }
        }
      }
      onExit {
        bit_loop := 0;
      }
    }
    val WAIT_ACK = new State {
      whenIsActive {
        io.clk                    := ~io.clk;
        dio_tri_state.writeEnable := False;
        io.tip_led                := dio_tri_state.read;
        io.test_led               := False;
        when(dio_tri_state.read === False && io.clk === True) {
          // when(STEP === 0) {
          //   goto(END)
          // } otherwise {
          //   goto(WRITE_DATA)
          // }
          goto(END)
        }
      }
    }
    val END = new State {
      whenIsActive {
        io.clk              := True;
        dio_tri_state.write := False;
        when(io.clk === True) {
          when(bit_loop === 1) {
            STEP                := STEP + 1;
            dio_tri_state.write := True;
            exit()
          } otherwise {
            bit_loop := bit_loop + 1;
          }
        }
        // goto(START)
      }
    }
  }
}

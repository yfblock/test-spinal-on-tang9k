package io.github.yfblock

import spinal.core._

object Hex2BCD {
    def converBits(width: Int): Int = {
        val maxValue = Math.pow(2, width) - 1;
        String.valueOf(maxValue).length()
    }
}

case class Hex2BCD(inputWitdh: Int) extends Component {
  val io = new Bundle {
    val input = UInt(inputWitdh bits)
    val output = Vec.fill(Hex2BCD.converBits(inputWitdh))(UInt(4 bits))
  }

  
}

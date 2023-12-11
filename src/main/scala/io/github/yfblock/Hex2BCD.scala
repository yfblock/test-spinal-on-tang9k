package io.github.yfblock

import spinal.core._

import scala.language.postfixOps

object Hex2BCD {

    def apply(width: Int)(input: UInt): Hex2BCD = {
      val hex2bcd = new Hex2BCD(width)
      hex2bcd.io.input <> input
      hex2bcd
    }

    def convertBits(width: Int): Int = {
        val maxValue = Math.pow(2, width) - 1;
        String.valueOf(maxValue).length()
    }
}

class Hex2BCD(inputWitdh: Int) extends Component {
  val resultWidth = Hex2BCD.convertBits(inputWitdh)
  val io = new Bundle {
    val input = in(UInt(inputWitdh bits))
    val output = out(Vec.fill(resultWidth)(UInt(4 bits)))
  }

  for(i <- 0 until Hex2BCD.convertBits(inputWitdh)) {
//    io.output(i) := io.input % U(Math.pow(10, i).toInt, resultWidth bits)
    io.output(i) := (io.input / Math.pow(10, i).toInt  % Math.pow(10, i + 1).toInt).resized
  }

  def output = io.output
  
}

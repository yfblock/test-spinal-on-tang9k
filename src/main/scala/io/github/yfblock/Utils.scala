package io.github.yfblock

import spinal.core.Vec
import spinal.core.UInt

object Utils {
  
}

object VecUtil {
    def initWith[T <: UInt](v: Vec[T], arr: Array[T]) {
        for(i <- 0 until arr.length) {
            v(i) := arr(i)
        }
    }
}

package io.github.iltotore.ball

import doodle.core.Vec

trait MathOps {

  extension (vec: Vec) {

    def *(other: Vec): Vec = Vec(vec.x * other.x, vec.y * other.y)
  }

  extension (x: Double) {

    inline def **(inline n: Int): Double =
      if (n == 0) 1.0
      else if (n % 2 == 1) x * x ** (n - 1)
      else (x * x) ** (n / 2)
  }
}

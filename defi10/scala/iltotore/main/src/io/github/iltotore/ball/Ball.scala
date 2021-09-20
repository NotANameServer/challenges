package io.github.iltotore.ball

import doodle.core.{Point, Vec}

case class Ball(position: Point, velocity: Vec, radius: Double) {
  
  inline def mass: Double = radius
}

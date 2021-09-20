package io.github.iltotore.ball

import doodle.core.Vec

case class Collision(vector: Vec, velocity: Vec, mass: Double)

object Collision {
  
  def fixed(vector: Vec, mass: Double = 999): Collision = Collision(vector, Vec.zero, mass)
}

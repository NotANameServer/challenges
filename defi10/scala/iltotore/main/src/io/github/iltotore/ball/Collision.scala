package io.github.iltotore.ball

import doodle.core.Vec

/**
 * A collision against an entity.
 * @param vector the collision/force vector
 * @param velocity the velocity of the colliding object
 * @param mass the mass of the colliding object
 */
case class Collision(vector: Vec, velocity: Vec, mass: Double, minDistance: Double)

object Collision {

  /**
   * A collision against an immobile, heavy-weight wall.
   * @param vector the collision/force vector
   * @param mass the mass of the colliding object (999 by default)
   * @return the resulting Collision object
   */
  def fixed(vector: Vec, minDistance: Double, mass: Double = 999): Collision = Collision(vector, Vec.zero, mass, minDistance)
}

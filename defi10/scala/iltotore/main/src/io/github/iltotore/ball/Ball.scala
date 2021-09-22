package io.github.iltotore.ball

import doodle.core.{Color, Point, Vec}

/**
 * Represent a ball.
 * @param position the ball position
 * @param velocity the ball velocity vector
 * @param radius the ball radius
 * @param color the color of the ball
 */
case class Ball(position: Point, velocity: Vec, radius: Double, color: Color) {

  /**
   * The mass of the ball.
   * @return the mass (radius by default)
   */
  inline def mass: Double = radius
}
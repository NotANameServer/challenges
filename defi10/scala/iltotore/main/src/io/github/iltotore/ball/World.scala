package io.github.iltotore.ball

import doodle.core.BoundingBox

/**
 * The main data model.
 * @param balls the bouncing balls of this world
 * @param zone the dimensions of this world
 */
case class World(balls: Iterable[Ball], zone: BoundingBox)
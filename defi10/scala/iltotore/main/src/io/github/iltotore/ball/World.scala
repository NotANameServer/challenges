package io.github.iltotore.ball

import doodle.core.BoundingBox

case class World(balls: Iterable[Ball], zone: BoundingBox)
package io.github.iltotore.ball

import doodle.image._
import doodle.image.syntax._
import doodle.core._

trait ToImage[-A] {

  def toImage(entity: A): Image
}

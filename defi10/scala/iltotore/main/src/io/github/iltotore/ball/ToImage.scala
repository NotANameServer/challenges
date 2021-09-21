package io.github.iltotore.ball

import doodle.image._
import doodle.image.syntax._
import doodle.core._

trait ToImage[-A] {

  /**
   * Render the given entity.
   * @param entity the entity to render
   * @return the rendering result of this entity as Image
   */
  def toImage(entity: A): Image
}

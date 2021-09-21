package io.github.iltotore.ball

trait Ticker[A] {

  /**
   * Tick the given entity.
   * @param entity the entity to update
   * @return an updated version of the given entity
   */
  def tick(entity: A): A
}
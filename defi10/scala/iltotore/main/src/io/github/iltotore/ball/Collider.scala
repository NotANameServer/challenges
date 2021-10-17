package io.github.iltotore.ball

trait Collider[A] {

  /**
   * Calculate collision on the given entity.
   * @param entity the entity to collide on
   * @param collision the collision info
   * @return a new version of this entity after colliding
   */
  def collide(entity: A, collision: Collision): A

}

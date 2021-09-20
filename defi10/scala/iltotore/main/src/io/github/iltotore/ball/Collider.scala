package io.github.iltotore.ball

trait Collider[A] {

  def collide(entity: A, collision: Collision): A

}

package io.github.iltotore.ball

import doodle.core.BoundingBox
import doodle.image.Image

trait TypeOps {

  extension[A] (entity: A)(using toImg: ToImage[A]) {

    def toImage: Image = toImg.toImage(entity)
  }

  extension[A] (entity: A)(using ticker: Ticker[A]) {

    def tick: A = ticker.tick(entity)
  }

  extension[A] (entity: A)(using collider: Collider[A]) {

    def collide(collision: Collision): A = collider.collide(entity, collision)
  }
}

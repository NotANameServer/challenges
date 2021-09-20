package io.github.iltotore

import doodle.core.*
import doodle.image.Image

package object ball extends TypeOps with MiscOps with MathOps {

  given ToImage[Ball] with {

    override def toImage(entity: Ball): Image =
      Image
        .circle(entity.radius*2)
        .fillColor(Color.red)
        .at(entity.position)
  }

  given (using ToImage[Ball]): ToImage[World] with {

    override def toImage(entity: World): Image =
      entity
        .balls
        .map(_.toImage)
        .reduce(_.on(_))
  }

  given Ticker[Ball] with {

    override def tick(entity: Ball): Ball = {
      entity.copy(position = entity.position + entity.velocity)
    }
  }

  given Ticker[World] with {

    def collideWithEntities(ball: Ball, others: Iterable[Ball]): Ball =
      others.find(b => b != ball && (b.position - ball.position).length <= b.radius + ball.radius) match {

        case Some(other) => ball.collide(Collision(ball.position - other.position, other.velocity, other.mass))

        case None => ball
      }

    override def tick(entity: World): World = {
      entity.copy(
        balls = entity
          .balls
          .map(collideWithEntities(_, entity.balls))
          .mapIf(b => b.position.x-b.radius <= entity.zone.left)(b => b.collide(Collision.fixed(Vec.unitX * -b.radius)))
          .mapIf(b => b.position.x+b.radius >= entity.zone.right)(b => b.collide(Collision.fixed(Vec.unitX * b.radius)))
          .mapIf(b => b.position.y-b.radius <= entity.zone.bottom)(b => b.collide(Collision.fixed(Vec.unitY * -b.radius)))
          .mapIf(b => b.position.y+b.radius >= entity.zone.top)(b => b.collide(Collision.fixed(Vec.unitY * b.radius)))
          .map(_.tick)
      )
    }
  }

  given Collider[Ball] with {

    override def collide(entity: Ball, collision: Collision): Ball = {
      val massCoef = (2 * collision.mass / (entity.mass + collision.mass))
      val velocityCoef = ((entity.velocity - collision.velocity).dot(collision.vector)) / Math.pow((collision.vector).length, 2)
      val bounce = entity.velocity - (collision.vector * massCoef * velocityCoef)
      entity.copy(position = entity.position + bounce, velocity = bounce)
    }

  }
}
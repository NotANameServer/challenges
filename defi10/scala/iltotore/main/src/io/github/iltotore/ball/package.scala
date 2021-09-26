package io.github.iltotore

import doodle.core.*
import doodle.image.Image

package object ball extends TypeOps with MiscOps {

  given ToImage[Ball] with {

    override def toImage(entity: Ball): Image =
      Image
        .circle(entity.radius*2)
        .fillColor(entity.color)
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
      entity.copy(
        position = entity.position + entity.velocity, //Application de la vélocité
        velocity = entity.velocity - Vec(0, 0.5) //Gravité
      )
    }
  }

  given Ticker[World] with {

    def collideWithEntities(ball: Ball, others: Iterable[Ball]): Ball =
      others.find(b => b != ball && (b.position - ball.position).length <= b.radius + ball.radius) match {

        case Some(other) => ball.collide(Collision(ball.position - other.position, other.velocity, other.mass, ball.radius + other.radius))

        case None => ball
      }

    override def tick(entity: World): World = {
      entity.copy(
        balls = entity
          .balls
          .map(collideWithEntities(_, entity.balls))
          .mapIf(b => b.position.x-b.radius <= entity.zone.left)(b =>
            b.collide(Collision.fixed(Vec.unitX * (b.position.x-entity.zone.left), b.radius))
          )
          .mapIf(b => b.position.x+b.radius >= entity.zone.right)(b =>
            b.collide(Collision.fixed(Vec.unitX * (b.position.x-entity.zone.right), b.radius))
          )
          .mapIf(b => b.position.y-b.radius <= entity.zone.bottom)(b =>
            b.collide(Collision.fixed(Vec.unitY * (b.position.y-entity.zone.bottom), b.radius))
          )
          .mapIf(b => b.position.y+b.radius >= entity.zone.top)(b =>
            b.collide(Collision.fixed(Vec.unitY * (b.position.y-entity.zone.top), b.radius))
          )
          .map(_.tick)
      )
    }
  }

  given Collider[Ball] with {

    override def collide(entity: Ball, collision: Collision): Ball = {
      val massCoef = (2 * collision.mass / (entity.mass + collision.mass))
      val velocityCoef = ((entity.velocity - collision.velocity).dot(collision.vector)) / Math.pow((collision.vector).length, 2)
      val bounce = entity.velocity - (collision.vector * massCoef * velocityCoef)
      val distance = collision.minDistance - collision.vector.length
      val movement = collision.vector*(distance/collision.vector.length)
      entity.copy(position = entity.position + movement, velocity = bounce)
    }

  }
}
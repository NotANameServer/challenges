package io.github.iltotore.ball

trait Ticker[A] {

  def tick(entity: A): A
}
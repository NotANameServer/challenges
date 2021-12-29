package io.github.iltotore.justeprix

import scala.util.Random

case class MysteryPrice(price: Int) {

  def compareTo(guess: Int): Int = price.compareTo(guess)
}

object MysteryPrice {

  def random(max: Int): MysteryPrice = MysteryPrice(Random.nextInt(max))
}
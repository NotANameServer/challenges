package io.github.iltotore.justeprix

case class Play(min: Int, max: Int) {

  val guess: Int = (max+min) / 2

  def next(result: Int): Play = result match {
    case 1 => Play(guess, max)
    case -1 => Play(min, guess)
  }
}
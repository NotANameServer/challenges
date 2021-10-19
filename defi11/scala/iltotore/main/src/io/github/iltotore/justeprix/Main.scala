package io.github.iltotore.justeprix

import scala.util.control.Breaks._


object Main extends App {

  val price = MysteryPrice.random(1000001) //1 million 1 exclus, ce qui revient à 1 million
  var play = Play(0, 1000001)
  var result = -1

  breakable {
    for (i <- 0 until 50) {
      println(s"Tentative $i: ${play.guess}")
      result = price.compareTo(play.guess)
      if (result == 0) break()
      if (result == 1) println("Plus grand")
      else println("Plus petit")
      play = play.next(result)
    }
  }

  if (result == 0) println(s"Le bot a gagné ! La réponse était: $price")
  else println(s"Perdu. La réponse était: $price")
}

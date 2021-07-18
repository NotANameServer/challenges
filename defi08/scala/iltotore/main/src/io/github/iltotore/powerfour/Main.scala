package io.github.iltotore.powerfour

import io.github.iltotore.powerfour.game.{Game, Grid, Player, UI}

import scala.io.StdIn

object Main extends App {

  val ui = new UI.TextBased

  val colors = Map(
    "red" -> Console.RED,
    "blue" -> Console.BLUE,
    "green" -> Console.GREEN,
    "magenta" -> Console.MAGENTA
  )

  val playerCount =
    Eithers.waitRight(Eithers.readInt("Combien de participants ?", "Nombre invalide"))(ui.sendMessage)

  val players = for (i <- 1 to playerCount) yield {
    val name = StdIn.readLine(s"Joueur $i: Quel-est votre nom ?")
    val color = Eithers.waitRight(
      colors.get(StdIn.readLine(s"Joueur $i: Quelle-est votre couleur ?"))
        .toRight("Couleur invalide")
    )(ui.sendMessage)

    if(StdIn.readLine(s"Joueur $i: Êtes-vous un humain ? true/false (par défaut: Oui)") equalsIgnoreCase "false")
      Player.NaiveAI(name, color)
    else Player.Human(name, color)
  }

  val grid = Grid.empty(7, 6)

  var game = new Game(players, grid, ui)
  val end = Eithers.waitRight(game.play.toRight("===== Tour suivant ====="))(ui.sendMessage)
  println(end)
}
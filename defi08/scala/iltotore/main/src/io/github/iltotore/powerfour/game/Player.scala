package io.github.iltotore.powerfour.game

import io.github.iltotore.powerfour.Eithers.readInt
import io.github.iltotore.powerfour.Eithers

trait Player {

  def name: String

  def color: String

  def play(grid: Grid, ui: UI): (Int, Int)
}

object Player {

  type Color = String

  case class Human(name: String, color: String) extends Player {

    /**
     * A functional, idiomatic way to handle user input.
     * @param grid the Grid played on
     * @return the first valid input
     */
    private def input(grid: Grid): Either[String, (Int, Int)] = for {
      choice <- readInt(s"$name: Choisissez un nombre entre 1 et ${grid.columns}", "Nombre invalide.")
      column <- Either.cond(choice > 0 && choice <= grid.columns, choice, s"La colonne $choice n'existe pas.")
      result <- grid.findLocation(column-1).toRight(s"La colonne $choice est pleine.")
    } yield result

    override def play(grid: Grid, ui: UI): (Int, Int) = Eithers.waitRight(input(grid))(ui.sendMessage)
  }
}
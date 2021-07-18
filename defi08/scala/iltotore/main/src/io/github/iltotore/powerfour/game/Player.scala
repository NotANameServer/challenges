package io.github.iltotore.powerfour.game

import io.github.iltotore.powerfour.Eithers.readInt
import io.github.iltotore.powerfour.Eithers

import scala.util.Random
import scala.util.control.Breaks.{break, breakable}

/**
 * Represent a Player (human or machine)
 */
trait Player {

  def name: String

  def color: String

  def play(grid: Grid, ui: UI): (Int, Int)
}

object Player {

  type Color = String

  /**
   * A human player.
   * @param name the player name
   * @param color the player color
   */
  case class Human(name: String, color: String) extends Player {

    /**
     * A functional, idiomatic way to handle user input.
     *
     * @param grid the Grid played on
     * @return the first valid input
     */
    private def input(grid: Grid): Either[String, (Int, Int)] = for {
      choice <- readInt(s"$name: Choisissez un nombre entre 1 et ${grid.columns}", "Nombre invalide.")
      column <- Either.cond(choice > 0 && choice <= grid.columns, choice, s"La colonne $choice n'existe pas.")
      result <- grid.findLocation(column - 1).toRight(s"La colonne $choice est pleine.")
    } yield result

    override def play(grid: Grid, ui: UI): (Int, Int) = Eithers.waitRight(input(grid))(ui.sendMessage)
  }


  /**
   * A naive AI. This AI only takes care of its own color without counter-playing or predict.
   * @param name the player name
   * @param color the player color
   */
  case class NaiveAI(name: String, color: String) extends Player {

    override def play(grid: Grid, ui: UI): (Int, Int) = {
      var choice = (-1, (0, 0))
      for (col <- 0 until grid.columns) breakable {
        val virtualGrid = grid.copy
        val location = virtualGrid.findLocation(col)
        if(location.isEmpty) break()
        val (x, y) = location.get
        virtualGrid.update(x, y, Some(color))
        val possibilities = for (dx <- -1 to 1; dy <- -1 to 1 if dx != 0 || dy != 0) yield {
          grid.getPower(x, y, dx, dy, 4)
        }

        val bestChoice = possibilities.max
        if(choice._1 == bestChoice && Random.nextBoolean()) choice = (bestChoice, (x, y))
        if (choice._1 < bestChoice) choice = (bestChoice, (x, y))
      }
      choice._2
    }
  }
}
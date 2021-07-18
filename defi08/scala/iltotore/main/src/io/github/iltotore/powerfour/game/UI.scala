package io.github.iltotore.powerfour.game

/**
 * Represent a user interface
 */
trait UI {

  /**
   * Send a message to the user.
   * @param msg the message to broadcast
   */
  def sendMessage(msg: String): Unit

  /**
   * Render the given Grid.
   * @param grid the Grid instance to be rendered
   */
  def showGrid(grid: Grid): Unit
}

object UI {

  /**
   * A simple TUI
   */
  class TextBased extends UI {

    override def sendMessage(msg: String): Unit = println(msg)

    override def showGrid(grid: Grid): Unit = {
      for (y <- 0 until grid.rows) {
        val row = grid.row(y)
          .map(_.getOrElse(Console.RESET) + "X")
          .mkString("", Console.RESET + " ", Console.RESET)
        sendMessage(row)
      }
      sendMessage(Range(1, 8).mkString(Console.WHITE, " ", Console.RESET))
    }
  }
}
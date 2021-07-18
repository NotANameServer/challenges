package io.github.iltotore.powerfour.game

import scala.Console.RESET

class Game(players: Seq[Player], grid: Grid, ui: UI) {

  /**
   * Play a turn.
   * @return the finish message (draw or player) or None
   */
  def play: Option[Player.Color] = {
    for(player <- players) {
      val (x, y) = player.play(players.map(_.color).toSet, grid, ui)
      grid.update(x, y, Some(player.color))
      ui.showGrid(grid)
      if(grid.isFinished) return Some(s"${player.color}${player.name}$RESET a gagné la partie.")
      if(grid.isFull) return Some(s"Égalité.")
    }
    None
  }
}
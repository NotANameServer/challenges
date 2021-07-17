package io.github.iltotore.powerfour.game

import scala.collection.mutable

/**
 * A 2D Grid
 *
 * @param impl    the mutable.Seq used by this Grid
 * @param columns the column count
 * @param rows    the row count
 */
class Grid(impl: mutable.Seq[Option[Player.Color]], val columns: Int, val rows: Int) extends mutable.Seq[Option[Player.Color]] {

  //Delegated methods to `impl

  override def update(idx: Int, elem: Option[Player.Color]): Unit = impl.update(idx, elem)

  override def apply(i: Int): Option[Player.Color] = impl.apply(i)

  override def length: Int = impl.length

  override def iterator: Iterator[Option[Player.Color]] = impl.iterator

  //Mathematical methods for the grid

  /**
   * Get 2D coords from the Seq index.
   *
   * @param i the Seq index
   * @return the 2D translation of i
   */
  def get2dCoords(i: Int): (Int, Int) = (i % columns, i / columns)

  /**
   * Get index from the 2D coords.
   *
   * @param x the x (column axis)
   * @param y the y (row axis)
   * @return the 1D translation of (x, y)
   */
  def getIndex(x: Int, y: Int): Int = x + y * columns

  /**
   * Get the element using its 2D coords.
   *
   * @param x the x (column axis)
   * @param y the y (row axis)
   * @return the element at (x, y)
   */
  def apply(x: Int, y: Int): Option[Player.Color] = apply(getIndex(x, y))

  /**
   * Get the element using its 2D coords.
   *
   * @param x    the x (column axis)
   * @param y    the y (row axis)
   * @param elem the new element
   * @return the element at (x, y)
   */
  def update(x: Int, y: Int, elem: Option[Player.Color]): Unit = impl.update(getIndex(x, y), elem)

  /**
   * Get the column at x
   *
   * @param x the x (column axis)
   * @return the elements at (x, ?)
   */
  def column(x: Int): Seq[Option[Player.Color]] = for (y <- 0 until rows) yield apply(x, y)

  /**
   * Get the row at y
   *
   * @param y the y (row axis)
   * @return the elements at (?, y)
   */
  def row(y: Int): Seq[Option[Player.Color]] = for (x <- 0 until columns) yield apply(x, y)

  /**
   * Check if the column at x exists.
   *
   * @param x the x (column axis)
   * @return true if the column exists
   */
  def containsColumn(x: Int): Boolean = x >= 0 && x < columns

  /**
   * Check if the row at y exists.
   *
   * @param y the y (row axis)
   * @return true if the row exists
   */
  def containsRow(y: Int): Boolean = y >= 0 && y < rows

  //Power4-related methods

  /**
   * Find the final location when "dropping" an element at x.
   *
   * @param x the x (column axis)
   * @return the last empty location of this column
   */
  def findLocation(x: Int): Option[(Int, Int)] = {
    val y = column(x).lastIndexOf(None) //Readability was preferred over optimization
    Option.unless(y == -1)((x, y))
  }

  /**
   * Check if the given point is aligned with 3 other cells with the same color.
   *
   * @param x the x (column axis)
   * @param y the y (row axis)
   * @param dx the direction on column axis
   * @param dy the direction on row axis
   * @param length the minimum line length (4 in Power4)
   * @return true if a line of size >= `length` pass trough the point at (x, y)
   */
  def isAligned(x: Int, y: Int, dx: Int, dy: Int, length: Int): Boolean = {
    val color = apply(x, y)
    if (color.isEmpty) return false
    var power = 0
    for (i <- 0 until length) {
      val cellX = x + dx * i
      val cellY = y - dy * i
      if (containsColumn(cellX) && containsRow(cellY) && apply(cellX, cellY).equals(color)) {
        power += 1
      } else power = 0
    }
    power == length
  }

  /**
   * Check if the game is finished (excluding draw).
   * @return true if a player won the game
   */
  def isFinished: Boolean = {
    for (x <- 0 until columns; y <- 0 until rows) {
      if (
        isAligned(x, y, 1, 0, 4) ||
          isAligned(x, y, 0, 1, 4) ||
          isAligned(x, y, 1, 1, 4) ||
          isAligned(x, y, -1, 1, 4)
      ) return true
    }
    false
  }

  /**
   * Check if the grid is full.
   * @return true if all slots have a color
   */
  def isFull: Boolean = !this.contains(None)

  /**
   * Return a copy of this Grid.
   * @return a value-based copy of this Grid
   */
  def copy: Grid = new Grid(mutable.Seq.from(impl), columns, rows)
}

object Grid {

  /**
   * Create an empty grid of the given dimensions.
   * @param columns the column count
   * @param rows the row count
   * @return an empty grid of size columns*rows
   */
  def empty(columns: Int, rows: Int): Grid = {
    val impl: Seq[Option[Player.Color]] = for (x <- 0 until columns; y <- 0 until rows) yield None
    new Grid(impl.to(mutable.Seq), columns, rows)
  }
}
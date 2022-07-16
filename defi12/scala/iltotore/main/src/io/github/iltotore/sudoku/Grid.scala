package io.github.iltotore.sudoku

import scala.collection.mutable

/**
 * Represent a bidimensional grid.
 *
 * @param seq    the inner slot collection
 * @param width  the grid width
 * @param height the grid height
 * @tparam A the type of a slot
 */
case class Grid[A](seq: Seq[A], width: Int, height: Int):

  export seq.contains

  export seq.forall

  /**
   * Get the value at the given coordinates. Analogous to [[Seq.apply]] but with 2D coordinates.
   *
   * @param x the X coordinate
   * @param y the Y coordinate
   * @return the value at the given coordinates
   */
  def apply(x: Int, y: Int): A =
    if x < 0 || x > width || y < 0 || y > height then throw IndexOutOfBoundsException(s"Coords: ($x, $y), Dim: $width*$height")
    seq(x + y * height)

  /**
   * Get the value using the raw index. Similar to [[Seq.apply]].
   *
   * @param i the index of the value to get
   * @return the value at the given index
   */
  def fromIndex(i: Int): A = seq(i)

  /**
   * Get the next position in the grid.
   *
   * @param x the X coordinate of the preceding position
   * @param y the Y coordinate of the preceding position
   * @return the position next to (x, y), taking into account this Grid's width and height
   */
  def nextPos(x: Int, y: Int): (Int, Int) = ((x + 1) % width, if x + 1 >= width then y + 1 else y)

  /**
   * Get a specific column.
   *
   * @param x the abs of the column to get
   * @return a Seq containing all slots with the same X coordinate
   */
  def column(x: Int): Seq[A] = for y <- 0 until height yield apply(x, y)

  /**
   * Get all columns.
   *
   * @return this Grid as a Seq of columns
   */
  def columns: Seq[Seq[A]] = for x <- 0 until width yield column(x)

  /**
   * Get a specific line.
   *
   * @param y the ordinate of the line to get
   * @return a Seq containing all slots with the same Y coordinate
   */
  def line(y: Int): Seq[A] = for x <- 0 until width yield apply(x, y)

  /**
   * Get all lines.
   *
   * @return this Grid as a Seq of lines
   */
  def lines: Seq[Seq[A]] = for y <- 0 until height yield line(y)

  /**
   * Select a slice of this Grid. Analogous to [[Seq.slice]] but using 2D coordinates.
   *
   * @param startX the lowest X to include from this grid
   * @param startY the lowest Y to include from this grid
   * @param endX   the lowest X to exclude from this grid
   * @param endY   the lowest Y to exclude from this grid
   * @return another grid, with its lowest corner being (startX, startY) and its highest corner being (endX-1, endY-1)
   */
  def slice(startX: Int, startY: Int, endX: Int, endY: Int): Grid[A] =
    val arr =
      for
        y <- startY until endY
        x <- startX until endX
      yield apply(x, y)

    Grid(arr, endX - startX, endY - startY)
  end slice

  /**
   * Get the Sudoku-like square containing the given coordinates.
   *
   * @param x the X coordinate of the slot to test
   * @param y the Y coordinate of the slot to test
   * @return a 3x3 grid containing the given slot
   */
  def squareAt(x: Int, y: Int): Grid[A] =
    val startX = x / 3 * 3
    val startY = y / 3 * 3

    slice(startX, startY, startX + 3, startY + 3)
  end squareAt

  /**
   * Change a specific slot of a Grid. Analogous to [[Seq.appended]] but with 2D coordinates.
   *
   * @param x     the X coordinate of the slot to change
   * @param y     the Y coordinate of the slot to change
   * @param value the new value to set to
   * @return a new Grid with the given slot updated
   */
  def withSlot(x: Int, y: Int, value: A): Grid[A] =
    if x < 0 || x > width || y < 0 || y > height then throw IndexOutOfBoundsException(s"Coords: ($x, $y), Dim: $width*$height")
    copy(seq = seq.updated(x + y * height, value))

  /**
   * Map all values using a function. Similar to [[Seq.map]]
   *
   * @param f the function to apply to all values
   * @tparam B the new slot type
   * @return a new Grid with the function applied to
   */
  def map[B](f: A => B): Grid[B] = copy(seq = seq.map(f))

  override def toString: String =

    val strColumns = Range(0, width).mkString("|")
    val strLines =
      Range(0, height)
        .zip(lines)
        .map { case (i, line) => s"$i  ${line.mkString("|")}" }
        .mkString("\n")

    s"""   $strColumns
       :
       :$strLines""".stripMargin(':')
  end toString

end Grid

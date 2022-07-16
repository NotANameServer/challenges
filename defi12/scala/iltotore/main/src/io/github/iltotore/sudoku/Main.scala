package io.github.iltotore.sudoku

import java.util.stream.Collectors

object Main {

  type SudokuGrid = Grid[Option[Int]] //A Sudoku grid contains a int or nothing (empty slot)
  type IndexGrid = Grid[Seq[Int]] //An index keeping track of possible solutions for each slot

  /**
   * Check if the given grid is valid.
   * @param grid the grid to test
   * @return true if the grid is a valid Sudoku grid (a uncompleted grid can still be valid)
   */
  def isValid(grid: SudokuGrid): Boolean =
    val coords =
      for
        y <- 0 until grid.height
        x <- 0 until grid.width
      yield (x, y)
      
    coords.forall { (x, y) =>
      grid(x, y) match
        case None => true
        case value =>
          val withoutSlot = grid.withSlot(x, y, None) //Do not take the current slot into account
          !withoutSlot.line(y).contains(value) && !withoutSlot.column(x).contains(value) && !withoutSlot.squareAt(x, y).contains(value)
    }
  end isValid

  /**
   * Try to solve a slot.
   *
   * @param grid  the current sudoku grid
   * @param index the current solution index
   * @param x     the x coordinate of the analyzed slot
   * @param y     the y coordinate of the analyzed slot
   * @return a Some[(SudokuGrid, IndexGrid)] if the grid or the index changed, None otherwise.
   */
  def solveSlot(grid: SudokuGrid, index: IndexGrid, x: Int, y: Int): Option[(SudokuGrid, IndexGrid)] =
    if grid(x, y).isDefined then Option.unless(index(x, y).isEmpty)((grid, index.withSlot(x, y, Seq.empty)))
    else
      val remaining =
        Range.inclusive(1, 9).toSet -- grid.line(y).flatten -- grid.column(x).flatten -- grid.squareAt(x, y).seq.flatten

      remaining.size match

        case 0 =>
          throw IllegalArgumentException("Unsolvable sudoku")

        case 1 => Some((grid.withSlot(x, y, remaining.headOption), index.withSlot(x, y, Seq.empty)))

        case _ =>
          val onlyPlaceableHere = remaining -- index.line(y).flatten -- index.column(x).flatten -- index.squareAt(x, y).seq.flatten

          onlyPlaceableHere.size match
            case 1 => Some((grid.withSlot(x, y, onlyPlaceableHere.headOption), index.withSlot(x, y, Seq.empty)))

            case _ =>
              Option.unless(remaining.equals(index(x, y).toSet))((grid, index.withSlot(x, y, remaining.toSeq)))

  end solveSlot

  /**
   * Solve (partially or completely) the given grid using classical Sudoku constraints.
   *
   * @param grid the current Sudoku grid
   * @return the (partially) solved grid
   */
  def solveByConstraint(grid: SudokuGrid): SudokuGrid =
    var result = grid
    var indexGrid: IndexGrid = grid.map(_ => Range.inclusive(1, 9))
    var changed = true
    while !result.forall(_.isDefined) && changed do
      changed = false
      for
        y <- 0 until result.height
        x <- 0 until result.width
        (res, index) <- solveSlot(result, indexGrid, x, y)
      do
        changed = true
        result = res
        indexGrid = index
    result

  /**
   * Try to solve the given grid using backtracking.
   *
   * @param grid the grid to solve
   * @param x    the x coordinate to start from
   * @param y    the y coordinate to start from
   * @return the solved grid or None if unsolvable
   */
  def solveRecursively(grid: SudokuGrid, x: Int, y: Int): Option[SudokuGrid] =
    val next = grid.nextPos(x, y)
    if y >= grid.height then Some(grid)
    else if grid(x, y).isDefined then solveRecursively(grid, next._1, next._2)
    else
      for
        candidate <- Range.inclusive(1, 9).toSet -- grid.line(y).flatten -- grid.column(x).flatten -- grid.squareAt(x, y).seq.flatten
      do
        solveRecursively(grid.withSlot(x, y, Some(candidate)), next._1, next._2) match
          case None =>
          case result => return result

      None
  end solveRecursively

  /**
   * Solve the given grid. Begin to partially solve the grid by constraint then use backtracking.
   *
   * @param grid the grid to solve
   * @return the solved grid
   */
  def solve(grid: SudokuGrid): SudokuGrid =
    val byConstraint = solveByConstraint(grid)
    solveRecursively(byConstraint, 0, 0).getOrElse(throw IllegalArgumentException("Unresolvable sudoku"))

  /**
   * Pretty print the given Sudoku grid.
   *
   * @param grid
   */
  def printSudoku(grid: SudokuGrid): Unit = println(grid.map(_.map(_.toString).getOrElse(" ")))

  @main def test =
    val sudoku = //Sudoku sample. Feel free to test the project with other grids
      """7   1   9
        |      41 
        |5 9  6  7
        |        2
        |18    9 5
        |     5 8 
        |  284  9 
        | 6    158
        |   6     """.stripMargin

    //Parse the grid
    val slots = sudoku.replaceAll("[^ \\d]", "").map(_.toString.toIntOption)
    val grid = Grid(slots, 9, 9)

    println(s"Valid: ${isValid(grid)}")
    printSudoku(solve(grid))

  end test
}

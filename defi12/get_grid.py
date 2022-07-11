"""
Example python module featuring a function to extract a grid from the
sudoku.txt file. Adapt it for your own language.
"""

def get_grid(grid_no: int) -> list[list[str]]:
    """
    Extract a grid from the sudoku.txt file.

    :param int grid_no: the grid to extract, 1-indexed
    :return: a 2d array where each value is a string.
    """
    with open('sudoku.txt') as file:
        file.seek(108 * (grid_no - 1) + 9)
        grid_txt = file.read(89)

    return [list(line) for line in grid_txt.splitlines()]
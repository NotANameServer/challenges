#!/usr/bin/env python3

import itertools
import sys
import fileinput


WIDTH = None
HEIGHT = None
PATH = " "
WALL = "#"
PEBBLE = "."


def breadth_first_search(grid, startpos):

    class TreeNode:
        def __init__(self, parent, value):
            self.parent = parent
            self.value = value

    stage = [TreeNode(None, startpos)]

    while stage is not None:
        next_stage = []

        for leaf in stage:
            x, y = curpos = leaf.value

            if is_at_exit(curpos):
                # Solution found
                stack = []
                while leaf is not None:
                    stack.append(leaf.value)
                    leaf = leaf.parent
                stack.reverse()
                return stack

            for dx, dy in iter(lambda: available_direction(grid, curpos), None):
                # Add every possible next position to the next stage
                grid[y + dy][x + dx] = PEBBLE
                grid[y + dy * 2][x + dx * 2] = PEBBLE
                stage.append(
                    TreeNode(
                        TreeNode(leaf, (x + dx, y + dy)),  # The intermediary peddle
                        (x + dx * 2, y + dy * 2)
                    )
                )

        stage = next_state

    raise ValueError("Unsolvable maze")


def reset(grid):
    for lno in range(len(grid)):
        for cno in range(len(grid[lno])):
            if grid[lno][cno] in (PEBBLE, "+"):
                grid[lno][cno] = PATH


def is_at_exit(pos):
    return pos == (WIDTH, HEIGHT - 2)


def available_direction(grid, pos):
    x, y = pos
    for dx, dy in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
        if (  0 <= x + dx < WIDTH
          and 0 <= y + dy < HEIGHT
          and grid[y + dy][x + dx] == PATH):
            return dx, dy


def main():
    global WIDTH, HEIGHT
    grid = [list(line.rstrip('\n')) for line in fileinput.input()]
    if not grid[-1]:
        grid.pop()
    HEIGHT = len(grid)
    WIDTH = len(grid[0])
    grid[-2].append(" ")  # Shhhhhh

    solution = breadth_first_search(grid, (1, 1))
    reset(grid)
    for x, y in solution:
        print(x, y)

    for line in grid:
        print("".join(line[:WIDTH]))


if __name__ == '__main__':
    main()

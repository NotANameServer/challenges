from random import random
import numpy as np
import pygame
from pygame.gfxdraw import filled_polygon
import hexagon_tools

BG, ASHES, YOUNG_FOREST, ANCIENT_FOREST, START_FIRE, FIRE, END_FIRE = range(7)
colors = {
    BG: (0xFF, 0xFF, 0xFF),
    ASHES: (0, 0, 0),
    YOUNG_FOREST: (0, 0xFF, 0),
    ANCIENT_FOREST: (0, 0x60, 0),
    START_FIRE: (0xFF, 0xFF, 0),
    FIRE: (0xFF, 0, 0),
    END_FIRE: (0xFF, 0x80, 0),
}


def forest(cell, neighbours):
    rn = random() * 100
    if cell == YOUNG_FOREST:
        if (
            (START_FIRE in neighbours or END_FIRE in neighbours)
            and rn < 1
            or FIRE in neighbours
            and rn < 2
        ):
            return START_FIRE
        if rn < 0.5:
            return ANCIENT_FOREST
    elif cell == ANCIENT_FOREST:
        if (
            (START_FIRE in neighbours or END_FIRE in neighbours)
            and rn < 10
            or FIRE in neighbours
            and rn < 20
            or neighbours.count(ANCIENT_FOREST) >= 5
            and rn < 0.005
        ):
            return START_FIRE
    elif cell == START_FIRE:
        if rn < 10:
            return FIRE
    elif cell == FIRE:
        if rn < 10:
            return END_FIRE
    elif cell == END_FIRE:
        if rn < 10:
            return ASHES
    elif cell == ASHES:
        if rn < 0.1:
            return YOUNG_FOREST
    return cell


class HexForestIterator:
    def __init__(
        self,
        height,
        width,
        rule=forest,
        init_state=None,
    ):
        self.height = height
        self.width = width
        self.rule = rule
        self.grid = init_state
        if not init_state:
            self.grid = np.random.choice(
                (ASHES, YOUNG_FOREST, ANCIENT_FOREST), (self.height, self.width)
            )
            self.grid[self.height // 2, self.width // 4] = START_FIRE

    def _get_neighbours(self, grid, row, col):
        res = []
        for i, j in [(0, 1), (0, -1), (-1, 0), (1, 0), (1, -1), (-1, 1)]:
            res.append(grid[(row + i) % self.height, (col + j) % self.width])
        return res

    def __iter__(self):
        while True:
            yield self.grid
            last_grid = np.copy(self.grid)
            for row in range(self.height):
                for col in range(self.width):
                    neighbours = self._get_neighbours(last_grid, row, col)
                    cell = last_grid[row, col]
                    self.grid[row, col] = self.rule(cell, neighbours)


def render(surface, grid, height, width):
    for row in range(-1, height + 1):
        for col in range(-row, width + 1):
            color = colors[grid[row % height, col % width]]
            hexcenter = hexagon_tools.coordinates(row, col)
            points = hexagon_tools.vertices(hexcenter)
            filled_polygon(surface, points, color)


def main():
    it = HexForestIterator(80, 80)
    pygame.init()
    display = pygame.display.set_mode(hexagon_tools.size(it.height, it.width), 0, 32)
    display.fill(colors[BG])

    for grid in it:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                pygame.quit()
        render(display, grid, it.height, it.width)
        pygame.display.update()


if __name__ == "__main__":
    main()

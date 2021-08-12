from math import sqrt
import numpy as np

ANGLE = 2 * np.pi / 6

RADIUS = 6
IN_DIAMETER = RADIUS * sqrt(3)
ANGLES = np.array((0, 1, 2, 3, 4, 5)) * ANGLE
X_OFFSET = RADIUS * np.sin(ANGLES)
Y_OFFSET = RADIUS * np.cos(ANGLES)


def size(width, height):
    return int(width * IN_DIAMETER), int(height * RADIUS * 1.5)


def vertices(center):
    x, y = center
    return np.array((X_OFFSET + x, Y_OFFSET + y)).T


def coordinates(row, col):
    return (col + row * 0.5) * IN_DIAMETER, row * 1.5 * RADIUS

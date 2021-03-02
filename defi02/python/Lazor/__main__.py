#!/usr/bin/python3

from random import randrange, shuffle


def gengrid(width, height):
    w = width  # absice, x
    h = height  # ordoninate, y
    grid = [["#" for _ in range(w)] for _ in range(h)]
    seen = set()

    def freearound(x, y):
        pairs = [(x - 2, y), (x, y - 2), (x + 2, y), (x, y + 2)]
        shuffle(pairs)

        for xp, yp in pairs:
            if 1 <= xp < w - 1 and 1 <= yp < h - 1 and (xp, yp) not in seen:
                yield xp, yp

    def move(x, y):
        seen.add((x, y))
        for nx, ny in freearound(x, y):
            grid[(y + ny) // 2][(x + nx) // 2] = " "
            grid[ny][nx] = " "
            move(nx, ny)

    move(w, h - 2)
    grid[1][1] = '.'
    return "\n".join("".join(line) for line in grid)


if __name__ == '__main__':
    import sys

    if len(sys.argv) < 3:
        sys.exit(f"usage: {sys.executable} {sys.argv[0]} <width> <height>")

    width = int(sys.argv[1]) | 1
    height = int(sys.argv[2]) | 1

    print(gengrid(width, height))


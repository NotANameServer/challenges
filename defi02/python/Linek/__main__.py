import random


def genGrid(x, y):
    assert x > 2 and y > 2

    laby = [["#"] * x for _ in range(y)]
    directions = [(0, 1), (0, -1), (-1, 0), (1, 0)]

    x_start = random.randrange(1, x - 1, 2)
    y_start = random.randrange(1, y - 1, 2)

    laby[y_start][x_start] = "."

    def gen(a, b):
        dirs = directions.copy()
        random.shuffle(dirs)
        for dx, dy in dirs:
            a2 = a + 2 * dx
            b2 = b + 2 * dy
            if 0 < a2 < x - 1 and 0 < b2 < y - 1 and laby[b2][a2] == "#":
                laby[b + dy][a + dx] = laby[b2][a2] = " "
                gen(a2, b2)

    gen(x_start, y_start)

    # bizarrement j'ai surtout galéré pour la porte de sortie :shrug:
    # mais bon ça semble propre
    x_end_dir, y_end_dir = random.choice(directions)
    x_end = [random.randrange(1, x - 1, 2), 0, x - 1][x_end_dir]
    y_end = [random.randrange(1, y - 1, 2), 0, y - 1][y_end_dir]
    while laby[y_end][x_end] == "#":
        laby[y_end][x_end] = " "
        x_end += x_end_dir
        y_end += y_end_dir

    return "\n".join("".join(l) for l in laby)

print(genGrid(23, 9))

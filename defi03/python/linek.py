import numpy as np
from PIL import Image
import queue


def maze2img(maze, path):
    " crée une image du laby avec le chemin en rouge et les parties visitées en vert "
    colors = {" ": 0, "#": 1, "S": 2}
    f = np.vectorize(colors.get, otypes=[np.uint8])
    img_array = f(maze)
    for el in path:
        img_array[el] = 3

    img = Image.fromarray(img_array, "P")
    img.putpalette([255, 255, 255, 0, 0, 0, 100, 255, 100, 255, 0, 0])
    img.save("maze.png")


def solveMaze(maze, start, create_image=False):
    " depht search "
    tmaze = list(filter(None, map(list, maze.splitlines())))
    sx, sy = len(tmaze), len(tmaze[0])
    end = False
    for i in range(sx):
        for j in range(0, sy, 1 if i in (0, sx - 1) else sy - 1):
            if tmaze[i][j] == " ":
                end = i, j
                break
        if end:
            break
    ex, ey = end

    def euclide(coord):
        x, y = coord
        return (x - ex) ** 2 + (y - ey) ** 2

    dirs = [(0, 1), (1, 0), (-1, 0), (0, -1)]
    path = [start]
    while path:
        x, y = path[-1]
        tmaze[x][y] = "S"  # visité
        if (x, y) == end:
            break
        neighbours = [
            (x + a, y + b) for a, b in dirs if tmaze[x + a][y + b] not in "#S"
        ]
        if neighbours:
            best_neighbour = min(neighbours, key=euclide)
            path.append(best_neighbour)
        else:
            path.pop()
    if create_image:
        maze2img(tmaze, path)
    return path


def solveMaze2(maze, start, create_image=False):
    " breadth first search "
    tmaze = list(filter(None, map(list, maze.splitlines())))
    parent = {}
    dirs = [(0, 1), (1, 0), (-1, 0), (0, -1)]
    fifo = queue.SimpleQueue()
    fifo.put(start)
    end = False
    while not fifo.empty():

        x = fifo.get()
        x1, x2 = x
        if tmaze[x1][x2] == "S":
            continue
        tmaze[x1][x2] = "S"
        try:
            neighbours = [
                (x1 + a, x2 + b) for a, b in dirs if tmaze[x1 + a][x2 + b] not in "#S"
            ]
        except IndexError:
            end = x
            break
        for y in neighbours:
            y1, y2 = y
            fifo.put(y)
            parent[y] = x

    if not end:
        raise ValueError("Ya pas de fin ! :goodenough: ")

    path = [end]
    x = end
    while x != start:
        x = parent[x]
        path.append(x)
    path = path[::-1]
    if create_image:
        maze2img(tmaze, path)
    return path


with open("maze.txt") as f:
    maze = f.read()
path = solveMaze2(maze, (1, 1), True)
print(len(path))


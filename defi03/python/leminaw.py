"""Simple maze solver using BFS and DFS traversal approaches.\n
Triphase's contrib for NaN challenge 3.
Mazes needs to be totally enclosed by walls, except its exit.
"""

from collections import deque
import numpy as np
from matplotlib import pyplot as plt
from sortedcontainers import SortedList


EMPTY   = 0x00
WALL    = 0x01
VISITED = 0x10
PATH    = 0x20

DIRECTTIONS = (
    ( 0,  1),
    ( 1,  0),
    ( 0, -1),
    (-1,  0)
)


def dist2(a, b):
    """Squared distance between two 2D points."""
    x, y = a[0] - b[0], a[1] - b[1]
    return x**2 + y**2


class Maze:
    def __init__(self, data, start, end):
        self.data = data
        self.start = start
        self.end   = end

    def compute_path(self):
        """Return an array of consecutive positions denoting a path that solves
        the maze.\n
        Return `None` if no path can be found.
        This is based on a Breadth First Search (BFS) traversal.
        """
        # This array will store the previous positions each time a new node is
        # "explorated", and will be used later on when backtracking.
        prev = np.zeros((*self.data.shape, 2), dtype='u2')
        # This sorted list acts like a stack that holds positions to be
        # explored, and is sorted (by reverse squared distance to the maze
        # exit) each time an element is pushed on it.
        # See sortedcontainers docs for implementation details.
        # Using squared distance in place of regular distance saves an
        # expensive `sqrt` computation. As square root is an increasing
        # function, it does not change ordering.
        stack = SortedList([self.start], key=lambda p: -dist2(p, self.end))

        self.data[self.start] = VISITED
        while stack:
            pos = stack.pop()

            if pos == self.end:
                # Exit were reached! :)
                break

            # Explore adjacent nodes
            for x, y in DIRECTTIONS:
                # Neighboor pos
                neigh = (pos[0] + x, pos[1] + y)

                if self.data[neigh] == EMPTY:
                    # Mark the adjacent pos as explored
                    self.data[neigh] = VISITED
                    # Add the adjacent pos to the exploration stack
                    # It will be inserted at the right index to keep the stack
                    # sorted
                    stack.add(neigh)
                    # Set the previous positions array at the adjacent pos to
                    # the current pos
                    prev[neigh] = pos
        else:
            # The given exit were never reached :(
            return None

        path = []
        pos = self.end
        while pos != (0, 0):
            # Backtrack from the exit, folowwing the values of the previous
            # posisions array until we reach an undefined value (0, 0)
            path.append(pos)
            pos = tuple(prev[pos])

        return np.array(path, dtype='u2')

    def compute_optimal_path(self):
        """Same as `compute_path`, except it is guaranteed to return an
        optimal path if it exists.\n
        This is based on a Depth First Search (DFS) traversal.
        Therefore, one might note it is MUCH (orders of magnitude) slower.
        """
        # FOR DOCUMENTATION, PLEASE SEE THE COMMENTS OF THE VERY SIMILAR
        # compute_path METHOD. COMMENTS IN THIS FUNCTION BODY WILL ONLY FOCUS
        # ON ΒEHAVIOURS THAT DIFFERS FROM compute_path.
        prev = np.zeros((*self.data.shape, 2), dtype='u2')
        # Instead of using a sorted stack, the list of nodes to be explored is
        # a double-ended queue which will be used as a FIFO structure.
        queue = deque([self.start])

        self.data[self.start] = VISITED
        while queue:
            # Take a value at the right end of the deque
            pos = queue.pop()

            if pos == self.end:
                break

            for x, y in DIRECTTIONS:
                neigh = (pos[0] + x, pos[1] + y)

                if self.data[neigh] == EMPTY:
                    self.data[neigh] = VISITED
                    # Add the adjacent pos to the left end of the deque
                    # This will ensure all positions at the current depth will
                    # be explored before digging a level further
                    queue.appendleft(neigh)
                    prev[neigh] = pos
        else:
            return None

        path = []
        pos = self.end
        while pos != (0, 0):
            path.append(pos)
            pos = tuple(prev[pos])

        return np.array(path, dtype='u2')

    def solve(self, optimal=False):
        """Try to compute a quick or optimal path to solve the maze. The maze
        will be modified in-place.\n
        Return the number of steps of the path if it was found, else return
        `None` if the maze cannot be solved.
        """
        if optimal:
            path = self.compute_optimal_path()
        else:
            path = self.compute_path()

        if path is None:
            return

        for pos in path:
            # Mark the path in the maze array
            self.data[tuple(pos)] = PATH
        return len(path)

    def show(self):
        """Show the maze using pyplot."""
        plt.imshow(self.data)
        plt.show()

    @classmethod
    def from_textfile(cls, path, start, end):
        """Load a maze from a text file and its start and end positions."""
        with open(path) as file:
            chars = np.array(
                [tuple(line.strip('\n')) for line in file.readlines()]
            )
        data = np.where(chars == '#', WALL, EMPTY).astype('u1')

        return cls(data, start, end)


if __name__ == '__main__':
    print("Loading input file...")
    maze = Maze.from_textfile('maze.txt', (1, 1), (4997, 4998))
    # maze = Maze.from_textfile('little_maze.txt', (1, 1), (11, 36))

    print("Now solving. Don't panic, it will only take a year or two...")
    r = maze.solve()
    # r = maze.solve(optimal=True)

    if r is None:
        print("Unsolvable. :c")
    else:
        print(f"Done (path: {r} steps).")
        maze.show()

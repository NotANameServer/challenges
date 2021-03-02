'''
Author : Ermite
Date : 01/03/21

Pour le défi 'Labyrinthe' de la communauté Not A Name du week-end du 28 février 2021.

'''

from random import randint, shuffle
from math import sqrt

class Grid():
    class _Cell():
        def __init__(self, r, c):
            self._value = '#'
            self._position = (r,c)

        def set_empty(self):
            self._value = ' '
        
        def set_start(self):
            self._value = '.'
        
        def get_value(self):
            return self._value
        
        def get_position(self):
            return self._position
        
        def is_adjacent(self, grid):
            ''' Return true if adjacent to more than 1 empty cell.
            May fall in a 'trap', I disable this function at the moment.'''
            counts = 0
            adj_cells = [grid[self._position[0]+1][self._position[1]], grid[self._position[0]-1][self._position[1]], 
        grid[self._position[0]][self._position[1]+1], grid[self._position[0]][self._position[1]-1] ]  
        # The list may be generated in a simplier way.
          
            for c in adj_cells:
                if c.get_value() != '#':
                    counts +=1 
            return counts > 1


    def __init__(self, width, height):
        self._width = width 
        self._height = height
        self._grid = [[Grid._Cell(r,c) for c in range(self._width)] for r in range(self._height)] 
        self._current_node = None
        self._end = None
        self._nodes = 0


    def maze_generation(self):
        self._set_start()
        self._dig(self._start)
    
    def _has_end(self):
        return not self._end is None
    
    # We don't want the end on the left or top edge of the maze.
    def _is_good_edge(self, cell):
        cell_pos = cell.get_position()
        return cell_pos[0] == self._height-1 or cell_pos[1] == self._width -1
    
    def _is_wrong_edge(self, cell):
        cell_pos = cell.get_position()
        return cell_pos[0] == 0 or cell_pos[1] == 0
    
    def _enough_nodes(self):
        ''' We want a minimum of nodes '''
        print(self._nodes> self._height * self._width)
        return self._nodes > sqrt(self._height * self._width)

    def _set_start(self):
        self._start = self._grid[randint(1, self._height-2)][1]
        self._start.set_start()
        self._current_node = self._start
        print('start in :', self._start.get_position())
    
    def _dig(self, node):
        node_pos = node.get_position()
        candidats = [self._grid[node_pos[0]+1][node_pos[1]], self._grid[node_pos[0]-1][node_pos[1]], 
        self._grid[node_pos[0]][node_pos[1]+1], self._grid[node_pos[0]][node_pos[1]-1] ]
        # The list may be generated in a simplier way.

        shuffle(candidats)
        for candidat in candidats:
            if self._is_wrong_edge(candidat):
                pass 
            elif self._is_good_edge(candidat):
                if not self._has_end() and self._enough_nodes():
                    self._end = candidat
                    self._end.set_empty()
                else:
                    pass
            else : 
                if not candidat.is_adjacent(self._grid) and candidat.get_value() == '#':
                    self._nodes += 1
                    candidat.set_empty()
                    self._dig(candidat)
                else:
                    pass


    def display(self):
        rows = [''.join([c.get_value() for c in self._grid[r]]) for r in range(len(self._grid))]
        return '\n'.join(rows)


if __name__ == '__main__':
    test = Grid(25,10)
    test.maze_generation()
    print('FINAL MAZE:')
    print(test.display())


def extract_maze(name):
	maze = []
	with open(name, 'r') as f:
		for el in f.readlines():
			maze.append(el.strip('\n'))

	return maze


def upload_maze(maze, path):
	out = [list(row) for row in maze]
	for cord in path:
		out[cord[1]][cord[0]] = '.'

	with open('out.txt', "w") as f:
		for row in out:
			f.write(''.join(row))
			f.write('\n')



def get_moves(current):
	"""
	Dictionnaire utilisé pour
	connaître l'ordre de priorité
	des déplacement par rapport
	à l'orientation.

	orientation:
					gauche   (par rapport à 'orientation')
					devant   ( // )
					droit    ( // )
					derrière ( // )
	"""
	return {
		'WEST': {
					'SOUTH': (current[0], current[1] + 1),
					'WEST': (current[0] - 1, current[1]),
					'NORTH': (current[0], current[1] - 1),
					'EAST': (current[0] + 1, current[1])
				},

		'SOUTH': {
					'EAST': (current[0] + 1, current[1]),
					'SOUTH': (current[0], current[1] + 1),
					'WEST': (current[0] - 1, current[1]),
					'NORTH': (current[0], current[1] - 1),
				},

		'EAST': {
					'NORTH': (current[0], current[1] - 1),
					'EAST': (current[0] + 1, current[1]),
					'SOUTH': (current[0], current[1] + 1),
					'WEST': (current[0] - 1, current[1])
				},

		'NORTH': {
					'WEST': (current[0] - 1, current[1]),
					'NORTH': (current[0], current[1] - 1),
					'EAST': (current[0] + 1, current[1]),
					'SOUTH': (current[0], current[1] + 1)
				}
	}


class Agent(object):
	"""
	L'entité se déplaçant dans le labyrinthe.
	il n'a connaissance que des cases voisines à
	la sienne et aux coordonnées de la sortie.
	"""
	def __init__(self, pos, env):
		self._pos = pos
		self.x = pos[0]
		self.y = pos[1]
		self.env = env

		self.path = [pos]
		self.facing = 'SOUTH'

	@property
	def pos(self):
		return self._pos

	@pos.setter
	def pos(self, new):
		self._pos = new
		self.x = new[0]
		self.y = new[1]

	def move(self, target):
		self.pos = target
		self.path.append(self.pos)

	def solver(self, maze, end):
		"""
		L'agent avance dans le labyrinthe
		en gardant toujours un mur à sa gauche.
		Si le labyrinthe suit la règle indiquant
		que chaque cellule est reliée à toutes les 
		autres, l'agent finira par atteindre la sorie.		
		"""
		while self.pos != end:
			for direction, cord in get_moves(self.pos)[self.facing].items():
				if is_valid(maze, cord):
					self.path.append(cord)
					self.move(cord)
					self.facing = direction
					break


def is_valid(matrix, target):
	"""
	Vérifie si target (x, y) est dans la matrice
	et si c'est bien une case vide
	"""
	if target[0] < 0 or target[0] >= len(matrix[0]):
		return False

	if target[1] < 0 or target[1] >= len(matrix):
		return False

	return True if matrix[target[1]][target[0]] != '#' else False



def solve_maze(maze, start, end):
	indiana_jones = Agent(start, maze)
	indiana_jones.solver(maze, end)
	upload_maze(maze, indiana_jones.path)

	print(f"finished in {len(indiana_jones.path)} moves")

work = extract_maze("soft_maze.txt")
solve_maze(work, (1, 1), (52, 19))

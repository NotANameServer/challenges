import random
MINIMUM = 1
MAXIMUM = 1_000_000


class JustePrix:
    def __init__(self):
        self.nombre = random.randint(MINIMUM, MAXIMUM)

    def propose(self, nombre):
        print(f"propose: {nombre} nb: {self.nombre}")
        if nombre < self.nombre:
            return -1
        elif nombre == self.nombre:
            return 0
        else:
            return 1


def trouver(jeu: JustePrix):
    mini = MINIMUM
    maxi = MAXIMUM + 1
    test = (mini+maxi)//2
    resp = jeu.propose(test)
    while resp != 0:
        if resp == -1:
            mini = test
        else:
            maxi = test
        test = (mini + maxi) // 2
        resp = jeu.propose(test)

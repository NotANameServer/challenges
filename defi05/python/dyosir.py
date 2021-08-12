"""
Niveau 2 : un simulateur de feu de forêt.

On note n la longueur de la forêt, ie le nombre de lignes d'abres, 
m la largeur de la forêt, ie le nombre de colonnes d'arbres, 
et {0, 1, 2, 3, 4, 5} les 6 états possibles des arbres, qui représentent
respectivement cendres, forêt jeune, forêt ancienne, début de combustion, 
en combustion et fin de combustion.

Les bords ne communiquent pas, le monde a donc la forme d'un rectangle.
"""


from random import random, randint


class Cellule:
    def __init__(self, etat):
        self.etat = etat
        
    def __str__(self):
        couleurs = [0, 156, 35, 228, 88, 208]
        return "\x1b[48;5;{}m \033[0m".format(couleurs[self.etat])


class Evenement:
    """
    On modélise de manière un peu absurde le feu de forêt, par exemple
    il est équivalent qu'une seule voisine ou 4 voisines soient en combustion.
    """
    def __init__(self, elt, seuil, proba, nouvel_etat):
        self.elt = elt
        self.seuil = seuil
        self.proba = proba
        self.nouvel_etat = nouvel_etat
        
    def est_verifie(self, etats_voisines):
        return etats_voisines.count(self.elt) >= self.seuil

    
class Monde:
    def __init__(self, n, m, init = -1):
        """
        Pour initialiser aléatoirement : init = -1,
        sinon pour initialiser avec un seul type de cellules,
        init = état de la cellule.
        """
        self.n = n
        self.m = m
        if init == -1:
            self.L = self.initialiser_aleatoirement()
        else:
            self.L = self.initialiser_etat(init)
        self.iteration = 0
        # regles[0] correspond aux événements possibles depuis l'état 0
        # regles[1] aux événements possibles depuis l'état 1
        # et ainsi de suite
        self.regles = [
            [Evenement(0, 0, 0.001, 1)],
            [Evenement(3, 1, 0.001, 3), Evenement(4, 1, 0.002, 3), Evenement(5, 1, 0.001, 3), Evenement(0, 0, 0.005, 2)],
            [Evenement(3, 1, 0.1, 3), Evenement(4, 1, 0.2, 3), Evenement(5, 1, 0.1, 3), Evenement(2, 5, 0.00005, 3)],
            [Evenement(0, 0, 0.1, 4)],
            [Evenement(0, 0, 0.1, 5)],
            [Evenement(0, 0, 0.1, 0)],
        ]
        
    def __str__(self):
        affichage = ""
        for l in self.L:
            for c in l:
                affichage += str(c)
            affichage += "\n"
        return affichage
    
    def effectifs(self):
        eff = [0] * 6
        for l in self.L:
            for c in l:
                eff[c.etat] += 1
        return eff
        
    def initialiser_aleatoirement(self):
        L = [[Cellule(randint(0, 5)) for _ in range(self.m)] for _ in range(self.n)]
        return L
    
    def initialiser_etat(self, etat):
        L = [[Cellule(etat) for _ in range(self.m)] for _ in range(self.n)]
        return L
        
    def recuperer_etats_voisines(self, i, j):
        """
        Renvoie une liste de longueur variable contenant les états des cellules voisines.
        Les cellules sur les bords ont moins de voisines, le monde ne boucle pas, donc
        la liste renvoyée est de longueur variable.
        """
        # On génère {-1, 0, -1}^2
        directions = {(x, y) for y in {-1, 0, 1} for x in {-1, 0, 1}}
        # Puis on va enlever toutes les directions qui ne sont pas permises
        # lorsqu'on est sur un bord
        a_enlever = {(0, 0)}
        if i == 0:
            a_enlever.update({(-1, -1), (-1, 0), (-1, 1)})
        if j == 0:
            a_enlever.update({(-1, -1), (0, -1), (1, -1)})
        if i == self.n - 1:
            a_enlever.update({(1, -1), (1, 0), (1, 1)})
        if j == self.m - 1:
            a_enlever.update({(-1, 1), (0, 1), (1, 1)})
        for e in a_enlever:
            directions.remove(e)
        etats = []
        for d in directions:
            etats.append(self.L[i + d[0]][j + d[1]].etat)
        return etats
    
    def nouvelle_cellule(self, c, i, j):
        etats_voisines = self.recuperer_etats_voisines(i, j)
        evenements = self.regles[c.etat]
        for e in evenements:
            if e.est_verifie(etats_voisines) and random() <= e.proba:
                return Cellule(e.nouvel_etat)
        return Cellule(c.etat)
    
    def iteration_suivante(self):
        nouveau_L = []
        for (i, l) in enumerate(self.L):
            nouveau_l = []
            for (j, c) in enumerate(l):
                nouveau_l.append(self.nouvelle_cellule(c, i, j))
            nouveau_L.append(nouveau_l)
        self.L = nouveau_L
        

def niveau2():
    n = int(input("Nombre de lignes du monde : "))
    m = int(input("Nombre de colonnes du monde : "))
    init = int(input("Initialiser à l'état (-1 : aléatoire) : "))
    k = int(input("Nombre d'itérations : "))
    
    M = Monde(n, m, init)
    print("Itération 0 ({}) : \n{}".format("|".join(map(str, M.effectifs())), M))
    for i in range(k):
        M.iteration_suivante()
        print("Itération {} ({}) : \n{}".format(i + 1, "|".join(map(str, M.effectifs())), M))


if __name__ == "__main__":
    niveau2()

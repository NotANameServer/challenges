from pysat.solvers import Glucose3
import time 
P = Glucose3()
'''On modélise le problème sous la forme d'un problème SAT,
où les littéraux sont représentés par des entiers relatifs non-nuls
n * 81 + l * 9 + c + 1 : il y a le chiffre n+1 à la l+1-ième ligne et c+1-ième colonne
-(n * 81 + l * 9 + c + 1) : il n'y a pas le chiffre n+1 à la l+1-ième ligne et c+1-ième colonne
'''
for n in range(9):
    for l in [1,4,7]:
        '''chaque chiffre apparait dans au moins une des neufs 
        case de chaque carré'''
        P.append_formula([[n*81 + l*9 + c + d + 1 for d in [0,1,8,9,10,-1,-8,-9,-10]]for c in [1,4,7]])
    for i in range(9):
        '''chaque chiffre apparait dans au moins une des neufs 
        case de chaque ligne'''
        P.add_clause([n*81+i*9+c + 1 for c in range(9)])
        P.add_clause([n*81+l*9+i + 1 for l in range(9)])
for n1 in range(9):
    for n2 in range(n1+1,9):
        '''Jamais deux chiffres dans la même case, 
        c'est à dire, pour chaque case
        (pas de 1 ou pas de 2) et (pas de 1 ou pas de 3) .... 
        et (pas de 8 ou pas de 9) '''
        clauses = [[-(n1*81+case+1),-(n2*81 +case + 1)]for case in range(81)]
        P.append_formula(clauses)
   
with open('sudoku.txt') as file:
    grilleChoisie = 1
    file.seek(108 * (grilleChoisie-1) + 9)
    lignes = file.read(89).splitlines()
    for l in range(9):
            for c in range(9) : 
                n = int(lignes[l][c])
                if n != 0:
                    P.add_clause([(n-1)*81+l*9+c+1])
    grid = [[0 for c in range(9)]for l in range(9)]
    t0 =time.time()
    print(P.solve())
    print(time.time()-t0)#normalement temps assez peu élevé, car solveur optimisé
    for i in P.get_model():
        if i > 0:
            i1 = i - 1 
            grid[(i1%81)//9][i1%9] = (i1//81) + 1
    print(grid)

    

                
        
      
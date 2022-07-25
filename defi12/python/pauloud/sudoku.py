import time
def ligneOk(S,l,c):
    for c1 in range (9):
        if c!= c1 and S[l][c1] == S[l][c] :
            #print ("erreur Ligne")
            return False  
    return True 
def colonneOk(S,l,c):
    for l1 in range (9):
        if l != l1 and S[l1][c] == S[l][c] :
            #print ("erreur Colonne")
            return False  
    return True 
def carreOk(S,l,c):
    lmillieu = (l//3)*3 + 1
    cmillieu = (c//3)*3 + 1
    for dl in [-1,0,1]:
        for cl in [-1,0,1]:
            l1,c1 = (lmillieu + dl,cmillieu + cl)
            if (l,c) != (l1,c1) and S[l1][c1]==S[l][c]:
                #print ("erreur Carre")
                return False 
    return True 

def resoudre(S,caseContrainte):
    i = 0
    while i != 81:
        l = i // 9 #numéro de ligne de 0 à 8
        c = i % 9 #numéro de colonne de 0 à 8 
        if not caseContrainte[i]:
            '''on essaye le chiffre suivant, si on est à 0, c'est qu'on a essayé
            les 9 chiffres pour la i-ème case de la grille 
            (c'est à dire ligne l+1 et colonne c + 1)
            '''
            S[l][c] = (S[l][c] + 1)%10 
        if S[l][c] != 0 and carreOk(S,l,c) and colonneOk(S,l,c) and ligneOk(S,l,c):
            i += 1
            continue 
        if S[l][c] == 0:
            i-=1
        while caseContrainte[i]:
            i-=1 

'''base de grilles fournie par le Lazor, 
qui m'a indiqué également comment la parser'''  
with open('sudoku.txt') as file: 
    
    for no in range(10): 
        file.seek(108 * no + 9)
        gridtxt = file.read(89)
        S = []
        caseContrainte = []
        for line in gridtxt.splitlines():
            l = []
            for n in line : 
                l.append(int(n))
                caseContrainte.append(l[-1]!=0)
            #assert(len(l)==9)
            S.append(l)
        #assert(len(S)==9)
        #assert(len(caseContrainte)==81)
        t1 =time.time()
        resoudre(S,caseContrainte)
        print(time.time()-t1)#temps varie de 3 millièmes à 6 secondes
        




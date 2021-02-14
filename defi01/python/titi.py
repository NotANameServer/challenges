from tools import register

def tri(tableau):
    operations = True
    while operations:
        operations = False
        for i in range(len(tableau)-1):
            if tableau[i] > tableau[i + 1]:
                a = tableau[i]
                tableau[i] = tableau[i+1]
                tableau[i+1] = a
                operations = True
    return tableau


@register
def titi_1(a, n):
    return tri(a)

from tools import register


@register
def jackblack_1(liste, taille):
    while True:
        for index in range(taille-1):
            actuel = liste[index]
            suivant = liste[index+1]
            if suivant < actuel:
                liste[index], liste[index+1] = liste[index+1], liste[index]
                break
        else:
            return liste


@register
def jackblack_2(liste, taille):
    for index in range(len(liste)):
        element = liste[index]
        liste_inverse = liste[:index] ; taille_liste_inverse = len(liste_inverse)
        for index_insert, suivant in enumerate(liste_inverse):
            if element < suivant:
                insert = liste.pop(index)
                liste.insert(index_insert, insert)
                break
    return liste


@register
def jackblack_3(liste, taille):
    for index in range(len(liste)):
        element = liste[index]
        liste_inverse = liste[:index] ; taille_inverse = len(liste_inverse)
        for index_suivant, suivant in enumerate(reversed(liste_inverse)):
            if element < suivant:
                index_insert = ((taille_inverse-1) - index_suivant)
                liste[index], liste[index_insert] = liste[index_insert], liste[index]
                index = index_insert
    return liste


@register
def jackblack_4(liste, taille):
    for index in range(len(liste)):
        element = liste[index]
        for index_suivant in reversed(range(index)):
            suivant = liste[index_suivant]
            if element < suivant:
                liste[index], liste[index_suivant] = liste[index_suivant], liste[index]
                index = index_suivant
            else:
                break
    return liste

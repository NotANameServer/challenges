from random import randint

nb_secret = randint(1,1000000)

minimum = 0
maximum = 1000000

def propose(nombre):
    if nombre < nb_secret:
        return -1
    elif nombre == nb_secret:
        return 0
    elif nombre > nb_secret:
        return 1

def trouver():
    global minimum
    global maximum

    tentatives = 0

    while True or tentatives != 50:
        nombre = minimum + int(minimum + (maximum - minimum) / 2)
        print("Proposition :", nombre)

        if propose(nombre) == -1:
            print("Trop petit")
            minimum = nombre + 1
            tentatives += 1

        elif propose(nombre) == 0:
            print("L'ordinateur avez trouvé !")
            break

        elif propose(nombre) == 1:
            print("Trop grand")
            maximum = nombre - 1
            tentatives += 1

    if tentatives == 50:
        print("Nombre de tentatives épuisé !")

trouver()

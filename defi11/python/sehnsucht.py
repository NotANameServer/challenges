from random import randint

def __make_propose():
    secret = randint(1, 1_000_000)
    return lambda nombre: (secret > nombre) - (secret < nombre)

propose = __make_propose()

def trouver():
    borne_min, borne_max = 1, 1_000_001

    for tentative in range(50):
        proposition = (borne_max + borne_min) // 2

        comparaison = propose(proposition)

        reponse, borne_min, borne_max = (
            ('trop grand', borne_min, proposition),
            ('trouvé', None, None),
            ('trop petit', proposition, borne_max)
        )[comparaison + 1]

        print('Proposition N°', tentative + 1, ':', proposition, '->', reponse)

        if not comparaison:
            break

trouver()

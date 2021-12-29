import random

max = 1000000

secret_number = random.randint(1, max)

def propose(number):
    if number > secret_number: return -1
    if number < secret_number: return 1
    else: return 0

def trouver():
    essais = 1
    number = max / 2
    new_max = max / 4

    found = False

    while not found:
        resp = propose(number)

        match resp:
            case -1:
                if new_max == 0:
                    new_max = 1
                old_number = number
                number = number - new_max
                new_max = abs(old_number - number) // 2

            case 1:
                if new_max == 0:
                    new_max = 1
                old_number = number
                number = number + new_max 
                new_max = abs(old_number - number) // 2

            case 0:
                print(f"Trouvé en {essais} essai(s).\nLe nombre était {secret_number} !")
                found = True

        if essais == 50:
            print("Perdu... l'ordinateur n'a pas trouvé en 50 essais")
            return

        essais += 1

if __name__ == "__main__":
    trouver()


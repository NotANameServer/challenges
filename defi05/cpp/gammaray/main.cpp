#include <iostream>
#include <stdlib.h>
#include <vector>
#include <cassert>
#include <unistd.h>


int getNextValue(int index, std::vector<int> world);
void printWorld(std::vector<int> world);
std::vector<int> worldCreator();


int main()
{
    std::vector<int> world { worldCreator() };  // Le monde actuel
    std::vector<int> nextWorld { world };       // La monde à l'état n + 1
    int iterMax { 0 };

    std::cout << "Entrez le nombre d'itérations voulues : ";
    std::cin >> iterMax;
    std::cout << "\033[J\033[H";  // ESC[J efface l'écran, ESC[H positionne le curseur en (0, 0)
    std::cout << "\033[J\033[H";  // Je sais pas vraiment pourquoi il faut le faire 2 fois ici
                                  // mais ça ne semble pas fonctioner correctement sans


    for (int iter { 0 }; iter < iterMax; iter++)
    {   
        for (int i { 0 }; i < std::size(world); i++)
        {
            nextWorld[i] = getNextValue(i, world);
        }

        world = nextWorld;  // Le monde actuel devient le monde n + 1
        printWorld(world);
        
        usleep(1000000);              // On attends quelques secondes
        std::cout << "\033[J\033[H";  // On efface l'écran et on reboucle
    }

    return 0;
}

int getNextValue(int index, std::vector<int> world)
{
    int sum { 0 };
    int nextValue { 0 };

    // on boucle pour récupérer la valeur des cases voisines :
    // celle de droite (index - 1) , la case actuelle (index), celle de gauche (index + 1)
    for (int i { index - 1 }; i <= index + 1; i++)
    {
        if (i < 0 || i >= std::size(world))
        {
            continue;  // Si l'index est trop petit/grand, on reboucle sans rien faire
        }

        sum += world[i];  // On ajoute à la somme la valeur de la case voisine i
    }
    
    // Selon la valeur de la somme totale, on détermine la valeur que prendra
    // la case à l'itération suivante
    switch (sum)
    {
        case 0:
        case 3:
        case 4:
            nextValue = 0;
            break;

        case 5:
        case 9:
            nextValue = 1;
            break;

        case 2:
        case 7:
            nextValue = 2;
            break;

        case 1:
        case 6:
        case 8:
            nextValue = 3;
            break;
    }


    return nextValue;
}


void printWorld(std::vector<int> world)
{
    //
    std::string currentRepr { "" };

    for (auto const item : world)
    {
        switch (item)
        {
            case 0:
                currentRepr += "\033[1;37m█\033[0m";  // white

                break;

            case 1:
                currentRepr += "\033[1;34m█\033[0m";  // blue
                break;

            case 2:
                currentRepr += "\033[1;32m█\033[0m";  // green
                break;

            case 3:
                currentRepr += "\033[1;31m█\033[0m";  // red
                break;
        }
    }

    std::cout << currentRepr << '\n';
}


std::vector<int> worldCreator()
{
    std::string worldInput { "" };
    std::vector<int> worldVerified { };

    std::cout << "Un monde doit être composé de case, chaque case ayant une valeur comprise entre [0; 3]\n";
    std::cout << "Entrez votre monde : ";
    std::cin >> worldInput;

    for (auto const el : worldInput)
    {
        int value = el - '0';

        if (value < 0 || value > 3)
        {
            assert(false && "Chaque case doit avoir une valeur comprise entre [0; 3].\n");
        }

        worldVerified.push_back(value);
    }

    return worldVerified; 
}

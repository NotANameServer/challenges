# Défi 11 - Le juste prix

Défi sur un weekend, à rendre pour le mardi 19 octobre à 6h du matin au plus tard à votre ambassadeur préféré. Le live se déroulera mardi à 20h00.

Avec la rentrée scolaire, nous vous proposons un défi qui se veut accessible à tous. Il s’agit d’écrire un programme qui résout le problème du juste prix. Le jeu du juste prix est un jeu à deux joueurs dans lequel le 1er joueur doit retrouver le nombre secret que le 2ème joueur a choisi. Le 1er joueur propose un nombre, par exemple 120 et le 2e joueur répond « trop petit », « trop grand » ou « trouvé » en fonction des essais du 1er joueur. Le but du 1er joueur est de trouver le nombre secret le plus rapidement possible.

Exemple avec les joueurs Willy et Serge :

    Willy choisit un nombre secret entre 1 et 20, il choisit 12.
    Serge propose 20, Willy lui répond « trop grand ».
    Serge propose 15, Willy lui répond « trop grand ».
    Serge propose 10, Willy lui répond « trop petit ».
    Serge propose 12, Willy lui répond « trouvé ».
    La partie est finie, Serge a trouvé le nombre secret en 4 coups.

L’objectif de ce défi n’est pas de faire jouer des joueurs humains entre eux, il n’est pas non plus de faire jouer l’humain contre l’ordinateur mais bien de faire jouer l’ordinateur contre lui-même.

Vous devez écrire un programme où l’ordinateur va choisir un nombre secret aléatoire entre 1 et 1 000 000 inclus, qui ne changera pas au cours de la partie et qui n’est connu que d’une fonction : propose(nombre) (que vous devez aussi écrire) dont la spécification est la suivante :

    Si le nombre secret est plus petit que la proposition, la fonction retourne -1
    Si le nombre secret est plus grand que la proposition, la fonction retourne 1
    Si le nombre secret et la proposition sont égaux, la fonction retourne 0

En n’utilisant que la fonction propose(nombre), vous écrivez une fonction trouver() où l’ordinateur doit réussir à retrouver le nombre secret. Attention, l’ordinateur n’a droit qu’à un maximum de 50 tentatives !

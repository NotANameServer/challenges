# Défi 12 - Sudoku

Défi sur un weekend, à rendre pour le mardi 19 à 6h du matin au plus tard à votre ambassadeur préféré. Le live se déroulera mardi à 20h00.

Un sudoku est une grille de 9x9 cases réparties en 9 lignes, 9 colonnes et 9 carrés où peuvent être inscrits des nombres allant de 1 à 9. Au départ, certaines cases sont laissées vides, le but du puzzle est de remplir toutes les cases de sortes à ce qu'un même nombre n'apparaisse pas 2x ni sur la même ligne, ni sur la même colonne, ni dans le même carré. 

![sudoku](https://cdn.discordapp.com/attachments/808414060147769415/995006051185328158/sudoku-rules.png)

Dans l'exemple ci-dessus, si on étudie la ligne rouge, on constate qu'il manque un 1. Il y a 5 emplacements vacants sur cette ligne, l'une des cases doit accueillir notre 1. Si on étudie les cases se trouvant dans les deux derniers carrés (celui du milieu et celui de droite), on constate qu'il y a déjà un 1 dans chacun de ces deux carrés: on ne peut pas inscrire un autre 1 dans ces deux carrés. Il ne reste alors qu'un seul emplacement possible pour inscrire un 1 sur la ligne rouge: à la 2e colonne, entre le 5 et le 9.

Le but de ce défi est d'écrire un programme capable de résoudre les grilles de sudoku.

Nous vous proposons 4 déclinaisons du défi:

1. Écrire un programme qui vérifie une grille complétée de sudoku.
2. Écrire un programme qui résout les petits sudoku (16 cases, 4 lignes, 4 colonnes, 4 carrés, des nombres allant de 1 à 4).
3. Écrire un programme qui résout les sudoku classiques.
4. Écrire un programme qui génère une grille de sudoku (à trou) qui a une solution garantie.

Petit sudoku:

    1  3
     3 4
    4132
    3 

Sudoku classique:

    7   1   9
          41 
    5 9  6  7
            2
    18    9 5
         5 8 
      284  9 
     6    158
       6     

Vous pouvez en générer d'autres via ce site: https://www.sudokuweb.org/ 
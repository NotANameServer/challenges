# Résolution de Sudoku en Scala

Voici un exemple du défi n°12 en Scala

Ce projet permet de :
- Vérifier si une grille de Sudoku est valide
- Résoudre une grille de Sudoku classique

La résolution de Sudoku utilise une première méthode de résolution par contraintes s'articulant autour de deux conditions :
- Si une case vide n'a plus qu'une seule possibilité, la remplacer par cette seule valeur possible
- Si un nombre n'est plaçable qu'à un endroit, alors le placer à ce dit endroit

# Lancer l'exemple

Vous pouvez exécuter le projet avec la commande suivante :
```sh
./millw main.run
```

Note : sur Windows, vous devrez utiliser le fichier `millw.bat`.

Vous pouvez ensuite éditer le fichier `Main.scala` pour tester le projet.

# Puissance 4 en Scala

Voici un exemple en Scala du défi 8: créer un puissance 4. Ce readme résume l'approche de certaines parties du projet.

Le code est entièrement documenté via une Scaladoc en anglais (voir le code).

# Lancer l'exemple

Vous pouvez exécuter le projet avec la commande suivante: `sh millw -i main.run`

# Grille

## Coordonnées 2D

L'implémentation de la grille 2D se fait via une collection 1D. Pour convertir les coordonnées x/y en un index, nous
utilisons simplement la division et le modulo:

- index = x+y*nombreDeColonne
- x = index%nombreDeColonne
- y = index/nombreDeColonne

## Vérification de l'alignement

Pour vérifier si 4 pions sont alignés, le jeu va tester 4 directions sur chaque pion:

- Vers la droite
- Vers le haut
- Diagonale haut-droite
- Diagonale haut-gauche

Note: il n'y a pas besoin de faire la symétrie (centrale) de ces directions puisqu'elles seront vérifiées via l'analyse
d'autres pions.

# Gestion des entrées

Les entrées sont gérées via une approche fonctionnelle en utilisant l'objet monadique `Either[A, B]`
(une donnée ou l'autre). Cette approche permet de gérer facilement les mauvaises entrées utilisateur en fail-fast:
```scala
private def input(grid: Grid): Either[String, (Int, Int)] = for {
  choice <- readInt(s"$name: Choisissez un nombre entre 1 et ${grid.columns}", "Nombre invalide.")
  column <- Either.cond(choice > 0 && choice <= grid.columns, choice, s"La colonne $choice n'existe pas.")
  result <- grid.findLocation(column-1).toRight(s"La colonne $choice est pleine.")
} yield result
```

Méthode `Human#input` (Player.scala).

# Abstraction

Le projet a été conçu avec de façon à avoir une couche abstraite (via les traits).
Cette approche permet de gérer différentes implémentations notamment au niveau de l'UI
(si on veut passer sur un GUI par exemple) ou du joueur (IA/Humain).

# IA

L'IA du projet est relativement simple et "naïve". Elle chaque tour vérifier quel placement est le meilleur sur le court
terme sans se soucier du joueur (sauf coup gagnant imminent)
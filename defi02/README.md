# Défi 2 - Génération de labyrinthe

Défi sur un weekend, à rendre pour le dimanche 28 février à 18h au plus tard à votre ambassadeur préféré. Le live se déroulera le sur-lendemain, mardi soir à 20h00.

Écrivez une fonction `genGrid` qui prend deux arguments, une largeur et une hauteur et qui génère un labyrinthe sous forme d'une chaîne de caractères.

La chaine de caractères est constituée uniquement des caractères "`# .\n`". Le dièse pour un mur, l'espace pour une case vide, le point pour le point de départ et le retour à la ligne au format UNIX comme fin de ligne.

Le labyrinthe doit être entièrement entouré d'un mur à l'exception d'**une seule case** ` ` qui montre la sortie et **doit être soluble** (et pas dans l'eau).

Note : appeler la fonction avec les mêmes paramètres ne doit pas forcément renvoyer le même labyrinthe.

Bonus : il serait intéressant en plus d'avoir une petite UI, en console, en application, en web, etc.

Exemple :
```
genGrid(23, 9)
```

```
#######################\n
####          ##     ##\n
#### ##### ##### ### ##\n
#.    #### ##### ###  #\n
### ######       ### ##\n
### ######## ####### ##\n
###     #### #       ##\n
####### #    # ##### ##\n
############## ########\n
```
![](https://media.discordapp.net/attachments/718186915165110302/814183269363875890/unknown.png)
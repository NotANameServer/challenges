# Défi 3 - Résolution de Labyrinthe

Défi sur un weekend, à rendre pour le lundi 15 mars à 6h du matin au plus tard
à votre ambassadeur préféré. Le live se déroulera à 20h00, lundi ou mardi.

Écrire une fonction solveMaze qui prend en argument un labyrinthe et une
position de départ et qui retourne le chemin à suivre pour atteindre la sortie
sous forme d'une séquence de coordonnées. Bonus si vous faites une
visualisation du chemin parcouru.

![exemple](https://cdn.discordapp.com/attachments/718186915165110302/819992199705919520/2021-03-12_18-55-42.png)

En pièce jointe, nous vous proposons un labyrinthe au format texte de dimension
4999x4999 complètement entouré d'un mur. Les caractères utilisés sont les mêmes
que la semaine dernière, à savoir un dièse (#) pour un mur, un espace pour un
chemin et un point pour le point de départ. Le départ se trouve en position
(x=1, y=1) et la sortie se trouve en (x=4999, y=4998). Attention, il y a des
boucles dans ce labyrinthe, vous risquez de tourner en rond si vous ne faites
pas attention, voir l'image ci-dessous.

![boucle](https://cdn.discordapp.com/attachments/718186915165110302/819527451457486868/2021-03-11_12-08-58.png)

Vous êtes bien sûr libre de générer votre propre labyrinthe si vous le
préférez.

* <https://docs.drlazor.be/maze.txt.gz>
* <https://docs.drlazor.be/maze.zip>
* <https://docs.drlazor.be/maze.7z>

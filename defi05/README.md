# Défi 5 - Automates cellulaires

Défi sur un weekend, à rendre au plus tard pour ce lundi 19 avril à 20h. Le live se déroulera normalement le lendemain à 20h.

Un grand merci à **[@LeMinaw](https://github.com/leminaw)** qui a rédigé ce défi.

:o: Pour ce défi, nous vous proposons de plonger dans le monde fascinant des automates cellulaires.

:o: Trois niveaux de difficulté vous sont proposés, le défi est ainsi accessible à tous et toutes. Le dernier niveau mettra à rude épreuve la créativité des plus chevronné·es, et donnera lieu à l'élection de la proposition la plus originale par l'ensemble de NaN, avec un rang personalisé (provisoire :p) à la clé !

*[Le document original est disponible ici.](./README.pdf)*

## Introduction

On imagine un « monde » constitué d’une **grille peuplée de cellules**. L’état de chaque cellule à l’instant 𝑡 + 1 est donné par l’évaluation d’une fonction **sur les cellules voisines** à l’instant 𝑡. Cette fonction est nommée « fonction de transition ».

**On appelle « automate cellulaire » un programme capable de calculer les états successifs de la grille-monde.**

En faisant varier la dimension, la taille ou le degré de connexité du monde (c’est-à-dire la forme des cellules), ainsi que le voisinage ou la fonction de transition, les automates cellulaires donnent naissance à des objets **très riches** et **très différents**.

### Une formulation équivalente

On nomme ![f1] l’ensemble des 𝑛 états possibles pour chaque cellule du monde, et 
![f2] le monde de dimension 𝑚 (c’est-à-dire l’ensemble des cellules 𝑤).

Soit ![f3] la fonction *voisinage*, qui permet d’obtenir l’état des 𝑘 cellules voisines d’une certaine cellule du monde, et ![f4] la fonction de transition.

L’état du monde 𝑊 à l’instant 𝑡 + 1 est alors :
<p align="center">
  <image src="https://render.githubusercontent.com/render/math?math=%5Clarge+%5Cdisplaystyle+W_%7Bt%2B1%7D+%3D+%5C%7B+f%28w_i%2C+v%28w_i%29%29+%5Cmid+w_i+%5Cin+W_t+%5C%7D">
</p>

### L'exemple canonique : le jeu de la vie de John Conway

Le plus célèbre des automates cellulaires est probablement le « jeu de la vie », formulé par le mathématicien John Conway en 1970.

<p align="center">
  <image height="250" src="https://www.conwaylife.com/w/images/1/1b/160p10h2v0.gif"> <image height="250" src="https://www.conwaylife.com/w/images/a/a0/132p37.gif">
  <br>
  <i>Deux structures dans</i> Life, <i>l’une mobile, l’autre statique.</i>
</p>



Les « règles » de cet automate sont les suivantes :
  - Le monde est une **grille de dimension 2** et de taille arbitraire, où chaque cellule est carrée ;
  - Chaque cellule peut avoir **deux états**, vivante ou morte ;
  - Le voisinage d’une cellule est composé des **8 cellules adjacentes** ;
  - Enfin, la fonction de transition est la suivante :
    - Une cellule **morte** possédant **exactement trois** voisines vivantes devient **vivante** (les cellules se « reproduisent ») ;
    - Une cellule **vivante** ne possédant **pas exactement deux ou trois** voisines vivantes devient **morte** (la cellule meurt de « solitude » ou de « surpopulation »).

On représente classiquement les cellules vivantes comme des carrés noir, les cellules mortes comme des carrés blancs. En appliquant successivement la fonction de transition à l’ensemble des cellules du monde, des motifs très riches et des comportements fascinants émergent de ces règles, pourtant très simples.

*Life* possède une assez grande communauté de « joueurs », qui construisent des structures parfois 
gigantesques, capables de se déplacer, de se cloner, de transmettre des données…

<p align="center">
  <a href="https://www.youtube.com/embed/xP5-iIeKXE8?feature=oembed"><image src="https://img.youtube.com/vi/xP5-iIeKXE8/0.jpg"></a>
  <br>
  Life in Life, <i>un automate cellulaire construit dans</i> Life <i>lui-même, exprime la Turing-complétude</i> de Life.
</p>

### Un extra : formulation mathématique de la fonction de transition

On définit les cellules vivantes comme à l’état logique vrai (𝑉) et les cellules mortes comme à l’état 
logique faux (![](https://render.githubusercontent.com/render/math?math=%5Cdisplaystyle+%5Coverline%7BV%7D)). En appelant 𝐸 l’état de la cellule actuelle et 𝑥 le nombre de cellules voisines vivantes, 
on peut définir la fonction de transition comme suit :
<p align="center">
  <image src="https://render.githubusercontent.com/render/math?math=%5Clarge+%5Cdisplaystyle+f+%5Ccoloneqq+%5Cbegin%7Bcases%7D+%28E%2C+x%29+%5Cto+%28x+%3D+3%29+%5Cvee+%28E+%5Cwedge+%28x+%3D+2%29%29+%5C%5C%0A%28%5C%7B+V%2C+%5Coverline%7BV%7D+%5C%7D%2C++%5Cleft%5B%5C%21%5Cleft%5B+0%2C8+%5Cright%5D%5C%21%5Cright%5D%29+%5Cto+%5C%7B+V%2C%5Coverline%7BV%7D+%5C%7D+%5Cend%7Bcases%7D%0A">
</p>

## Le défi

Vous êtes fin prêt à réaliser votre propre automate cellulaire !

En fonction de votre niveau, des capacités des outils que vous désirez utiliser, et du temps que vous souhaitez y consacrer, **trois niveaux** de complexité sont proposés. Le troisième niveau, plus libre, sera sujet à un **vote de la communauté** qui élira la proposition la plus intéressante ! :sparkles:

Vous êtes évidemment libre de réaliser les niveaux qu’il vous plaira, merci toutefois de ne soumettre qu’**une seule proposition**.

### Niveau 1 : un automate monodimensionnel totalistique

Ne vous laissez pas impressionner par le nom, c’est bien moins compliqué que ça en a l’air ! :stuck_out_tongue:

Contrairement au jeu de la vie qui se déroule dans un monde en deux dimensions, le monde de l’automate que vous devez réaliser ne comporte qu’une seule ligne de cellules, il ne se « joue » que sur une seule dimension.

Dans ce monde, les cellules peuvent prendre quatre états, correspondant aux nombres {0,1,2,3}.

<table>
  <tbody>
    <tr>
      <td>0</td>
      <td>2</td>
      <td>0</td>
      <td>1</td>
      <td>2</td>
      <td>3</td>
      <td>3</td>
      <td>0</td>
      <td>1</td>
      <td>1</td>
    </tr>
  </tbody>
</table>

*Une vue d’un monde correspondant à ces règles, à un état quelconque.*

Et la fonction de transition ? Elle est un peu différente de celle de Life, mais elle est en fait plus simple. En effet, **le nouvel état d’une cellule dépendra de la somme de la cellule et de ses deux voisines**, selon le tableau suivant :

<table>
  <tbody>
    <tr>
      <th scope="row">Valeur de la somme</th>
      <td>0</td>
      <td>1</td>
      <td>2</td>
      <td>3</td>
      <td>4</td>
      <td>5</td>
      <td>6</td>
      <td>7</td>
      <td>8</td>
      <td>9</td>
    </tr>
    <tr>
      <th scope="row">Nouvel état</th>
      <td>0</td>
      <td>3</td>
      <td>2</td>
      <td>0</td>
      <td>0</td>
      <td>1</td>
      <td>3</td>
      <td>2</td>
      <td>3</td>
      <td>1</td>
    </tr>
  </tbody>
</table>

Pas clair ? :sweat_smile: Bon, un exemple. Reprenons le monde du paragraphe précédent, et supposons que je veuille obtenir le nouvel état de la cellule en gras :

<table>
  <tbody>
    <tr>
      <td>0</td>
      <td>2</td>
      <td><i>0</i></td>
      <td><b>1</b></td>
      <td><i>2</i></td>
      <td>3</td>
      <td>3</td>
      <td>0</td>
      <td>1</td>
      <td>1</td>
    </tr>
  </tbody>
</table>

Pour cela, je fais la somme entre la cellule et son voisinage, en italique : 0 + 1 + 2 = 3. D’après le tableau de la fonction de transition, si la somme vaut 3, le nouvel état de la cellule sera 0.

Votre mission est donc de faire un programme qui **calcule l’évolution de ce monde**, et qui affiche ses états successifs. Vous pouvez par exemple afficher ces états ligne par ligne dans un terminal, où chaque valeur de cellule est représentée par un caractère ; ou bien générer une image dont chaque ligne de pixels correspond à un état du monde.

#### Quelques conseils

  - Un très bon état initial pour votre monde est une ligne remplie de zéros, avec **une unique cellule à l’état 1 au centre**. Son évolution pourrait vous surprendre… :
<table>
  <tbody>
    <tr>
      <td>...</td>
      <td>0</td>
      <td>0</td>
      <td>0</td>
      <td>1</td>
      <td>0</td>
      <td>0</td>
      <td>0</td>
      <td>...</td>
    </tr>
  </tbody>
</table>

*Un état initial vivement recommandé !*

  - Si c’est compliqué, ne perdez pas trop de temps avec un affichage graphique ! Vous pouvez 
obtenir de très bons résultats dans le terminal, essayez par exemple de représenter les 
nombres {0,1,2,3} par les caractères « », « . », « | » et « X ».

### Niveau 2 : un simulateur de feu de forêt

Pour ce simulateur, le monde est une **grille bidimensionnelle** de taille arbitraire (identique au jeu de la vie).

Les cellules peuvent être dans les états suivant. Selon la manière dont vous souhaitez afficher votre résultat (image, terminal…), un code couleur et des emojis sont également suggérés :
  - Cendres :skull: (noir)
  - Forêt jeune :seedling: (vert clair)
  - Forêt ancienne :deciduous_tree: (vert foncé)
  - Début de combustion :sparkler: (jaune)
  - En combustion :fire: (rouge)
  - Fin de combustion :fire_extinguisher: (orange)

On considère le voisinage d’une cellule comme étant **ses 8 cellules adjacentes**. Les règles de transition sont les suivantes (les règles les plus hautes dans le tableau sont prioritaires) :

<table>
  <thead>
    <tr>
      <th>Ancien état</th>
      <th>Nouvel état</th>
      <th>Condition sur le voisinage</th>
      <th>Proba</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td rowspan="3">Forêt jeune</td>
      <td rowspan="3">Début de comp.</td>
      <td>Une voisine au moins en début de combustion</td>
      <td>1%</td>
    </tr>
    <tr>
      <td>Une voisine au moins en combustion</td>
      <td>2%</td>
    </tr>
    <tr>
      <td>Une voisine au moins en fin de combustion</td>
      <td>1%</td>
    </tr>
    <tr>
      <td rowspan="3">Forêt ancienne</td>
      <td rowspan="3">Début de comp.</td>
      <td>Une voisine au moins en début de combustion</td>
      <td>10%</td>
    </tr>
    <tr>
      <td>Une voisine au moins en combustion</td>
      <td>20%</td>
    </tr>
    <tr>
      <td>Une voisine au moins en fin de combustion</td>
      <td>10%</td>
    </tr>
    <tr>
      <td>Début de comp.</td>
      <td>Combustion</td>
      <td rowspan="3"><i>Aucune</i></td>
      <td rowspan="3">10%</td>
    </tr>
    <tr>
      <td>Combustion.</td>
      <td>Fin de comp.</td>
    </tr>
    <tr>
      <td>Fin de comp.</td>
      <td>Cendres</td>
    </tr>
    <tr>
      <td>Cendres</td>
      <td>Forêt jeune</td>
      <td rowspan="2"><i>Aucune</i></td>
      <td>0,1%</td>
    </tr>
    <tr>
      <td>Forêt jeune</td>
      <td>Forêt ancienne</td>
      <td>0,5%</td>
    </tr>
    <tr>
      <td>Forêt ancienne</td>
      <td>Début de comp.</td>
      <td>Cinq voisines au moins en forêt ancienne</td>
      <td>0,005%</td>
    </tr>
  </tbody>
</table>

Contrairement aux automates proposés jusqu’ici, celui-ci introduit la notion de hasard. Par exemple, si une cellule contient de la forêt ancienne **et** qu’une de ses voisines au moins est en combustion, la cellule à 20% de chances de s’embraser à son tour.

Vous pouvez initialiser le monde comme vous le souhaiter, par exemple avec toutes les cellules à l’état « cendres » si vous voulez voir votre forêt pousser, ou avec toutes les cellules à l’état « forêt jeune » si vous êtes pressés de voir vos premiers incendies. :relaxed:

### Niveau 3 : mini-concours, définissez les règles du jeu !

En vous inspirant des règles présentées ici ou en créant de nouvelles de toutes pièces, **fabriquez un automate cellulaire** original.

Laissez libre cours à votre imagination et **expérimentez** : voisinages asymétriques, 3D, grilles triangles ou hexagonales, règles de transition étranges… Tout est possible ! Notez également que rien n’oblige les cellules à se retreindre à un nombre fini d’états, ou à n’être que dans un seul état à la fois. :smile:

Peut-être est-ce purement abstrait, ou peut-être allez-vous partir d’un phénomène réel ? Peut-être est-ce interactif, avec une interface graphique ? Ou bien uniquement affiché dans un terminal ? :relaxed:

Une fois satisfait de votre automate, réalisez-en une capture (image fixe, GIF, vidéo…). Les membres de NaN seront invités à **voter pour celui qu’ils préfèrent** !

*Astuce : comme l’illustre le jeu de la vie, **il n’y a pas besoin de partir dans des choses compliquées pour obtenir des résultats très intéressants**. Commencez par fixer des paramètres et des règles simples, et complexifiez-les au fur et à mesure, plutôt que de créer d’une traite un jeu de règles compliqué que vous aurez du mal à faire évoluer.*

[f1]: https://render.githubusercontent.com/render/math?math=%5Cdisplaystyle+E+%3D+%5C%7B+e_0%2C+e_1%2C+%5Cdots%2C+e_%7Bn-1%7D+%5C%7D
[f2]: https://render.githubusercontent.com/render/math?math=%5Cdisplaystyle+W+%3D+%5C%7B+w_i+%5Cmid+i+%5Cin+%5Cmathbb%7BN%7D%5Em%2C+w_i+%5Cin+E++%5C%7D
[f3]: https://render.githubusercontent.com/render/math?math=%5Cdisplaystyle+v+%3A+W+%5Cto+E%5Ek
[f4]: https://render.githubusercontent.com/render/math?math=%5Cdisplaystyle+f+%3A+%28E%2C+E%5Ek%29+%5Cto+E
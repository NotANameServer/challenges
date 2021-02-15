
**[1] Algorithme de tri** 

Défi sur un weekend, à rendre pour ce dimanche 14 février à 14h au plus tard à votre ambassadeur préféré. Écrivez une fonction qui prend deux arguments : un tableau d'entiers `a` d'une taille `n`, qui renvoie le tableau trié par ordre croissant. Vous avez le droit de renvoyer une copie triée du tableau original ou bien de renvoyer le même tableau si vous l'avez trié sur place. Je rappelle que la définition d'un tri est la permutation d'une liste de sorte à ce que chaque élément de la liste soit ordonné par rapport aux autres.

**Attention**
Les temps sont à prendre avec des pincettes, ils sont dépendants du matériel, du contexte de lancement et dans le cas du tris des données en entrées, ils n'ont donc aucunne valeurs

# emalios
**Source :** [https://github.com/Emalios/Sorter](https://github.com/Emalios/Sorter "https://github.com/Emalios/Sorter")

**Méthode :** tris fusion
| Test | Temps (ms) |
|--|--|
| testSimple| 0 |
| testSimpleX1000 | 0 |
| testLimit| 0 |
| testEquality| 0 |
| testLargeOderingArray1k| 15 |
| testLargeReverseOderingArray1k | 15 |
| testLargeArrayRandom1k | 16 |
| testLargeOderingArray10k| 321 |
| testLargeReverseOderingArray10k | 321 |
| testLargeArrayRandom10k | 524 |
| testLargeOderingArray150k| ko |
| testLargeReverseOderingArray150k | ko |
| testLargeArrayRandom150k | ko |

**Commentaire :**
Si je devait pinailler, tu ne vérifie pas les valeurs d'entrées de ta fonction (si l'array est null plantage), pas de javadoc, la classe Tuple2 qui n'ai pas très élégante (Nom de la classe qui devrait plutôt être quelque chose comme Pair, s et t ne sont pas des nom de variable terrible, first et seconde serait mieux par exemple), en Junit on préfère utiliser les Assertion jUnit plutôt que les assert de base.
De plus, l'algo étant basé sur la récursivité, cela pose problème pour des tableau de grande taille

# syrows
**Source :** [https://gitlab.com/Syr0ws/heapsort](https://gitlab.com/Syr0ws/heapsort "https://gitlab.com/Syr0ws/heapsort")

**Méthode :** Tri par tas
| Test | Temps (ms) |
|--|--|
| testSimple| 0 |
| testSimpleX1000 | 0 |
| testLimit| 0 |
| testEquality| 0 |
| testLargeOderingArray1k| 0 |
| testLargeReverseOderingArray1k | 0 |
| testLargeArrayRandom1k | 0 |
| testLargeOderingArray10k| 0 |
| testLargeReverseOderingArray10k | 0 |
| testLargeArrayRandom10k | 0 |
| testLargeOderingArray150k| 47 |
| testLargeReverseOderingArray150k | 32 |
| testLargeArrayRandom150k | 31 |

**Commentaire :**
Le code est vraiment propre, les seul bémols que j'ai trouvé, sont quel que commentaires pas super utile et les for sur une ligne que je trouve particulièrement illisible.
Mais la qualité du code est clairement d'un niveau pro.

# Défi 7 - Calculatrice

Défi sur une semaine, à rendre pour le lundi 5 juillet 19h59 à votre ambassadeur préféré. Le live se déroulera le mardi 6 juillet à 20h00.

Dans ce défi, vous allez devoir réaliser une calculatrice basique.

Deux niveaux de difficulté vous sont proposés :
  - Le premier est de réaliser une calculatrice en notation postifxée (cf. le paragraphe suivant pour l'explication).
  - Le second est de réaliser une calculatrice en notation infixée (l'écriture "standard", notamment utilisée en France).

La notation postfixée, aussi appelée notation polonaise inverse (NPI), consiste à placer les opérateurs derrière les opérandes. Cela évite les problèmes liés aux parenthèses et aux priorités, étant donné que les calculs se lisent de gauche à droite.
Par exemple, l'expression `(3 + 4) / 2` se note `3 4 + 2 /` en notation postfixée.

Votre calculatrice devra obligatoirement implémenter les opérateurs suivants : `+`, `-`, `*`, `/`, `!` (factorielle) et `^` (puissance), mais vous êtes libres d'en implémenter d'autres (n'oubliez cependant pas d'ajouter un commentaire, un petit fichier de documentation pour expliquer ou lister les opérateurs implémentés).

Pour le niveau 2, sachez que directement évaluer des opérations en notation infixée, c'est très difficile; cependant, transformer de la notation infixée en notation postfixée est bien plus simple.

Ce qui rend l'évaluation de la notation infixée difficile, ce sont les priorités opératoires : `5 + 6 / 2` ne sera pas évalué de la même manière que `(5 + 6) / 2`. Vous devez bien sûr prendre ça en considération lors de l'écriture de votre algorithme.

Bonne chance à tous et amusez vous bien. :slightly_smiling_face:

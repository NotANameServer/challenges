# Solutions PHP au Défi 1 - Algorithme de tri

Il y a eu un total de 2 participants:

* [`Hesode#0487 (@391358353839882253)`](./Hesode)
* [`Superkooka#0612 (@283933553975230464)`](./superkooka)

## Résultats pour Hesode#0487:
| Test                       | Temps (ms) |
|:-------------------------- |:----------:|
| testEmptyArray             | 0          |
| testDefaultArray           | 0          |
| testOrderedIntArray2Pow10  | 9          |
| testReversedIntArray2Pow10 | 9          |
| testRandomArray2Pow10      | 9          |
| testOrderedIntArray2Pow15  | 1 048      |
| testReversedIntArray2Pow15 | 1 060      |
| testRandomArray2Pow15      | 713        |
| testOrderedIntArray2Pow16  | 2 871      |
| testReversedIntArray2Pow16 | 2 879      |
| testRandomArray2Pow16      | 716        |
| testOrderedIntArray2Pow17  | 8 672      |
| testReversedIntArray2Pow17 | 7 891      |
| testRandomArray2Pow17      | 1 498      |
| testOrderedIntArray2Pow20  | 189 019    |
| testReversedIntArray2Pow20 | 182 833    |
| testRandomArray2Pow20      | 19 038     |

Quantitée maximale de RAM utilisée: 158 Mio

**Méthode:** Tree sort

**Commentaire**:
Code très propre, correctement indenté et cohérent dans l'ensemble. La partie tests unitaires est apréciable.
Il manque juste quelques commentaires dans la classe IntTreeSort, et l'utilisation d'un while(true) qui est bofbof.

## Résultats pour Superkooka#0612:

### solution 1

| Test                       | Temps (ms) |
|:-------------------------- |:----------:|

En attente des retours de Kooka a ce sujet car on a un grand écart de temps. Et je ne vois pas le soucis niveau code. 

**Méthode:** Selection sort

**Commentaire**

Algo très basique mais qui fait le taff, globalement le code est propre, juste du chipotage à quelques endroits niveau code. 


### solution 2

| Test                       | Temps (ms) |
|:-------------------------- |:----------:|

En attente des retours de Kooka a ce sujet car on a un grand écart de temps. Et je ne vois pas le soucis niveau code. De mon côté quasis aucune différence niveau temps de trie par rapport à la première solution en algo pur. 

**Méthode:** Selection sort en mode triche

**Commentaire**

Pour sa deuxième solution, il utilise la fonction usort de php qui fait trie un tableau en passant une fonction callback. Donc php fait aussi du taff de son côté.  Et pour le reste rien à redire niveau code.


### Conclusion

Comme selection sort est un algo très simple, j'ai proposé à kooka de regarder d'autres algo plus costaud, comme le trie avec des arbres.  





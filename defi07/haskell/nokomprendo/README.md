# Calculatrice

## Description des différentes implémentations

- [Level1String.hs](Level1String.hs) : calculatrice postfixe avec évaluation
  directe

- [Level1Token.hs](Level1Token.hs) : idem mais avec une étape de découpage en
  token (pas vraiment utile mais ça permet d'introduire les versions avec
  Shunting-Yard)

- [Level2Statet.hs](Level2Statet.hs) : calculatrice infixe, avec combinateurs
  de parseurs implémentés par un StateT (transformateur de monades).

- [Level2Adt.hs](Level2Adt.hs) : idem mais avec des types et instances customs
  à la place du StateT (si on veut vraiment s'interdire la MTL)

- [Level2ShuntingYard.hs](Level2ShuntingYard.hs) : calculatrice infixe
  implémentée par un découpage/traitement de tokens puis Shunting-Yard pour
  passer en postfixe puis évaluation postfixe.

- [Level2ShuntingYardSpaces.hs](Level2ShuntingYardSpaces.hs) : idem mais en
  considérant que les opérations sont séparées par des espaces (le code est
  plus simple mais on gère moins de cas d'utilisation; par exemple `12 * (7 -
  4)` mais pas `12*(7-4)`)

## Exemple d'utilisation

```
$ runghc Level2Statet.hs 

> 1 + 2 * 3
Just (7.0,"")

> 2 * 3 + 1
Just (7.0,"")

> -2.3!--2
Just (3.0,"")

> foobar
Nothing

> 
```


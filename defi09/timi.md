# Puissance 4 en réseau

Protocole pour jouer au puissance 4 en réseau avec des 
spectateurs, indépendant de la couche application.

Il peut s'implémenter avec toute application qui implémente
la notion de salon, de message broadcast, de message privé
et de privilège utilisateur. On peut citer IRC, discord...

Il utile des messages publics (par exemple PRIVMSG), 
sans nécessiter de modifier les protocoles sous-jacent
en question.

## Vocabulaire
* Arbitre: utilisateur qui a le pouvoir de créer des canaux
et qui gère les parties

## Fonctionnement
Le protocole nécessite un environnement qui gère des
utilisateurs, des privilèges utilisateur et des salons.

Un arbitre créé des salons à sa propre
initiative selon la manière propre à l'environnement.

Des utilisateurs normaux qui souhaitent jouer peuvent découvrir
les salons disponibles selon une manière propre à chaque environnement.
Sur IRC en envoyant une commande `/list`, sur discord, en regardant
si il existe des canaux qui servent à jouer au p4 sur la colonne de 
gauche.

Ils peuvent regarder qui se trouve dans les salons pour 
choisir leur adversaires, ca fait office de matchmaking.
Les deux premiers utilisateurs qui rejoignent un salon 
sont automatiquement des joueurs. Ceux qui rejoignent le 
salon après sont des spectateurs et leurs messages sont ignorés.

Lorsque deux personnes au minimum entrent dans un salon
la partie commence et le'arbitre envoie un broadcast
`command-task-play` dans le salon pour dire qui joue 
(le premier à jouer peut être tiré au sort, ou être le 
premier à être entré dans le salon, ou le joueur qui 
arrive en premier dans l'ordre alphabétique...), le temps
de réflexion autorisé, en secondes, ainsi que le dernier
coup joué; comme aucun coup n'a encore été joué, il met 0 ici.

 `PRIVMSG #salon1 play tata 15 0`
 
Le joueur tata a donc 15 secondes pour jouer un coup. Il
joue ainsi avec une commande `answer-play`

 `PRIVMSG #salon1 play 4`

pour jouer à la quatrième colonne. La numérotation commence
à 1, en suivant l'ordre ou le damier est représenté en mémoire,
indépendament du sens d'affichage.

Si le joueur n'a pas répondu dans les temps la partie
se termine.

Tout message qui ne respecte pas la grammaire ou qui
ne respecte pas les règles du jeu en cours est ignoré.
Un client peut détecter que son message est valide 
si l'arbitre répond par une commande 
`command-task-play` comprennant le joueur suivant et
la colonne où il a joué.

 `PRIVMSG #salon1 play titi 15 4`

La commande `command-task-play` comprend la dernière
colonne jouée au cas où un des jouers cherche à "flood"
le canal de commandes `answer-play`, de sorte que l'autre
joueur ne sait pas quelle est la bonne, celle qui a été
enregistrée par l'arbitre.

Lorsqu'une partie se termine, soit par une victoire, 
soit par une égalité, soit car un joueur a été disqualifié
pour n'avoir pas joué à temps, l'arbitre envoie un
broadcast `end` avec optionellement la raison.

Exemple
```
PRIVMSG #salon1 end le joueur 1 a été déconnecté
PRIVMSG #salon1 end server error
PRIVMSG #salon1 end player 2 win
PRIVMSG #salon1 end
```

C'est aux clients de détecter qui a gagné mais l'arbitre
peut également ajouter un texte explicatif.

Au cours de la partie le serveur peut broadcast dans le format
qui lui semble le plus lisible pour les humains l'état de
la partie en cours ou terminée, de la manière qu'il 
veut, par exemple après chaque coup, ou bien en un seul tenant 
à la fin de la partie, d'une manière qui n'entre pas en
conflit avec le protocole.

A la fin d'une partie le salon peut être archivé, et d'autres 
créés au besoin.

## Grammaire
```abnf
command = command-task-play \   ; super users -> broadcast
          command-play \        ; player -> broadcast
          command-end \         ; super user -> broadcast


player = ALPHA *8(ALPHA / DIGIT)

room = ALPHA *8(ALPHA / DIGIT)

column = ("1" / "2" / "3" / "4" / "5" / "6" / "7")

command-task-play = "play" SP player SP DIGIT *(DIGIT) SP column CRLF

answer-play = "play" SP column CRLF

command-end = "end" [*(ALPHA / SP)] CRLF
```

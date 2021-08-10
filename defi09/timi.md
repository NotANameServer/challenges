# Puissance 4 en reseau

Protocole pour jouer au puissance 4 en réseau avec des 
spectateurs, indépendant de la couche application.

Il peut s'implémenter avec toute application qui implémente
la notion de salon, de message broadcast, de message privé
et de privilège utilisateur. On peut citer IRC, discord...

Il passe via des messages publics (par exemple PRIVMSG), 
sans nécessiter de modifier les protocoles en question.

## Fonctionnement
Le protocole nécessite un environnement qui gère des
utilisateurs, des privilèges utilisateur et des salons.

Un super utilisateur peut créer des salons à sa propre
initiative selon la manière propre au protocole applicatif.

Des utilisateurs normaux qui souhaitent jouer envoient un message
privé à un super utilisateur.

 `list games`
 
Par exemple dans un environnement IRC, si le super 
utilisateur s'appele toto et le joueur tata, le message
est ainsi

 `PRIVMSG toto list games`

Le super utilisateur répond le nombre de salons disponibles
puis une liste séparée soit par des sauts de ligne `\r\n`, 
soit via des messages répétés si le protocole ne permet pas de 
saut de ligne dans les messages une liste de salons qui 
permettent de jouer au puissance 4.

Dans le cas d'IRC
```
PRIVMSG tata 4
PRIVMSG tata salon1
PRIVMSG tata salon2
PRIVMSG tata salon3
PRIVMSG tata salon4
```

Un salon disponible est un salon qui n'est pas rempli 
(où une partie n'est pas déjà en cours)

Le joueur tata peut alors rejoindre un salon. Les deux premiers
utilisateurs qui rejoignent un salon sont automatiquement
des joueurs. Ceux qui rejoignent le salon après sont
des spectateurs et leurs messages sont ignorés.

Lorsque deux personnes au minimum entrent dans un salon
la partie commence et le serveur envoie un broadcast
`play` dans le salon pour dire qui joue (le premier à jouer
peut être tiré au sort, ou être le premier à être
entré dans le salon, ou le joueur qui arrive en premier
dans l'ordre alphabétique...), et le temps
de réflexion autorisé, en secondes.

 `PRIVMSG #salon1 play tata 15`
 
Le joueur tata a donc 15 secondes pour jouer un coup. Il
joue ainsi

 `PRIVMSG #salon1 play 4`

pour jouer à la quatrième colonne. La numérotation commence
à 1.

Si le joueur n'a pas répondu dans les temps le super 
utilisateur peut soit jouer un coup aléatoire à sa place, 
soit le déclarer perdant. Un joueur peut se déconnecter
et se reconnecter dans le temps imparti 
sans que la règle précédente ne s'annule.

Lorsqu'une partie se termine, soit par une victoire, 
soit par une égalité, soit car un joueur a été pour n'avoir
pas joué à temps, le super utilisateur envoie un
broadcast `end`.

 `PRIVMSG #salon1 end`

C'est aux clients de détecter qui a gagné.

Le serveur PEUT mettre le salon en lecture seule lorsqu'une
partie est terminée.

Le serveut PEUT archiver le salon en question pour permettre
à quiconque de revoir la partie jouée.

Par souci d'économie de ressources le serveur PEUT supprimer
le salon et l'historique des messages postés dans le salon
lorsque la partie est terminée, après un délai ou immédiatement.

Au cours de la partie le serveur PEUT broadcast dans le format
qui lui semble le plus lisible pour les humains l'état de
la partie en cours ou terminée, de la manière qu'il 
veut, par exemple après chaque coup, ou bien en un seul tenant 
à la fin de la partie.

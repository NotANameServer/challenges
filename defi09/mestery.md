# Protuissance 4

Ce document documente (oui) les détails techniques du protocole "Protuissance 4", un protocole permettant de jouer au Puissance 4 en réseau, via un serveur.

Ce protocole repose sur TCP. Toutefois le serveur peut en option également supporter des connections WebSocket, notamment pour supporter les clients web.

## Constants
- `CONNECT: 0`
- `INIT: 1`
- `GAME_START: 2`
- `PLAY_MOVE: 3`
- `GAME_END: 4`
- `JOIN_QUEUE: 5`
- `QUIT: 6`

### Erreurs

- `ERR_INVALID_NICK: 7`
- `ERR_ALREADY_USED_NICK: 8`
- `ERR_INVALID_COLUMN: 9`
- `ERR_TIMEOUT: 10`

## Définitions
Ce document utilise la grammaire [ABNF](https://datatracker.ietf.org/doc/html/rfc5234).

```abnf
message = message-connect /
          message-init /
          message-game-start /
          message-play-move /
          message-game-end /
          message-join-queue /
          message-quit /
          message-error

message-error = ( "7" /  "8" / "9" / "10" ) CRLF ; respectively ERR_INVALID_NICK, ERR_ALREADY_USED_NICK, ERR_INVALID_COLUMN, ERR_TIMEOUT

nick = 1*16( ALPHA / DIGIT / "-" / "_" )
message-connect = "0" nick CRLF

width = OCTET ; uint8 number
height = OCTET ; uint8 number
message-init = "1" width height CRLF

first-player-name-length = OCTET ; uint8 number (1-16)
message-game-start = "2" first-player-name-length nick nick CRLF ; player 0's name, player 1's name

column = OCTET ; uint8 number (must be between 0 and width-1)
message-play-move = "3" column CRLF

message-game-end = "4" [ BIT ] CRLF ; winner player index or none

message-join-queue = "5" CRLF

message-quit = "6" CRLF
```

### Timeouts
*Ces valeurs (par défauts) peuvent varier en fonction de la configuration du serveur.*

timeout = 3s
choice_timeout = 10s

## Client

Lorsque le client doit se connecter pour n'importe quelle raison, il doit envoyer un `message-quit`, puis doit se déconnecter.

### Connexion
Pour se connecter au serveur, le client doit envoyer un `message-connect` suivi du *pseudo* contenant seulement des caractères alphanumériques (`[A-Za-z0-9_-]`), entre 1 et 16 caractères inclus.

Si la connexion a réussi, le client reçoit un `message-init` avec la largeur et longueur du jeu.

### Partie
Lorsque le serveur trouve un adversaire, le client reçoit alors un `message-game-start` :

Le client 0 a alors `timeout + 1` secondes pour jouer. Le client 1 doit répondre avant `timeout` secondes, et ainsi de suite, jusqu'à la fin de la partie.

#### Jouer un coup
Pour jouer un coup, le client doit envoyer un `message-play-move`, avec le numéro de la colonne où il souhaite déposer son pion, le numéro doit être valide (c'est-à-dire que la colonne doit existé et ne doit pas être plaine), entre `0` et `width` non inclus.

Si la colonne est invalide, le client reçoit une erreur `ERR_INVALID_COLUMN`, il a 1 seconde pour renvoyer un coup. Si le coup est encore invalide, le client est déconnecté du serveur, avec la même erreur, et l'adversaire remporte la partie.

#### Fin de partie
Lorsqu'une personne gagne, lorsque la partie nulle, ou bien lorsqu'un des clients se déconnecte, la fin de la partie est annoncé via un `message-game-end` suivi de l'indice (0 ou 1) du joueur gagnant, ou rien s'il y a partie nulle.

Après avoir reçu ce message, chaque client doit explicitement soit se réinscrire à la fil d'attente (`message-join-queue`), soit quitter le serveur (`message-quit`). En cas de non réponse au bout de `choice_timeout`, le client est déconnecté.

## Serveur

*La plupart des `message`s qui doivent être envoyés au client ont été définis dans la partie client ci-dessus.*

Au démarrage, le serveur doit définir la largeur et hauteur du jeu. Ces derniers doivent être inclus entre 6 et 255.

### Connexion
Le serveur doit vérifier si un pseudo d'au moins un caractère est fourni, et contenant uniquement des caractères alphanumériques (voir ci-dessus) et doit envoyer une erreur `ERR_INVALID_NICK` en cas d’invalidité. Le serveur doit lire le pseudo jusqu'à `min(32, buf.length - 1)` (c'est-à-dire que le pseudo est tronqué, et qu'aucune erreur n'est envoyé).
Le pseudo doit être unique, dans le cas contraire une erreur `ERR_ALREADY_USED_NICK` doit être envoyé.

Si tout va bien, le serveur inscrit automatiquement le client sur la liste d'attente.

### Matchmaking
Le serveur doit faire affronter les clients sur la liste d'attente. Idéalement, il doit composer des parties pertinentes (par exemple, ne pas retomber contre le même client que la partie précédente). L'implémentation de cette partie (matchmaking) est libre.

### Partie
Lorsque le serveur crée une partie, il doit envoyer le pseudo du client 0 (celui qui commencera), suivi du pseudo du client 1. Le choix du client 0 et 1 est libre, cela peut par exemple être de l'aléatoire.
Le serveur doit ensuite initialiser le jeu dans une [Bitboard](https://en.wikipedia.org/wiki/Bitboard) (pour chaque joueur).

Comme décrit de la partie client, chaque joueur à un maximum de `timeout` secondes pour joueur (sauf pour le premier coup ou il a `timeout + 1`). En cas d'excès, le serveur doit déconnecter le client avec l'erreur `ERR_TIMEOUT`.

Si le client envoie un coup invalide, le serveur doit refuser le coup et envoyer une erreur comme décrit dans la partie client. Si cela ce reproduit (pour le même tour, pas toute la partie), il doit directement déconnecter le client avec cette même erreur.

Lorsque la partie se finit (déconnexion, gain ou partie nulle), le serveur l'annonce, et doit attendre le choix du client (continuer ou quitter), en prenant en compte le `choice_timeout`.

### Et ainsi de suite.

## Note
Ce protocole est expressément (et parfois inutilement ?) compliqué, notamment pour introduire des formats de sérialisations de données (en l’occurrence ici, un format non-textuel, crée sur-mesure) peu habituels pour certains (cela change du JSON par exemple !).

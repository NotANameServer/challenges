# Protocole réseau pour Puissance 4 


## Résumé

Il s'agit d'un protocole de communication réseau qui permet de confronter des
joueurs de Puissance 4 (serveur unique, clients multiples). Quand un client se
connecte au serveur, il y a une étape de connexion puis le serveur le contacte
pour initier et dérouler des parties de jeu. Un client ne joue qu'à une seule
partie à la fois. Le serveur gère les parties en essayant de faire jouer tous
les clients entre eux. Les clients doivent donc rester connectés en attendant
qu'une partie soit lancée (ou qu'un nouveau client arrive).

[exemple d'implémentation](https://github.com/nokomprendo/not-a-connect4)


## Protocole

### Généralités

- communication par websocket

- messages textes terminant par " \n"

- une partie débute avec un temps de réflexion donné (par exemple 42s pour
  chaque joueur)

- le temps restant au joueur pour la partie est indiqué quand le serveur
  demande de jouer un coup

- si un client joue un coup invalide, le serveur lui redemande de jouer et
  compte une pénalité de temps (1s) en plus du temps consommé

- en cas de timeout, le serveur envoie un message de fin de partie aux deux
  joueurs (le joueur timeout perd la partie); le serveur peut éventuellement
  relancer une autre partie ensuite

- en cas de déconnexion d'un client qui joue, il perd la partie


### Paramètres

- username (sans espacement : ' ', \t, \n, \r)

- BOARD: (. | R | Y)x42                         ; ligne0, ligne1...

- PLAYER: R | Y

- MOVE: 0-6                                     ; numéro de la colonne

- STATUS: WinR | WinY | Tie | PlayR | PlayY

- GAMESTATUS: Ok | Timeout


### Messages

- légende:

    - s2c = du serveur à un client
    - c2s = d'un client au serveur

- messages de connexion:

    - `connect <username> \n` (c2s)
    - `connected [welcome message] \n` (s2c)
    - `not-connected [error message] \n` (s2c)

- messages de jeu:

    - `newgame <user> <user> \n` (s2c)
    - `genmove BOARD COLOR <time> \n` (c2s; COLOR indique le joueur courant)
    - `playmove MOVE \n` (c2s)
    - `endgame BOARD COLOR STATUS GAMESTATUS \n` (s2c)


## Discussion

Le client peut gérer son temps au cours de la partie, par exemple en estimant
le nombre de coups potentiels restant.

Après l'étape de connexion, le client répond aux messages `genmove` du serveur.
Cependant comme le serveur vérifie le temps, il peut envoyer un `endgame` à
tout moment et recommencer une nouvelle partie peu de temps après. Typiquement,
on implémentera donc le calcul d'un coup dans un thread spécifique, qu'on
interrompera en cas de timeout.


## Exemples

### Séquence ok

![](uml/sequence_ok.svg)

```
connect client1
connected hello client1
connect client2
connected hello client2
newgame client1 client2
newgame client1 client2
genmove .......................................... R 21.0
playmove 1
genmove .R........................................ Y 21.0
playmove 1
...
endgame .RRRR...YYY............................... R WinR Ok
endgame .RRRR...YYY............................... Y WinR Ok
```


### Séquence avec timeout

![](uml/sequence_ko.svg)

```
connect client1
connected hello client1
connect client2
connected hello client2
newgame client1 client2
newgame client1 client2
genmove .......................................... R 21.0
playmove 1
genmove .R........................................ Y 21.0

endgame .R........................................ R WinR Timeout
endgame .R........................................ Y WinR Timeout
```



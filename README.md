# Défis de Programmation

Les ambassadeurs vous présentent un nouveau concept pour venir égailler la vie
du serveur : des défis de programmation.

Généralement sur une durée d’un weekend ou de 10 jours, nous vous proposons un
sujet ou une question à résoudre avec un programme ou une fonction écrit dans
le langage de votre choix.

Vous pouvez discuter avec les autres participants dans vos salons de
développement, partager vos idées et demander de l’aide comme d’habitude. Tout
ce que nous vous demandons est de ne pas gâcher le défi en envoyant une
solution avant la fin du sujet.

Lorsque vous avez une solution, nous vous invitons à l’envoyer aux ambassadeurs
de votre salon par message privé. C’est eux qui s’occupent de vous donner un
retour préliminaire sur votre travail et de compiler les solutions de leur
communauté.

Au terme du défi, tout le monde se réuni en vocal et nous invitons l’un ou
l’autre participant à prendre la parole pour expliquer sa solution.

Vous l’aurez compris, ces défis de programmation sont essentiellement un moment
d’entre-aide et de partage. Nous espérons que tout le monde pourra s’y
retrouver, aussi bien les tenors que les membres plus discrets, aussi bien les
gourous que les débutants. En ce sens, ces défis ne sont pas une compétition,
ni entre membres, ni entre langages (modulo troll :smile:). Le seul prix à
gagner est de passer du bon temps et d’apprendre des nouvelles choses.

## Soumettre sa participation

Pour soumettre votre participation, nous vous invitons à faire une PR
avec votre projet (soumission) dans `defiXX/<langage>/<pseudo>`,
`<pseudo>` peut être un dossier ou un fichier.

Si vous le souhaitez, vous pouvez utiliser un [sous-module Git](https://git-scm.com/docs/git-submodule),
pointant vers votre repo au lieu de directement inclure votre soumission,
dans ce cas, vous devez l'ajouter de cette façon :

```sh
# clonez votre fork (si ce n'est pas déjà fait)
$ git clone https://github.com/<pseudo>/challenges
$ cd challenges

# créez une branche à partir de master
$ git checkout -b <nom de la branche> master

# ajoutez votre sous-module (repo)
$ git submodule add <lien du repo git> defiXX/<langage>/<pseudo>

# commitez et poussez les changements
$ git commit -am "..."
$ git push origin <nom de la branche>
```

Toutefois, pour faciliter la tâche des correcteurs,
nous vous recommandons de ne pas utiliser de sous-module.

## Les ambassadeurs et assistants

Tous les langages ne sont pas encore représentés, n'hésitez pas à soumettre
votre candidature pour devenir ambassadeur et rejoindre l'aventure !

| Langages | Correcteurs |
| -------- | ----------- |
| asm | *personne* |
| c | gbdivers#9340 |
| cpp | gbdivers#9340 |
| csharp | Kaktus#0006 |
| html-css | *personne* |
| java | florent#8092 |
| javascript-typescript | Romain Lanz#0042 et Mestery#3859 |
| lua | *personne* |
| php | Mjöllnir#3515 et Cara#0385 |
| python | Dr Lazor#6737 |
| rust | Wafelack#6513 |

## La liste des défis

1. [Algorithme de tri](defi01)
2. [Génération de Labyrinthe](defi02)
3. [Résolution de Labyrinthe](defi03)
4. [Implémentation d'un serveur HTTP](defi04)
5. [Automates cellulaires](defi05)
6. [Client IRC](defi06)

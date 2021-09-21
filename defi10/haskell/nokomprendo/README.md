
# Boules 2D


## Description

- simulation de boules 2D en mouvement

- collisions entre boules et avec les bords de la fenêtre

- touche espace pour réinitialiser une scène aléatoire (algorithme d'échantillonnage par rejet)

- touche echap pour quitter

- masses des boules proportionnelles à leur surface


## Lancement

- avec cabal+nix : `nix-shell --run "cabal run"`

- avec stack : `stack run`


## Détection de collisions

- animer la scène d'une durée T (avec détection de collisions a priori) : 

    - trouver l'intersection qui arrive le plus tôt (à la durée Ti)
    - calculer le rebond et déplacer tous les objets d'une durée Ti
    - recommencer jusqu'à arriver à la durée T

- calcul de collision entre deux boules : <https://www.f-legrand.fr/scidoc/docmml/sciphys/meca/collidisques/collidisques.html>


#ifndef LABYRINTHE_H
#define LABYRINTHE_H

#include <string>
#include <vector>
#include <iostream>

namespace maze {

  /**
   * 
   * Permet de connaître l'état de la case du labyrinthe.
   * 
   * WALL : C'est un mur impossible de s'y rendre.
   * FREE : C'est libre on peut y aller.
   * END : C'est une des sorties du labyrinthe.
   * ALREADY_SEEN : ont est déjà allé sur cette case. 
   */
  enum typeCase {WALL, FREE, END, ALREADY_SEEN};

  /**
   * 
   * Un Labyrinthe :
   * La chaine de caractères est constituée uniquement des caractères "# .\n".
   * Le dièse pour un mur, l'espace pour une case vide,
   * le point pour le point de départ et le retour à la ligne au format UNIX comme fin de ligne.
   * 
   * Le labyrinthe doit être entièrement entouré d'un mur 
   * à l'exception d'une seule case qui montre la sortie et doit être soluble (et pas dans l'eau).
   * 
   * Le départ se trouve en position (x=1, y=1) et la sortie se trouve en (x=4998, y=4997).
   * 
   */
  class Labyrinthe {

  std::vector<std::vector<int> > labyrinthe;

  std::string filePath;

  std::pair<int,int> start;

  std::vector<std::vector<std::pair<int,int> > > parcours;

  bool load(std::string filePath, int x=1, int y=1);

  /**
   * 
   * Permet de trouver la sortie.
   * 
   * Les coordonnées sont sauvegarder dans l'attribut "parcours". 
   * 
   */
  bool goOut();

  /**
   * 
   * Affiche le labyrinthe et le chemin de sorti dans la console.
   * 
   * !!!Attention!!! : Affichage lisible uniquement pour un labyrinthe de petite taille par exemple 97 colonne et 33 lignes !
   * 
   */
  void afficherResultat();

  /**
   * 
   * Affiche le labyrinthe dans la console.
   * 
   * !!!Attention!!! : Affichage lisible uniquement pour un labyrinthe de petite taille par exemple 97 colonne et 33 lignes !
   * 
   */
  void afficher();

  //TODO faire une fonction move pour éviter le copier coller
  //qui prendra en paramètre une direction (up, down, left ou right)
  /**
   * 
   * Renvoie les coordonnées de la case voisine si elle est disponnible.
   * C'est à dire pas déjà visité ou si ce n'est pas un mur.
   * 
   * Sinon renvoie la coordonnée (-1,-1).
   * 
   */
  std::pair<int,int> up();
  std::pair<int,int> down();
  std::pair<int,int> left();
  std::pair<int,int> right();

  public :

  /**
   * 
   * Constructeur par défaut qui initialise les attributs labyrinthe(0) et start(1,1). 
   * 
   */
  Labyrinthe();


  /**
   * 
   * L'algorithme du petit poucet ;).
   * Ce n'est pas le chemin le plus court car il aime faire du sport x).
   *  
   */
  void solveMaze(std::string filePath, int x=1, int y=1);

  /**
   * 
   * Permet d'enregistrer le labyrinthe et le parcours pour sortir dans une image png.
   * 
   * TODO : en cours de dev. 
   */
  void saveImg(std::string fileImg);

  /**
   * 
   * Permet de sauvegarder les coordonnées de sortie du labyrinthe sous la forme (x,y), dans un fichier text.
   * L'extention ".coord" est rajoutée au fichier passé en paramètre.
   * 
   */
  void saveCoord(std::string fileCoord);

  };

}
#endif
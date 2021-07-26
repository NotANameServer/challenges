#include "labyrinthe.hpp"
#include <fstream>

maze::Labyrinthe::Labyrinthe() : start(1,1), labyrinthe(0) {
  
}

bool maze::Labyrinthe::load(std::string filePath, int x, int y) {
  this->filePath = filePath;
  std::ifstream fileLabirynthe(filePath);

  std::string ligne;
  while(getline(fileLabirynthe,ligne)) {             
    if(ligne.size()>1) {            
      labyrinthe.push_back(std::vector<int>());
    }
    for(int i=0; i < ligne.size(); ++i) {      
      switch (ligne[i])
      {
      case '#':
        labyrinthe[labyrinthe.size()-1].push_back(int(WALL));
        break;
      case ' ':
        //si on est aux extrémitées ouest, est et nord du labyrinthe alors c'est une sortie
        if ((i==0) or (i==(ligne.size()-1)) or (labyrinthe.size()==1)) {
          labyrinthe[labyrinthe.size()-1].push_back(int(END));          
          std::cout << "Arrivé : (" << i << ";" << labyrinthe.size()-1 << ")" << std::endl;
          break;
        }
        labyrinthe[labyrinthe.size()-1].push_back(int(FREE));
        break;
      case '.':
        start = std::make_pair(i,labyrinthe.size()-1);
        labyrinthe[labyrinthe.size()-1].push_back(int(ALREADY_SEEN));
        break;
      case '\n':
        /* nothing */
        break;
      default:
        return false;
        break;
      }
    }    
  }

  //TODO optimiser pour éviter de refaire une boucle
  //permet de regarder s'il y a une sortie au sud du labyrinthe et l'affecter.
  for (int i=0; i < labyrinthe[labyrinthe.size()-1].size(); ++i) {
    if (labyrinthe[labyrinthe.size()-1][i] == FREE) {
      labyrinthe[labyrinthe.size()-1][i] = END;
      std::cout << "Arrivé : (" << i << ";" << labyrinthe.size()-1 << ")" << std::endl;
    }
  }

  return true;
}

void maze::Labyrinthe::solveMaze(std::string filePath, int x, int y) {
  if (!load(filePath,x,y)) {
    std::cout << "Impossible de charger le fichier : " << filePath << std::endl;
    //TODO  : gérer une exception throw
    return;      
  }
    
  std::cout << "Labyrinthe chargé : nb col " << labyrinthe[0].size() << " - nb ligne " << labyrinthe.size();
  std::cout << " - départ (" << start.first << ";" << start.second << ")." << std::endl;
  //afficher();

  std::vector<std::pair<int,int> > parcour;
  parcour.push_back(std::make_pair(start.first,start.second));
  parcours.push_back(parcour);

  if (goOut()) {
    std::cout << "You're free" << std::endl;    
    //afficherResultat();
  } else {
    std::cout << "You're lost" << std::endl;
    //afficher();
  }

}

std::pair<int,int> maze::Labyrinthe::up() {
  //dernier parcours et dernière coord de ce parcours
  int x = parcours[parcours.size()-1][parcours[parcours.size()-1].size()-1].first;
  int y = parcours[parcours.size()-1][parcours[parcours.size()-1].size()-1].second-1;

  if (y<0 or labyrinthe[y][x]==WALL or labyrinthe[y][x]==ALREADY_SEEN) {
    return std::make_pair(-1,-1);
  } else {    
    return std::make_pair(x,y);
  }
}

std::pair<int,int> maze::Labyrinthe::down() {
  int x = parcours[parcours.size()-1][parcours[parcours.size()-1].size()-1].first;
  int y = parcours[parcours.size()-1][parcours[parcours.size()-1].size()-1].second+1;

  if (y>=labyrinthe.size() or labyrinthe[y][x]==WALL or labyrinthe[y][x]==ALREADY_SEEN) {
    return std::make_pair(-1,-1);
  } else {    
    return std::make_pair(x,y);
  }
}

std::pair<int,int> maze::Labyrinthe::left() {
  int x = parcours[parcours.size()-1][parcours[parcours.size()-1].size()-1].first-1;
  int y = parcours[parcours.size()-1][parcours[parcours.size()-1].size()-1].second;

  if (x<0 or labyrinthe[y][x]==WALL or labyrinthe[y][x]==ALREADY_SEEN) {
    return std::make_pair(-1,-1);
  } else {    
    return std::make_pair(x,y);
  }
}

std::pair<int,int> maze::Labyrinthe::right() {
  int x = parcours[parcours.size()-1][parcours[parcours.size()-1].size()-1].first+1;
  int y = parcours[parcours.size()-1][parcours[parcours.size()-1].size()-1].second;

  if (x>=labyrinthe[y].size() or labyrinthe[y][x]==WALL or labyrinthe[y][x]==ALREADY_SEEN) {
    return std::make_pair(-1,-1);
  } else {    
    return std::make_pair(x,y);
  }
}

void maze::Labyrinthe::afficher() {
  for(int y=0; y<labyrinthe.size(); ++y) {
    for(int x=0; x<labyrinthe[y].size(); ++x) {
      switch (labyrinthe[y][x])
      {
      case WALL:
        std::cout << '#';
        break;
      case FREE or END:
        std::cout << ' ';
        break;
      case ALREADY_SEEN:
        std::cout << '.';
        break;
      default:
        break;
      }      
    }
    std::cout << std::endl;
  }
}

void maze::Labyrinthe::afficherResultat() {
  for(int y=0; y<labyrinthe.size(); ++y) {
    for(int x=0; x<labyrinthe[y].size(); ++x) {
      switch (labyrinthe[y][x])
      {
      case WALL:
        std::cout << '#';
        break;
      case FREE or END:
        std::cout << ' ';
        break;
      case ALREADY_SEEN:
        bool vue;
        vue = false;
        for(int j=0; j<parcours.size(); ++j) {          
          for(int i=0; i<parcours[j].size()-1; ++i) {            
            if (parcours[j][i].first==x and parcours[j][i].second==y) {
              std::cout << '.';
              vue = true;
              break;
            }
          }
        }
        //int j_max = parcours.size()-1;
        //int i_max = parcours[parcours.size()-1].size()-1;
        //if (parcours[j_max][i_max].first==x and parcours[j_max][i_max].second==y) {
        if (parcours[parcours.size()-1][parcours[parcours.size()-1].size()-1].first==x and parcours[parcours.size()-1][parcours[parcours.size()-1].size()-1].second==y) {
          std::cout << '.';
          vue = true;
          break;
        }

        if (!vue) {
          std::cout << ' ';
        }      
        break;
      default:
        break;
      }      
    }
    std::cout << std::endl;
  }
}

bool maze::Labyrinthe::goOut() {

  while (true) {

  int nbMove = 0;  
  std::vector<std::pair<int,int> > parcour_down(0);
  std::vector<std::pair<int,int> > parcour_left(0);
  std::vector<std::pair<int,int> > parcour_right(0);

  std::pair<int,int> next_case_up(-1,-1);  
  std::pair<int,int> next_case_down(-1,-1);
  std::pair<int,int> next_case_left(-1,-1);
  std::pair<int,int> next_case_right(-1,-1);

  next_case_up = up();
  if(next_case_up.first>-1) {        
    if (labyrinthe[next_case_up.second][next_case_up.first]==END) {      
      return true;
    }

    labyrinthe[next_case_up.second][next_case_up.first] = ALREADY_SEEN;

    ++nbMove;
  }

  next_case_down = down();
  if(next_case_down.first>-1) {    
    
    if (nbMove>0) {            
      parcour_down.push_back(std::make_pair(next_case_down.first,next_case_down.second));      
    } 
    
    if (labyrinthe[next_case_down.second][next_case_down.first]==END) {      
      return true;
    }

    labyrinthe[next_case_down.second][next_case_down.first] = ALREADY_SEEN;

    ++nbMove;

  }

  next_case_left = left();
  if(next_case_left.first>-1) {    

    if (nbMove>0) {            
      parcour_left.push_back(std::make_pair(next_case_left.first,next_case_left.second));      
    } 

    if (labyrinthe[next_case_left.second][next_case_left.first]==END) {      
      return true;
    }

    labyrinthe[next_case_left.second][next_case_left.first] = ALREADY_SEEN;

    ++nbMove;

  }

  next_case_right = right();
  if(next_case_right.first>-1) {    

    if (nbMove>0) {            
      parcour_right.push_back(std::make_pair(next_case_right.first,next_case_right.second));      
    }

    if (labyrinthe[next_case_right.second][next_case_right.first]==END) {      
      return true;
    }

    labyrinthe[next_case_right.second][next_case_right.first] = ALREADY_SEEN;

    ++nbMove;

  }

  if (next_case_up.first>-1) {
    parcours[parcours.size()-1].push_back(std::make_pair(next_case_up.first,next_case_up.second));
  }

  if (next_case_down.first>-1) {
    if(parcour_down.size()>0) {
      parcours.push_back(parcour_down);
    } else {
      parcours[parcours.size()-1].push_back(std::make_pair(next_case_down.first,next_case_down.second));  
    }      
  }

  if (next_case_left.first>-1) {
    if(parcour_left.size()>0) {
      parcours.push_back(parcour_left);
    } else {
      parcours[parcours.size()-1].push_back(std::make_pair(next_case_left.first,next_case_left.second));  
    }      
  }

  if (next_case_right.first>-1) {
    if(parcour_right.size()>0) {
      parcours.push_back(parcour_right);
    } else {
      parcours[parcours.size()-1].push_back(std::make_pair(next_case_right.first,next_case_right.second));  
    }      
  }

  if (nbMove == 0) {
    parcours.pop_back();
  }

  //plus de chemin on est en prison :(
  if(parcours.size()==0) {
    return false;
  }
  
  }  
}

void maze::Labyrinthe::saveImg(std::string fileImg) {
  std::ofstream image(fileImg,std::ios::out | std::ios::binary);  
  
  int magicNumberPNG[8] {137, 80, 78, 71, 13, 10, 26, 10};
  
  for(int i=0; i<8; ++i) {
    image.write((char *) &magicNumberPNG[i],1);
  }
  
}

void maze::Labyrinthe::saveCoord(std::string fileCoord) {
  std::ofstream fCoord(fileCoord + ".coord");

  if (!fCoord.is_open()) {
    std::cout << "Fichier invalide !" << std::endl;
    return;
  }

  for(int j=0; j<parcours.size(); ++j) {    
    for(int i=0; i<parcours[j].size()-1; ++i) {            
        fCoord << "(" << parcours[j][i].first << ";" << parcours[j][i].second << ")" << '\n';
    }
  }

  int j_max = parcours.size()-1;
  int i_max = parcours[parcours.size()-1].size()-1;
  fCoord << "(" << parcours[j_max][i_max].first << ";" << parcours[j_max][i_max].second << ")" << '\n';

}
#include <iostream>
#include <string>
#include "labyrinthe.hpp"
#include <chrono>

/**
 * 
 * g++ main.cpp labyrinthe.cpp -o main -std=c++17
 *  
 */
int main(int argc, char *argv[]) {  

  if (argc != 2) {
    std::cout << "Usage : " << argv[0] << " labyrinthe" << std::endl;
  }

  maze::Labyrinthe labyrinthe;
  const auto& start = std::chrono::high_resolution_clock::now();
  labyrinthe.solveMaze(argv[1]); 
  const auto& stop = std::chrono::high_resolution_clock::now();
  auto res = stop - start;
  std::cout << "time : " << res.count() << " ns." << std::endl;
    
  labyrinthe.saveCoord(argv[1]);  
  //TODO : en cours de dev
  //labyrinthe.saveImg("res.png");
}
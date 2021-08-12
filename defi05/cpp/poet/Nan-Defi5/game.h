#ifndef GAME_H
#define GAME_H

#include <QPixmap>
#include <QPainter>

#include "matrix.h"
#include "gamerules.h"

class Game
{
public:
    Game(int width, int height);

    void Update();
    void SetSize(int width, int height);

    Matrix<Gamerules::TileType>* GetData(){return &m_data;}
    Gamerules* GetGamerules(){return &m_gamerules;}

private:
    void ProcessGameFrame();

    int m_width;
    int m_height;

    Gamerules m_gamerules;
    Matrix<Gamerules::TileType> m_data;
};

#endif // GAME_H

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
    QPixmap* GetImage(){return &m_image;}

private:
    void ProcessGameFrame();
    void UpdateImage();

    int m_width;
    int m_height;
    QPixmap m_image;

    Gamerules m_gamerules;
    Matrix<Gamerules::TileType> m_data;
};

#endif // GAME_H

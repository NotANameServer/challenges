#include "game.h"

Game::Game(int width, int height)
    : m_image(1, 1)
    , m_data(1, 1, Gamerules::TileType::Ash)
{
    SetSize(width, height);
}

void Game::Update()
{
    ProcessGameFrame();
    UpdateImage();
}

void Game::SetSize(int width, int height)
{
    m_width = width;
    m_height = height;
    m_image = m_image.scaled(width, height);
    m_data.Resize(width, height, Gamerules::TileType::Ash);
    UpdateImage();
}


void Game::UpdateImage()
{
    QPainter painter(&m_image);
    for(int i = 0 ; i < m_width ; i++)
    {
        for(int j = 0 ; j < m_height ; j++)
        {
            auto color = m_gamerules.GetColor(m_data(i, j));
            painter.setPen(QPen(color));
            painter.drawPoint(i, j);
        }
    }
}

void Game::ProcessGameFrame()
{
    Matrix<Gamerules::TileType> newMat(m_data.width(), m_data.height());

    for(int i = 0 ; i < m_data.width(); i++)
    {
        for(int j = 0 ; j < m_data.height(); j++)
        {
            newMat(i, j) = m_gamerules.ProcessRules({m_data, i, j});
        }
    }

    m_data = newMat;

    UpdateImage();
}

#include "game.h"

Game::Game(int width, int height)
    : m_data(1, 1, 0)
{
    SetSize(width, height);
}

void Game::Update()
{
    ProcessGameFrame();
}

void Game::SetSize(int width, int height)
{
    m_width = width;
    m_height = height;
    m_data.Resize(width, height, 0);
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
}

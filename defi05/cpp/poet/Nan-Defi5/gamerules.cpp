#include "gamerules.h"

Gamerules::Gamerules(int seed)
    :m_rand(seed)
{
    InitRules();
}


QColor Gamerules::GetColor(const TileType & tile)
{
    switch(tile)
    {
    case TileType::Ash:
        return QColor(41, 41, 41);
    case TileType::YoungTree:
        return QColor(86, 217, 30);
    case TileType::OldTree:
        return QColor(9, 153, 6);
    case TileType::YoungFire:
        return QColor(255, 255, 33);
    case TileType::Fire:
        return QColor(255, 153, 0);
    case TileType::OldFire:
        return QColor(217, 22, 22);
    default:
        break;
    }

    return QColor(255, 0, 255);
}

void Gamerules::InitRules()
{
    m_rules.emplace_back(Tile::YoungTree, Tile::YoungFire, 0.01f, Tile::YoungFire, 1);
    m_rules.emplace_back(Tile::YoungTree, Tile::YoungFire, 0.02f, Tile::Fire, 1);
    m_rules.emplace_back(Tile::YoungTree, Tile::YoungFire, 0.01f, Tile::OldFire, 1);

    m_rules.emplace_back(Tile::OldTree, Tile::YoungFire, 0.1f, Tile::YoungFire, 1);
    m_rules.emplace_back(Tile::OldTree, Tile::YoungFire, 0.2f, Tile::Fire, 1);
    m_rules.emplace_back(Tile::OldTree, Tile::YoungFire, 0.1f, Tile::OldFire, 1);

    m_rules.emplace_back(Tile::YoungFire, Tile::Fire, 0.1f);
    m_rules.emplace_back(Tile::Fire, Tile::OldFire, 0.1f);
    m_rules.emplace_back(Tile::OldFire, Tile::Ash, 0.1f);

    m_rules.emplace_back(Tile::Ash, Tile::YoungTree, 0.001f);
    m_rules.emplace_back(Tile::YoungTree, Tile::OldTree, 0.005f);

    m_rules.emplace_back(Tile::OldTree, Tile::YoungFire, 0.0005f, Tile::OldTree, 5);
}


Gamerules::TileType Gamerules::ProcessRules(const MatrixView<TileType> & m_view)
{
    TileType old = m_view(0, 0);
    for(const auto & rule : m_rules)
    {
        if(old != rule.oldValue)
            continue;

        bool validCondition = true;
        if(rule.neighbourNb > 0)
        {
            validCondition = false;
            int neighbourNb = 0;
            for(int i = -1 ; i <= 1 ; i++)
            {
                for(int j = -1 ; j <= 1 ; j++)
                {
                    if(i == 0 && j == 0)
                        continue;
                    if(!m_view.ValidPos(i, j))
                        continue;
                    if(m_view(i, j) == rule.neighbour)
                        neighbourNb++;
                }
            }
            if(neighbourNb >= rule.neighbourNb)
                validCondition = true;
        }

        if(validCondition)
        {
            if(std::bernoulli_distribution(rule.proba)(m_rand))
                return rule.newValue;
        }
    }

    return old;
}

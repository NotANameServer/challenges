#include "gamerules.h"

Gamerules::Gamerules(int seed)
    :m_rand(seed)
{
    InitRules();
}

Tile* Gamerules::GetTile(int id)
{
    for(auto & t : m_tiles)
    {
        if(t.id == id)
            return &t;
    }
    return nullptr;
}

Tile* Gamerules::GetTile(QString name)
{
    for(auto & t : m_tiles)
    {
        if(t.name == name)
            return &t;
    }
    return nullptr;
}

std::vector<int> Gamerules::GetAllTilesID() const
{
    std::vector<int> ids;
    for(auto & t : m_tiles)
    {
        ids.push_back(t.id);
    }
    return ids;
}

int Gamerules::AddTile(QString name, QColor color)
{
    int id = 0;
    for(int i = 0 ; i < static_cast<int>(m_tiles.size()) ; i++)
    {
        if(m_tiles[i].id == id)
        {
            id++;
            i = -1;
        }
    }

    m_tiles.emplace_back(id, name, color);

    return id;
}

void Gamerules::RemoveTile(QString name)
{
    int index = GetTileIndex(name);
    if(index >= 0)
        RemoveTileImp(index);
}

void Gamerules::RemoveTile(int ID)
{
    int index = GetTileIndex(ID);
    if(index >= 0)
        RemoveTileImp(index);
}

void Gamerules::RemoveTileImp(int index)
{
    Tile* t = &m_tiles[index];
    m_rules.erase(std::remove_if(m_rules.begin(), m_rules.end(), [t](const Rule & r)
    {
        if(r.oldValue == t->id)
            return true;
        if(r.newValue == t->id)
            return true;
        if(r.neighbour == t->id)
            return true;
        return false;
    }), m_rules.end());

    m_tiles.erase(m_tiles.begin() + index);
}

int Gamerules::GetTileIndex(int id)
{
    for(int i = 0 ; i < static_cast<int>(m_tiles.size()); i++)
    {
        if(m_tiles[i].id == id)
            return i;
    }
    return -1;
}

int Gamerules::GetTileIndex(QString name)
{
    for(int i = 0 ; i < static_cast<int>(m_tiles.size()); i++)
    {
        if(m_tiles[i].name == name)
            return i;
    }
    return -1;
}

void Gamerules::RemoveRule(int index)
{
    m_rules.erase(m_rules.begin() + index);
}

void Gamerules::AddRule(int oldID, int newID, float proba)
{
    Tile *t = GetTile(oldID);
    if(t == nullptr)
        return;
    t = GetTile(newID);
    if(t == nullptr)
        return;

    m_rules.emplace_back(oldID, newID, proba);
}

void Gamerules::AddRule(int oldID, int newID, float proba, int neighbourID, int neighbourNbMin, int neighbourNbMax)
{
    Tile *t = GetTile(oldID);
    if(t == nullptr)
        return;
    t = GetTile(newID);
    if(t == nullptr)
        return;
    t = GetTile(neighbourID);
    if(t == nullptr)
        return;

    m_rules.emplace_back(oldID, newID, proba, neighbourID, neighbourNbMin, neighbourNbMax);
}

QColor Gamerules::GetColor(const TileType & tileID)
{
    Tile* tile = GetTile(tileID);
    if(tile != nullptr)
        return tile->color;

    return QColor(255, 0, 255);
}

void Gamerules::InitRules()
{
    m_tiles.clear();

    int ash = AddTile("Ash", {41, 41, 41});
    int youngTree = AddTile("Young Tree", {86, 217, 30});
    int oldTree = AddTile("Old Tree", {9, 153, 6});
    int youngFire = AddTile("Young Fire", {255, 255, 33});
    int fire = AddTile("Fire", {255, 153, 0});
    int oldFire = AddTile("Old Fire", {217, 22, 22});

    m_rules.clear();

    AddRule(youngTree, youngFire, 0.01f, youngFire, 1, 8);
    AddRule(youngTree, youngFire, 0.02f, fire, 1, 8);
    AddRule(youngTree, youngFire, 0.01f, oldFire, 1, 8);

    AddRule(oldTree, youngFire, 0.1f, youngFire, 1, 8);
    AddRule(oldTree, youngFire, 0.2f, youngFire, 1, 8);
    AddRule(oldTree, youngFire, 0.1f, youngFire, 1, 8);

    AddRule(youngFire, fire, 0.1f);
    AddRule(fire, oldFire, 0.1f);
    AddRule(oldFire, ash, 0.1f);

    AddRule(ash, youngTree, 0.001f);
    AddRule(youngTree, oldTree, 0.005f);

    AddRule(oldTree, youngFire, 0.0005f, oldTree, 5, 8);
}


Gamerules::TileType Gamerules::ProcessRules(const MatrixView<TileType> & m_view)
{
    TileType old = m_view(0, 0);
    for(const auto & rule : m_rules)
    {
        if(old != rule.oldValue)
            continue;

        bool validCondition = true;
        if(rule.neighbourNbMax > 0)
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
            if(neighbourNb >= rule.neighbourNbMin && neighbourNb <= rule.neighbourNbMax)
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

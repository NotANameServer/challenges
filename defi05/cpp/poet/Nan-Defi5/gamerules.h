#ifndef GAMERULES_H
#define GAMERULES_H

#include <QColor>

#include <vector>
#include <random>

#include "matrix.h"

enum Tile
{
    Ash,
    YoungTree,
    OldTree,
    YoungFire,
    Fire,
    OldFire
};

struct Rule
{
    Tile oldValue;
    Tile newValue;
    float proba;

    Tile neighbour;
    int neighbourNb;

    Rule(Tile _old, Tile _new, float _proba)
        :oldValue(_old), newValue(_new), proba(_proba), neighbour(Tile::Ash), neighbourNb(0)
    {}

    Rule(Tile _old, Tile _new, float _proba, Tile _neighbour, int _neighbourNb)
        :oldValue(_old), newValue(_new), proba(_proba), neighbour(_neighbour), neighbourNb(_neighbourNb)
    {}
};

class Gamerules
{
public:
    using TileType = Tile;

    Gamerules(int seed = 0);

    QColor GetColor(const TileType & tile);

    TileType ProcessRules(const MatrixView<TileType> & m_view);

private:
    void InitRules();

    std::vector<Rule> m_rules;
    std::mt19937 m_rand;
};

#endif // GAMERULES_H

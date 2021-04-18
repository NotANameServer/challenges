#ifndef GAMERULES_H
#define GAMERULES_H

#include <QColor>

#include <vector>
#include <random>

#include "matrix.h"

struct EnumClassHash
{
    template <typename T>
    std::size_t operator()(T t) const
    {
        return static_cast<std::size_t>(t);
    }
};

struct Tile
{
    int id;
    QString name;
    QColor color;

    Tile(int _id, QString _name, QColor _color)
        :id(_id), name(_name), color(_color)
    {}
};

struct Rule
{
    int oldValue;
    int newValue;
    float proba;

    int neighbour;
    int neighbourNbMin;
    int neighbourNbMax;

    Rule(int _old, int _new, float _proba)
        :oldValue(_old), newValue(_new), proba(_proba), neighbour(-1), neighbourNbMin(0), neighbourNbMax(8)
    {}

    Rule(int _old, int _new, float _proba, int _neighbour, int _neighbourNb)
        :oldValue(_old), newValue(_new), proba(_proba), neighbour(_neighbour), neighbourNbMin(_neighbourNb), neighbourNbMax(8)
    {}

    Rule(int _old, int _new, float _proba, int _neighbour, int _neighbourNbMin, int _neighbourNbMax)
        :oldValue(_old), newValue(_new), proba(_proba), neighbour(_neighbour), neighbourNbMin(_neighbourNbMin), neighbourNbMax(_neighbourNbMax)
    {}
};

class Gamerules
{
public:
    using TileType = int;

    Gamerules(int seed = 0);

    QColor GetColor(const TileType & tile);

    TileType ProcessRules(const MatrixView<TileType> & m_view);

    Tile* GetTile(int id);
    Tile* GetTile(QString name);
    Tile* GetTileFromIndex(int index){return &m_tiles[index];}
    std::vector<int> GetAllTilesID() const;
    int AddTile(QString name, QColor color);
    void RemoveTile(QString name);
    void RemoveTile(int ID);

    Rule* GetRule(int index){return &m_rules[index];}
    int GetRuleNb() const {return m_rules.size();}
    void RemoveRule(int index);

    void AddRule(int oldID, int newID, float proba);
    void AddRule(int oldID, int newID, float proba, int neighbourID, int neighbourNbMin, int neighbourNbMax);

private:
    int GetTileIndex(int id);
    int GetTileIndex(QString name);
    void RemoveTileImp(int index);

    void InitRules();

    std::vector<Tile> m_tiles;
    std::vector<Rule> m_rules;
    std::mt19937 m_rand;
};

#endif // GAMERULES_H

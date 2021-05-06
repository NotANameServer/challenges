#include "cell.h"

//Initialise les couleurs
const QVector<QColor> Cell::colors{QColorConstants::Svg::lightgreen, QColorConstants::Svg::darkgreen, QColorConstants::Svg::yellow, QColorConstants::Svg::red,
                      QColorConstants::Svg::orange, QColorConstants::Svg::black,QColorConstants::Svg::white};

// todo: prendre en compte à la place des couleurs
const QVector<QString> Cell::icons{"🌱","🌳","🎇","🔥","🧯","💀"," "};

//Initilalise les 6 états possibles avec leurs conditions de changement et leurs nouvels états le cas échéant.
const struct Etat Cell::etatYoung = {Forest::YOUNG, {{Forest::BEGIN_FIRE,1,qMakePair(1,100),Forest::BEGIN_FIRE},
                                                    {Forest::ON_FIRE,1,qMakePair(1,50),Forest::BEGIN_FIRE},
                                                    {Forest::END_FIRE,1,qMakePair(1,100),Forest::BEGIN_FIRE},
                                                    {Forest::ANY,0,qMakePair(1,200),Forest::OLD},}};

const struct Etat Cell::etatOld = {Forest::OLD, {{Forest::BEGIN_FIRE,1,qMakePair(1,10),Forest::BEGIN_FIRE},
                                                    {Forest::ON_FIRE,1,qMakePair(1,5),Forest::BEGIN_FIRE},
                                                    {Forest::END_FIRE,1,qMakePair(1,10),Forest::BEGIN_FIRE},
                                                    {Forest::OLD,5,qMakePair(1,20000),Forest::BEGIN_FIRE},}};

const struct Etat Cell::etatBegingFire = {Forest::BEGIN_FIRE, {{Forest::ANY,0,qMakePair(1,10),Forest::ON_FIRE}}};

const struct Etat Cell::etatOnFire = {Forest::ON_FIRE, {{Forest::ANY,0,qMakePair(1,10),Forest::END_FIRE}}};

const struct Etat Cell::etatEndFire = {Forest::END_FIRE, {{Forest::ANY,0,qMakePair(1,10),Forest::ASH}}};

const struct Etat Cell::etatAsh = {Forest::ASH, {{Forest::ANY,0,qMakePair(1,1000),Forest::YOUNG}}};

//Liste des 6 états pour récupérer les conditions de changement d'état en fonction d'un état.
//Ce qui évite de porter les conditions sur chaque forêt, car à partir de son état on peut retrouver les conditions grâce à cette liste.
const QVector<Etat> Cell::etats{Cell::etatYoung, Cell::etatOld, Cell::etatBegingFire, Cell::etatOnFire, Cell::etatEndFire, Cell::etatAsh};

/**
 * @brief Cell::Cell
 * @param coord
 * @param typeForest
 * @param parent
 *
 * Constructeur d'une forêt. C'est son point d'entrée à sa création.
 * Par défaut elle est initialisée à cendre.
 */
Cell::Cell(QPair<int,int> coord, Forest typeForest, QObject *parent) : QObject(parent), m_coord(coord), m_typeForest(typeForest)
{

}

/**
 * @brief Cell::getColor
 * @return La couleur d'une forêt en fonction de son type (YOUNG, OLD...)
 */
QColor Cell::getColor() const {
    return Cell::getColorOfForest(m_typeForest);
}

/**
 * @brief Cell::getConditions
 * @return Les conditions de changement de type d'une forêt en fonction de son type (YOUNG, OLD...)
 */
QVector<Condition> Cell::getConditions() const {
    foreach(Etat e, Cell::etats) {
        if (m_typeForest == e.forest) {
            return e.conditions;
        }
    }
    //Permet de renvoyer une condition sans changer l'état de la forêt car 9 voisins pas possible
    return {{Forest::ANY,9,qMakePair(1,1),Forest::ANY}};
}

/**
 * @brief Cell::getTypeForest
 * @return le type de la forêt (YOUNG, OLD...)
 */
Forest Cell::getTypeForest() const {
    return m_typeForest;
}

/**
 * @brief Cell::setTypeForest
 * @param forest
 *
 * Met à jour le type de la forêt (YOUNG, OLD...).
 */
void Cell::setTypeForest(Forest forest) {
    m_typeForest = forest;
}

//************************************************************************************************
//**********************************Méthodes statiques********************************************
//************************************************************************************************

/**
 * @brief getColorOfForest
 * @param forest
 * @return QColor la couleur de la forêt en fonction de son type.
 */
QColor Cell::getColorOfForest(Forest forest) {
    return Cell::colors[forest];
}

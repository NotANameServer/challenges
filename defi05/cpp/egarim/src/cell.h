#ifndef CELL_H
#define CELL_H

#include <QObject>
#include <QColor>

/**
 * @brief The Forest enum
 *
 * L'état d'une forêt :
 *
 * YOUNG      : Forêt jeune 🌱 (vert clair)
 * OLD        : Forêt ancienne 🌳 (vert foncé)
 * BEGIN_FIRE : Début de combustion 🎇 (jaune)
 * ON_FIRE    : En combustion 🔥 (rouge)
 * END_FIRE   : Fin de combustion 🧯 (orange)
 * ASH        : Cendres 💀 (noir)
 * ANY        : Aucune, pour mettre un état à une Condition
 */
enum Forest{
    YOUNG, OLD, BEGIN_FIRE, ON_FIRE, END_FIRE, ASH, ANY
};

/**
 * @brief The Condition struct
 *
 * Condition sur le voisinage pour qu'une forêt change d'état.
 *
 * On ramène la probability à 1 c'est à dire <1,n> avec n ∈ ℕ.
 *
 * Ex  : Une voisine au moins en début de combustion avec une probabilité de 1% (probability<1,100>)
 * Ex2 : Cinq voisines au moins en forêt ancienne avec une probabilité de 0,005% (5/100 000 donc probability<1,20 000>)
 * Ex3 : Aucune (forest=Forest::ANY et nbNeighbor=0) condition avec une probabilité de 10% (10/100 donc probability<1,10>)
 */
struct Condition {
    Forest forest;
    int nbNeighbor;
    QPair<int,int> probability;
    Forest next_forest;
};

/**
 * @brief The Etat struct
 *
 * Etat d'une forêt (par défaut elle est en cendre) et ses conditions pour changer d'état.
 *
 * La liste des conditions est à prendre en compte par priorité.
 * C'est à dire que la première condition remplie, elle fait changer d'état la forêt et les suivantes sont ignorées.
 *
 * La priorité est portée par l'indice du vecteur conditions :
 *  -> conditions[n-1] > conditions[n] > conditions[n+1] avec n ∈ ℕ
 *                  (x>y x est plus prioritaire que y)
 */
struct Etat {
    Forest forest = ASH;
    //QColor color = QColorConstants::Svg::black;
    QVector<Condition> conditions;
};

/**
 * @brief The Cell class
 *
 * Représente la position d'une forêt sur une surface 2D et son état.
 */
class Cell : public QObject
{
    Q_OBJECT

public:

    //https://www.w3.org/TR/SVG11/types.html#ColorKeywords
    static const QVector<QColor> colors;
    static const QVector<QString> icons;

    //représente les différents états possible d'une forêt
    static const struct Etat etatYoung;
    static const struct Etat etatOld;
    static const struct Etat etatBegingFire;
    static const struct Etat etatOnFire;
    static const struct Etat etatEndFire;
    static const struct Etat etatAsh;

    static const QVector<Etat> etats;

    static QColor getColorOfForest(Forest forest);

    explicit Cell(QPair<int,int> coord, Forest typeForest = Forest::ASH, QObject *parent = nullptr);

    QColor getColor() const;

    QPair<int,int> m_coord;
    Forest getTypeForest() const;
    void setTypeForest(Forest forest);
    QVector<Condition> getConditions() const;

private:
    Forest m_typeForest;

signals:

};

#endif // CELL_H

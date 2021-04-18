#ifndef RULESWINDOW_H
#define RULESWINDOW_H

#include <QWidget>
#include <QListWidget>
#include <QLineEdit>
#include <QPushButton>
#include <QComboBox>
#include <QSpinBox>
#include <QDoubleSpinBox>
#include "gamerules.h"

class RulesWindow : public QWidget
{
    Q_OBJECT
public:
    RulesWindow(QWidget *parent = nullptr);

    void SetGamerules(Gamerules* rules);

private:
    void UpdateTilesList();
    void UpdateCurrentTile();
    void UpdateRulesList();
    void UpdateCurrentRule();

    void AddTile();
    void RemoveCurrentTile();
    void OnTileEdit();
    void AddRule();
    void RemoveCurrentRule();
    void OnRuleEdit();

    Gamerules* m_gamerules;

    QListWidget* m_tiles;
    QLineEdit* m_tileName;
    QPushButton* m_tileColor;

    QListWidget* m_rules;
    QComboBox* m_oldTile;
    QComboBox* m_newTile;
    QComboBox* m_neighbourTile;
    QDoubleSpinBox* m_probaRule;
    QSpinBox* m_neighbourMin;
    QSpinBox* m_neighbourMax;


};

#endif // RULESWINDOW_H

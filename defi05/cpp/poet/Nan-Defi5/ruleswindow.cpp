#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QLabel>

#include "ruleswindow.h"

RulesWindow::RulesWindow(QWidget *parent) : QWidget(parent), m_gamerules(nullptr)
{
    m_tiles = new QListWidget();
    m_rules = new QListWidget();

    m_tileName = new QLineEdit();
    m_tileColor = new QPushButton();
    m_tileColor->setFixedWidth(30);

    QPushButton* addTileButton = new QPushButton("Add");
    QPushButton* delTileButton = new QPushButton("Del");

    QHBoxLayout* tileButtonsLayout = new QHBoxLayout();
    tileButtonsLayout->addWidget(addTileButton);
    tileButtonsLayout->addWidget(delTileButton);

    QHBoxLayout* tileLayout =new QHBoxLayout();
    tileLayout->addWidget(new QLabel("Name:"));
    tileLayout->addWidget(m_tileName);
    tileLayout->addSpacing(5);
    tileLayout->addWidget(m_tileColor);

    QPushButton* addRuleButton = new QPushButton("Add");
    QPushButton* delRuleButton = new QPushButton("Del");

    m_oldTile = new QComboBox();
    m_newTile = new QComboBox();
    m_neighbourTile = new QComboBox();
    m_probaRule = new QDoubleSpinBox();
    m_neighbourMin = new QSpinBox();
    m_neighbourMax = new QSpinBox();

    QHBoxLayout* ruleButtonsLayout = new QHBoxLayout();
    ruleButtonsLayout->addWidget(addRuleButton);
    ruleButtonsLayout->addWidget(delRuleButton);
    QHBoxLayout* ruleTileLayout = new QHBoxLayout();
    ruleTileLayout->addWidget(m_oldTile, 1);
    ruleTileLayout->addWidget(new QLabel("->"));
    ruleTileLayout->addWidget(m_newTile, 1);
    QHBoxLayout* ruleProbaLayout = new QHBoxLayout();
    ruleProbaLayout->addWidget(new QLabel("Proba:"));
    ruleProbaLayout->addWidget(m_probaRule, 1);
    QHBoxLayout* neighbourLayout = new QHBoxLayout();
    neighbourLayout->addWidget(new QLabel("Neighbour"));
    neighbourLayout->addWidget(m_neighbourTile, 1);
    neighbourLayout->addWidget(m_neighbourMin);
    neighbourLayout->addWidget(m_neighbourMax);

    QVBoxLayout* mainLayout = new QVBoxLayout();
    mainLayout->addWidget(new QLabel("Tiles"));
    mainLayout->addLayout(tileButtonsLayout);
    mainLayout->addWidget(m_tiles);
    mainLayout->addWidget(new QLabel("Current tile"));
    mainLayout->addLayout(tileLayout);

    mainLayout->addSpacing(5);
    mainLayout->addWidget(new QLabel("Rules"));
    mainLayout->addLayout(ruleButtonsLayout);
    mainLayout->addWidget(m_rules);
    mainLayout->addWidget(new QLabel("Current rule"));
    mainLayout->addLayout(ruleTileLayout);
    mainLayout->addLayout(ruleProbaLayout);
    mainLayout->addLayout(neighbourLayout);

    setLayout(mainLayout);
    setMinimumWidth(200);
}

void RulesWindow::SetGamerules(Gamerules* rules)
{
    m_gamerules = rules;
    UpdateTilesList();
    UpdateRulesList();
}

void RulesWindow::UpdateTilesList()
{
    if(m_gamerules == nullptr)
        return;

    auto tiles = m_gamerules->GetAllTilesID();

    int currentIndex = m_tiles->currentRow();
    m_tiles->clear();

    for(auto t : tiles)
    {
        Tile* tile = m_gamerules->GetTile(t);
        QPixmap icon(10, 10);
        icon.fill(tile->color);
        QListWidgetItem* pItem = new QListWidgetItem(icon, tile->name);
        m_tiles->addItem(pItem);
    }

    if(m_tiles->count() > currentIndex)
        m_tiles->setCurrentRow(currentIndex);
    else m_tiles->setCurrentRow(-1);
}

void RulesWindow::UpdateCurrentTile()
{
    if(m_gamerules == nullptr)
        return;

}

void RulesWindow::UpdateRulesList()
{
    if(m_gamerules == nullptr)
        return;

    int currentIndex = m_rules->currentRow();
    m_rules->clear();

    int nbRules = m_gamerules->GetRuleNb();
    for(int i = 0 ; i < nbRules; i++)
    {
        Rule* rule = m_gamerules->GetRule(i);
        Tile* oldTile = m_gamerules->GetTile(rule->oldValue);
        Tile* newTile = m_gamerules->GetTile(rule->newValue);
        if(oldTile == nullptr || newTile == nullptr)
            return;

        m_rules->addItem(oldTile->name + " -> " + newTile->name);
    }

    if(m_rules->count() > currentIndex)
        m_rules->setCurrentRow(currentIndex);
    else m_rules->setCurrentRow(-1);
}

void RulesWindow::UpdateCurrentRule()
{
    if(m_gamerules == nullptr)
        return;

}

void RulesWindow::AddTile()
{
    m_gamerules->AddTile("New Tile", {255, 255, 255});
    UpdateTilesList();
}

void RulesWindow::RemoveCurrentTile()
{
    int index = m_tiles->currentRow();
    if(index >= 0)
    {
        Tile* t = m_gamerules->GetTileFromIndex(index);
        m_gamerules->RemoveTile(t->id);
    }

    UpdateTilesList();
}

void RulesWindow::OnTileEdit()
{

}

void RulesWindow::AddRule()
{

}

void RulesWindow::RemoveCurrentRule()
{

}

void RulesWindow::OnRuleEdit()
{

}

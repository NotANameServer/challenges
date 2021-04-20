#include "mainwindow.h"
#include "lvlonewindow.h"
#include "lvltwowindow.h"
#include <QGraphicsScene>
#include <QGraphicsView>
#include <QLabel>
#include <QPushButton>
#include <QVBoxLayout>
#include <QHBoxLayout>

const QString MainWindow::LBL_BTN_LVL_1 = "niveau 1";
const QString MainWindow::LBL_BTN_LVL_2 = "niveau 2";
const QString MainWindow::LBL_BTN_LVL_3 = "niveau 3";

/**
 * @brief MainWindow::MainWindow
 * @param parent
 *
 * La fenêtre principal de l'application.
 * Elle contient un lien vers le github de NaN, l'énoncé ralisé par @Triphase et 3 boutons pour lancer les défis
 */
// todo: défi numéro 3.
MainWindow::MainWindow(QWidget *parent) : QMainWindow(parent)
{
    QWidget *zoneCentrale = new QWidget;

    QLabel *lblTitle = new QLabel;
    lblTitle->setText("<a href=\"https://github.com/NotANameServer/challenges/tree/master/defi05\">Défi 5 - Automates cellulaires</a>");
    lblTitle->setOpenExternalLinks(true);
    lblTitle->setAlignment(Qt::AlignCenter);

    //Add README.jpg
    QGraphicsScene *scene = new QGraphicsScene;
    QPixmap pixmap(":/res/images/README_JPG");
    scene->addPixmap(pixmap);
    QGraphicsView *graphicsView = new QGraphicsView;
    graphicsView->setScene(scene);
    graphicsView->show();

    QPushButton *btnLvl1 = new QPushButton(LBL_BTN_LVL_1);
    connect(btnLvl1,&QPushButton::clicked,this,&MainWindow::showLvlOne);

    QPushButton *btnLvl2 = new QPushButton(LBL_BTN_LVL_2);
    connect(btnLvl2,&QPushButton::clicked,this,&MainWindow::showLvlTwo);

    QPushButton *btnLvl3 = new QPushButton(LBL_BTN_LVL_3);

    QHBoxLayout *hboxLayout = new QHBoxLayout;
    hboxLayout->addWidget(btnLvl1);
    hboxLayout->addWidget(btnLvl2);
    hboxLayout->addWidget(btnLvl3);

    QVBoxLayout *vboxLayout = new QVBoxLayout;
    vboxLayout->addWidget(lblTitle);
    vboxLayout->addWidget(graphicsView);
    vboxLayout->addLayout(hboxLayout);

    resize(1280,1024);
    zoneCentrale->setLayout(vboxLayout);
    setCentralWidget(zoneCentrale);

}

/**
 * @brief MainWindow::showLvlOne
 *
 * Lance la fenpetre du défi numéro 1.
 */
void MainWindow::showLvlOne() {
    LvlOneWindow *lvlOneWindow = new LvlOneWindow;
    lvlOneWindow->setWindowModality(Qt::ApplicationModal);
    lvlOneWindow->show();
}

/**
 * @brief MainWindow::showLvlTwo
 *
 * Lance la fenpetre du défi numéro 2.
 */
void MainWindow::showLvlTwo() {
    LvlTwoWindow *lvlTwoWindow = new LvlTwoWindow;
    lvlTwoWindow->setWindowModality(Qt::ApplicationModal);
    lvlTwoWindow->show();
}



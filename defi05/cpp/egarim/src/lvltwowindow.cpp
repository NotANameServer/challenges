#include "cell.h"
#include "lvltwowindow.h"
#include <QPainter>
#include <QVBoxLayout>
#include <QPushButton>
#include <QGraphicsScene>
#include <QGraphicsView>
#include <ctime>
#include <QMutex>

//*********************************************************************************************
//****Implémentation de la class LvlTwoWindow qui est une fenêtre pour afficher le résultat****
//*********************************************************************************************

const QString LvlTwoWindow::LBL_BTN_START = "démarrer";
const QString LvlTwoWindow::LBL_BTN_STOP = "pause";

/**
 * @brief LvlTwoWindow::FORESTS_WIDTH
 *
 * Nombre de forêt sur l'axe x.
 */
const int LvlTwoWindow::FORESTS_WIDTH = 50;
/**
 * @brief LvlTwoWindow::FORESTS_HEIGHT
 *
 * Nombre de forêt sur l'axe y.
 */
const int LvlTwoWindow::FORESTS_HEIGHT = 50;

/**
 * @brief LvlTwoWindow::LvlTwoWindow
 * @param parent
 *
 * Initialisation de la fenêtre avec des QGraphicsEllipseItem et deux boutons.
 */
LvlTwoWindow::LvlTwoWindow(QWidget *parent) : QMainWindow(parent), m_screenWidth(600), m_screenHeight(600)
{
    QWidget *zoneCentrale = new QWidget(this);

    QVBoxLayout *vboxLayout = new QVBoxLayout;

    QGraphicsView *view = new QGraphicsView;
    QGraphicsScene *planete = new QGraphicsScene;
    planete->setBackgroundBrush(Qt::gray);
    view->setScene(planete);

    //Affiche les forêts et initialise les tableaux pour l'affichage et les calculs.
    for (int y=0;y<(FORESTS_HEIGHT*10);y+=10){
        for (int x=0;x<(FORESTS_WIDTH*10);x+=10){
            QGraphicsEllipseItem *item= new QGraphicsEllipseItem(x,y,8,8);
            //init à cendre par défaut
            //sinon pour initialialiser une forêt jeune :
            //Cell *cell = new Cell(qMakePair(x,y),Forest::YOUNG);
            Cell *cell = new Cell(qMakePair(x,y));
            m_forests.push_back(cell);
            item->setBrush(cell->getColor());
            m_ihm_forests.push_back(item);
            planete->addItem(item);
        }
    }

    QHBoxLayout *hboxlayout = new QHBoxLayout;

    m_btnStart = new QPushButton(LBL_BTN_START);
    connect(m_btnStart,&QPushButton::clicked,this,&LvlTwoWindow::onClickBtnStart);

    m_btnStop = new QPushButton(LBL_BTN_STOP);
    m_btnStop->setEnabled(false);
    connect(m_btnStop,&QPushButton::clicked,this,&LvlTwoWindow::onClickBtnStop);

    hboxlayout->addWidget(m_btnStart);
    hboxlayout->addWidget(m_btnStop);

    vboxLayout->addWidget(view);
    vboxLayout->addLayout(hboxlayout);

    zoneCentrale->setLayout(vboxLayout);

    setCentralWidget(zoneCentrale);
    resize(m_screenHeight,m_screenHeight);
}

/**
 * @brief LvlTwoWindow::~LvlTwoWindow
 *
 * Destructeur pour libérer les forêts.
 */
LvlTwoWindow::~LvlTwoWindow() {
    foreach (Cell *item, m_forests) {
        delete item;
        item = 0;
    }
    m_forests.clear();
}

/**
 * @brief LvlTwoWindow::onFillGridLayout
 * @param j
 *
 * Méthode appelée par le Thread Alive pour mettre à jour la fenêtre en fonction des calculs obtenus.
 * QGraphicsEllipseItem est un composant graphique qui contient une couleur en fonction de l'état de la forêt.
 */
void LvlTwoWindow::onFillGridLayout(int j)
{
    QPainter painter(this);

    int i=0;
    foreach (QGraphicsEllipseItem *item, m_ihm_forests) {
        item->setBrush(m_forests[i]->getColor());
        ++i;
    }
}

/**
 * @brief LvlTwoWindow::start
 *
 * Lance le thread de calcul.
 */
void LvlTwoWindow::start() {
    m_thread = new Alive2(this, m_forests,false);
    connect(m_thread, SIGNAL(fillGridLayout(int)), this, SLOT(onFillGridLayout(int)));
    m_thread->start();
    m_btnStart->setEnabled(false);
    m_btnStop->setEnabled(true);
}

/**
 * @brief LvlTwoWindow::stop
 *
 * Arrête le thread de calcul.
 */
void LvlTwoWindow::stop() {
    if(m_thread != 0) {
        m_thread->stop();
        m_btnStart->setEnabled(true);
        m_btnStop->setEnabled(false);
    }
}

/**
 * @brief LvlTwoWindow::onClickBtnStart
 *
 * Quand on appui sur le bouton démarrer.
 */
void LvlTwoWindow::onClickBtnStart() {
    start();
}

/**
 * @brief LvlTwoWindow::onClickBtnStop
 *
 * Quand on appui sur le bouton pause.
 */
void LvlTwoWindow::onClickBtnStop() {
    stop();
}

/**
 * @brief LvlTwoWindow::closeEvent
 * @param event
 *
 * Quand l'utilisateur ferme la fenêtre, on arrête le thread de calcul.
 */
void LvlTwoWindow::closeEvent(QCloseEvent *event) {
    stop();
    event->accept();
}

//*********************************************************************************************
//*************Implémentation de la class Alive qui est un Thread pour les calculs*************
//*********************************************************************************************

/**
 * @brief Alive2::Alive2
 * @param parent
 * @param forests
 * @param stop
 *
 * Constructeur du thread.C'est son point d'entrée à sa création.
 */
Alive2::Alive2(QObject *parent, QVector<Cell*> forests, bool stop) : QThread(parent), m_forests(forests), m_stop(stop) {
    generator.seed(std::time(nullptr));
}

/**
 * @brief Alive2::run
 *
 * Une boucle infinie qui s'arrête si on clique sur le bouton pause de la fenêtre.
 */
void Alive2::run() {

    while (true) {
        QMutex mutex;
        //empêche les autres threads notament l'UI thread d'accèder au valeur m_stop et m_forests
        mutex.lock();
        if(m_stop) {
            break;
        }
        //calcul le prochain état
        next();
        mutex.unlock();

        //averti l'ihm de se mettre à jour avec la nouvelle valeur du vecteur seed
        emit fillGridLayout(0);

        //met le thread en pause une mili seconde
        QThread::msleep(1);
    }
}

/**
 * @brief Alive2::next
 *
 * Change l'état les valeurs du tableau m_forests en fonction de certaine(s) condition(s) et d'une probabilité.
 */
void Alive2::next() {
    QMap<int,Forest> forest;

    for (int y=0; y<LvlTwoWindow::FORESTS_HEIGHT; ++y) {
        for (int x=0; x<LvlTwoWindow::FORESTS_WIDTH; ++x) {
            //On récupère les voisins qui ne sont pas en dehors du plateau 2D.
            QVector<int> neighbors;

            //on transforme la coordonnée 2D en coordonée 1D.
            int idx = (y+1) * (x+1) - 1;

            if ((idx-1)>-1) { neighbors.push_back(idx-1); }
            if ((idx-LvlTwoWindow::FORESTS_WIDTH+1)>-1) { neighbors.push_back(idx-LvlTwoWindow::FORESTS_WIDTH+1); }
            if ((idx-LvlTwoWindow::FORESTS_WIDTH)>-1) { neighbors.push_back(idx-LvlTwoWindow::FORESTS_WIDTH); }
            if ((idx-LvlTwoWindow::FORESTS_WIDTH-1)>-1) { neighbors.push_back(idx-LvlTwoWindow::FORESTS_WIDTH-1); }

            if ((idx+1)<(LvlTwoWindow::FORESTS_WIDTH*LvlTwoWindow::FORESTS_HEIGHT)) { neighbors.push_back(idx+1); }
            if ((idx+LvlTwoWindow::FORESTS_WIDTH-1)<(LvlTwoWindow::FORESTS_WIDTH*LvlTwoWindow::FORESTS_HEIGHT)) { neighbors.push_back(idx+LvlTwoWindow::FORESTS_WIDTH-1); }
            if ((idx+LvlTwoWindow::FORESTS_WIDTH)<(LvlTwoWindow::FORESTS_WIDTH*LvlTwoWindow::FORESTS_HEIGHT)) { neighbors.push_back(idx+LvlTwoWindow::FORESTS_WIDTH); }
            if ((idx+LvlTwoWindow::FORESTS_WIDTH+1)<(LvlTwoWindow::FORESTS_WIDTH*LvlTwoWindow::FORESTS_HEIGHT)) { neighbors.push_back(idx+LvlTwoWindow::FORESTS_WIDTH+1); }

            //on parcours la liste des conditions par ordre de priorité
            //si une condition est remplie on lance le dé du destin et on stop la boucle quelques soit le résultat.
            foreach(Condition c, m_forests[idx]->getConditions()) {
                int nbNeighbor = c.nbNeighbor;
                //on décrémente si un voisin est éligible
                foreach (int idxNeighbor, neighbors) {
                    if (m_forests[idxNeighbor]->getTypeForest() == c.forest) {
                        nbNeighbor--;
                    }
                }
                //si condition remplie on lance le dé du destin pour savoir si on change d'état
                if (nbNeighbor<1) {

                    //aléa : Générateurs déterministes
                    std::uniform_int_distribution<int> distribution(c.probability.first,c.probability.second);
                    int dice_roll = distribution(generator);

                    //change état de la forêt si proba ok
                    //aléa : Générateurs déterministes
                    if (dice_roll==1) {
                    //aléa : Générateurs non déterministes
                    //if ((rd()%c.probability.second)==1) {
                        //QMap [] surchargé est crée l'objet s'il n'éxiste pas
                        forest[idx]=c.next_forest;
                        //break;
                    }

                    break;
                }
            }
        }
    }

    //mets à jour uniqment les forêts qui ont changées d'états.
    QMapIterator<int, Forest> i(forest);
    while (i.hasNext()) {
        i.next();
        m_forests[i.key()]->setTypeForest(i.value());
    }
}

/**
 * @brief Alive2::stop
 *
 * Met à jour la variable de memebre stop, pour signaler que le thread doit s'arrêter.
 */
void Alive2::stop() {
    m_stop = true;
}

#include "lvlonewindow.h"
#include <QWidget>
#include <QLabel>
#include <QThread>
#include <QMutex>

//*********************************************************************************************
//****Implémentation de la class LvlOneWindow qui est une fenêtre pour afficher le résultat****
//*********************************************************************************************

const QString LvlOneWindow::LBL_BTN_START = "démarrer";
const QString LvlOneWindow::LBL_BTN_STOP = "pause";

/**
 * @brief LvlOneWindow::LvlOneWindow
 * @param parent
 *
 * Initialisation de la fenêtre avec des labels et deux boutons.
 */
LvlOneWindow::LvlOneWindow(QWidget *parent) : QMainWindow(parent)
{
    zoneCentrale = new QWidget(this);

    QVBoxLayout *vboxlayout = new QVBoxLayout;

    m_gridLayout = new QGridLayout;

    for (int i=0; i<m_seed.size(); ++i) {
        QLabel *lbl = new QLabel(QString::number(m_seed[i]));
        m_gridLayout->addWidget(lbl,0,i,1,1,Qt::AlignHCenter);
    }

    QHBoxLayout *hboxlayout = new QHBoxLayout;

    m_btnStart = new QPushButton(LBL_BTN_START);
    connect(m_btnStart,&QPushButton::clicked,this,&LvlOneWindow::onClickBtnStart);

    m_btnStop = new QPushButton(LBL_BTN_STOP);
    m_btnStop->setEnabled(false);
    connect(m_btnStop,&QPushButton::clicked,this,&LvlOneWindow::onClickBtnStop);

    hboxlayout->addWidget(m_btnStart);
    hboxlayout->addWidget(m_btnStop);

    vboxlayout->addLayout(m_gridLayout);
    vboxlayout->addLayout(hboxlayout);

    resize(1024,768);
    zoneCentrale->setLayout(vboxlayout);
    setCentralWidget(zoneCentrale);
}

/**
 * @brief LvlOneWindow::onFillGridLayout
 * @param j
 *
 * Méthode appelée par le Thread Alive pour mettre à jour la fenêtre en fonction des calculs obtenus.
 * m_gridLayout composant graphique qui contient des labels pour afficher le résultat.
 *
 */
void LvlOneWindow::onFillGridLayout(int j)
{
    for (int i=j; i<m_seed.size(); ++i) {
        (dynamic_cast<QLabel*>(m_gridLayout->itemAtPosition(0,i)->widget()))->setText(QString::number(m_seed[i]));
    }
}

/**
 * @brief LvlOneWindow::start
 *
 * Lance le thread de calcul.
 */
void LvlOneWindow::start() {
    m_thread = new Alive(this, &m_seed,false);
    connect(m_thread, SIGNAL(fillGridLayout(int)), this, SLOT(onFillGridLayout(int)));
    m_thread->start();
    m_btnStart->setEnabled(false);
    m_btnStop->setEnabled(true);
}

/**
 * @brief LvlOneWindow::stop
 *
 * Arrête le thread de calcul.
 */
void LvlOneWindow::stop() {
    if(m_thread != 0) {
        m_thread->stop();
        m_btnStart->setEnabled(true);
        m_btnStop->setEnabled(false);
    }
}

/**
 * @brief LvlOneWindow::onClickBtnStart
 *
 * Quand on appuie sur le bouton démarrer.
 */
void LvlOneWindow::onClickBtnStart() {
    start();
}

/**
 * @brief LvlOneWindow::onClickBtnStop
 *
 * Quand on appuie sur le bouton pause.
 */
void LvlOneWindow::onClickBtnStop() {
    stop();
}

/**
 * @brief LvlOneWindow::closeEvent
 * @param event
 *
 * Quand l'utilisateur ferme la fenêtre, on arrête le thread de calcul.
 */
void LvlOneWindow::closeEvent(QCloseEvent *event) {
    stop();
    event->accept();
}

//*********************************************************************************************
//*************Implémentation de la class Alive qui est un Thread pour les calculs*************
//*********************************************************************************************

/**
 * @brief Alive::Alive
 * @param parent
 * @param seed
 * @param stop
 *
 * Constructeur du thread.C'est son point d'entrée à sa création.
 */
Alive::Alive(QObject *parent, QVector<int> *seed, bool stop) : QThread(parent), m_seed(seed), m_stop(stop) {

}

/**
 * @brief Alive::run
 *
 * Quand on lance le thread à partir du bouton démarrer de la fenêtre.
 *
 * Une boucle infinie qui s'arrête si on clique sur le bouton pause de la fenêtre.
 */
void Alive::run() {

    while (true) {
        QMutex mutex;
        //empêche les autres threads notamment l'UI thread d'accèder au valeur m_stop et m_seed.
        mutex.lock();
        if(m_stop) {
            break;
        }
        //calcul le prochain état
        next();
        mutex.unlock();

        //averti l'ihm de se mettre à jour avec la nouvelle valeur du vecteur seed
        emit fillGridLayout(0);

        //met le thread en pause une seconde
        QThread::sleep(1);
    }
}

/**
 * @brief Alive::next
 *
 * Change l'état des valeurs du tableau m_seed en fonction d'un tableau d'état et du résultat de la somme de :
 * m_seed[n-1] + m_seed[n] + m_seed[n+1] avec n ∈ ℕ.
 */
void Alive::next() {
    //Effectue une copie du tableau m_seed pour ne pas changer les états tant que le calcul de tous les éléments n'a pas été effectués.
    QVector<int> seed(*m_seed);
    //cas particulier du premier élment du tableau ou n-1 n'existe pas donc on l'ignore.
    seed[0]=m_etat[(*m_seed)[0]+(*m_seed)[1]];

    //Effectue la somme de tous les éléments du tableau de copie seed et le met à jour.
    for (int i=1; i<(*m_seed).size()-1; ++i) {
        seed[i] = m_etat[(*m_seed)[i-1]+(*m_seed)[i]+(*m_seed)[i+1]];
    }

    //cas particulier du dernier élément du tableau ou n+1 n'existe pas donc on l'ignore.
    seed[seed.size()-1] = m_etat[(*m_seed)[(*m_seed).size()-1]+(*m_seed)[(*m_seed).size()-2]];

    //met à jour le tableau original m_seed une fois les changements effectués.
    for (int i=0; i<seed.size(); ++i) {
        (*m_seed)[i]=seed[i];
    }
}

/**
 * @brief Alive::stop
 *
 * Met à jour la variable de membre stop, pour signaler que le thread doit s'arrêter.
 */
void Alive::stop() {
    m_stop = true;
}

#ifndef LVLONEWINDOW_H
#define LVLONEWINDOW_H

#include <QMainWindow>
#include <QMap>
#include <QGridLayout>
#include <QThread>
#include <QLabel>
#include <QPushButton>
#include <QCloseEvent>

/**
 * @brief The Alive class
 *
 * Thread du défi numéro 1. Il permet de faire les calculs.
 */
class Alive : public QThread
{
    Q_OBJECT
public:
    explicit Alive(QObject *parent, QVector<int> *seed, bool stop = false);
    void run();
    void stop();

private:
    QVector<int> *m_seed;
    bool m_stop;
    //{Valeur de la somme, Nouvel état}
    QMap<int,int> m_etat {{0,0}, {1,3}, {2,2}, {3,0}, {4,0}, {5,1}, {6,3}, {7,2}, {8,3}, {9,1}};

    void next();

signals:
    void fillGridLayout(int);

};

/**
 * @brief The LvlOneWindow class
 *
 * La fenêtre du défi numéro 1.
 */
class LvlOneWindow : public QMainWindow
{
    Q_OBJECT
public:
    Alive *m_thread;

    explicit LvlOneWindow(QWidget *parent = nullptr);

    void next();

private:
    static const QString LBL_BTN_START;
    static const QString LBL_BTN_STOP;

    QWidget *zoneCentrale;

    QGridLayout *m_gridLayout;
    QPushButton *m_btnStart;
    QPushButton *m_btnStop;

    //état initial
    QVector<int> m_seed {0, 0, 0, 1, 0, 0, 0};

    void start();
    void stop();

protected:
    void closeEvent(QCloseEvent *event) override;

public slots:
    void onFillGridLayout(int);
    void onClickBtnStart();
    void onClickBtnStop();

signals:

};

#endif // LVLONEWINDOW_H

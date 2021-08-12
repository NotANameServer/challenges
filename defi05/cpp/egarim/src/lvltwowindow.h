#ifndef LVLTWOWINDOW_H
#define LVLTWOWINDOW_H

#include "cell.h"
#include <QMainWindow>
#include <QThread>
#include <QGraphicsEllipseItem>
#include <QPushButton>
#include <QCloseEvent>
#include <random>

/**
 * @brief The Alive2 class
 *
 * Thread du défi numéro 2. Il permet de faire les calculs.
 */
class Alive2 : public QThread
{
    Q_OBJECT
public:
    explicit Alive2(QObject *parent, QVector<Cell*> forests, bool stop = false);
    void run();
    void stop();

private:
    QVector<Cell*> m_forests;
    bool m_stop;
    //aléa : Générateurs déterministes
    std::default_random_engine generator;
    //aléa : Générateurs non déterministes
    //std::random_device rd{};

    void next();

signals:
    void fillGridLayout(int);

};

/**
 * @brief The LvlTwoWindow class
 *
 * La fenêtre du défi numéro 2.
 */
class LvlTwoWindow : public QMainWindow
{
    Q_OBJECT
public:    
    static const int FORESTS_WIDTH;
    static const int FORESTS_HEIGHT;

    explicit LvlTwoWindow(QWidget *parent = nullptr);
    virtual ~LvlTwoWindow();

    Alive2 *m_thread;

private:
    static const QString LBL_BTN_START;
    static const QString LBL_BTN_STOP;

    QPushButton *m_btnStart;
    QPushButton *m_btnStop;

    int m_screenWidth;
    int m_screenHeight;

    QVector<QGraphicsEllipseItem*> m_ihm_forests;
    QVector<Cell*> m_forests;

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

#endif // LVLTWOWINDOW_H

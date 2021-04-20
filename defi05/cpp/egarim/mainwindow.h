#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>

/**
 * @brief The MainWindow class
 *
 * C'est la fenêtre principale.
 */
class MainWindow : public QMainWindow
{
    Q_OBJECT
public:
    explicit MainWindow(QWidget *parent = nullptr);

public slots:
    void showLvlOne();
    void showLvlTwo();

private:
    static const QString LBL_BTN_LVL_1;
    static const QString LBL_BTN_LVL_2;
    static const QString LBL_BTN_LVL_3;

signals:

};

#endif // MAINWINDOW_H

#include "mainwindow.h"

#include <QApplication>

/**
 * @brief main
 * @param argc
 * @param argv
 * @return
 *
 * Le point d'entrée de l'application.
 */
int main(int argc, char *argv[])
{
    QApplication app(argc, argv);

    MainWindow w;
    //Affiche la fenêtre principale.
    w.show();

    return app.exec();
}

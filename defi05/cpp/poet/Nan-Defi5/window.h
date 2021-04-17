#ifndef WINDOW_H
#define WINDOW_H

#include <QWidget>
#include <QGraphicsScene>
#include <QGraphicsView>
#include <QPixmap>
#include <QTimer>

#include "game.h"

class Window : public QWidget
{
    Q_OBJECT

public:
    Window(QWidget *parent = nullptr);
    ~Window();

protected:
    virtual void resizeEvent(QResizeEvent *event) override;

private:
    void OnSceneSizeChange(const QRectF & rect);

    void OnTimer();

    QGraphicsScene* m_scene;
    QGraphicsView* m_view;
    QGraphicsPixmapItem* m_item;
    QTimer* m_timer;

    Game m_game;
};

#endif // WINDOW_H

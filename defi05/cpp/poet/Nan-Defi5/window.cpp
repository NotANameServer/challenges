#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QGraphicsPixmapItem>

#include <iostream>

#include "window.h"

Window::Window(QWidget *parent)
    : QWidget(parent), m_game(1, 1)
{
    m_scene = new QGraphicsScene();
    m_view = new QGraphicsView(m_scene);

    QVBoxLayout* mainLayout = new QVBoxLayout();
    mainLayout->addWidget(m_view);

    setLayout(mainLayout);

    m_item = m_scene->addPixmap(*m_game.GetImage());

    m_timer = new QTimer(this);
    m_timer->setSingleShot(false);
    connect(m_timer, &QTimer::timeout, this, &Window::OnTimer);
    m_timer->setInterval(10);
    m_timer->start();
}

Window::~Window()
{

}

void Window::resizeEvent(QResizeEvent *event)
{
    QWidget::resizeEvent(event);

    auto rect = m_view->rect();
    //idk how to get the scene view, just remove the border
    rect.setWidth(rect.width() - 2);
    rect.setHeight(rect.height() - 2);
    m_view->setSceneRect(rect);

    m_game.SetSize(rect.width(), rect.height());
    m_item->setPixmap(*m_game.GetImage());
}


void Window::OnTimer()
{
    m_game.Update();
    m_item->setPixmap(*m_game.GetImage());
}


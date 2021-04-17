#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QGraphicsPixmapItem>

#include <iostream>

#include "window.h"

Window::Window(QWidget *parent)
    : QWidget(parent), m_game(1, 1)
{
    m_render = new RenderWidget();
    m_render->SetData(m_game.GetData());
    m_render->SetGamerules(m_game.GetGamerules());

    QVBoxLayout* mainLayout = new QVBoxLayout();
    mainLayout->addWidget(m_render);

    setLayout(mainLayout);

    m_timer = new QTimer(this);
    m_timer->setSingleShot(false);
    connect(m_timer, &QTimer::timeout, this, &Window::OnTimer);
    m_timer->setInterval(10);
    m_timer->start();

    setMinimumSize(200, 200);
}

Window::~Window()
{

}

void Window::resizeEvent(QResizeEvent *event)
{
    QWidget::resizeEvent(event);

    auto rect = m_render->rect();

    m_game.SetSize(rect.width(), rect.height());
}


void Window::OnTimer()
{
    m_game.Update();
    repaint();
}


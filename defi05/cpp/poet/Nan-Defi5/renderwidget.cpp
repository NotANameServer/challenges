#include <QPainter>

#include "renderwidget.h"

RenderWidget::RenderWidget(QWidget *parent)
    : QWidget(parent), m_image(1, 1, QImage::Format_RGB32)
{
    setMinimumSize(200, 200);
}

void RenderWidget::paintEvent(QPaintEvent*)
{
    if(m_data == nullptr || m_gamerules == nullptr)
        return;

    int width = std::max(1, m_data->width());
    int height = std::max(1, m_data->height());

    if(m_image.width() != width || m_image.height() != height)
    {
        m_image = m_image.scaled(width, height);
    }

    if(m_data->width() == 0 || m_data->height() == 0)
        return;

    for(int i = 0 ; i < height; i++)
    {
        unsigned char* line = m_image.scanLine(i);
        for(int j = 0 ; j <width; j++)
        {
            QColor color = m_gamerules->GetColor((*m_data)(j, i));

            int index = j * 4;
            line[index + 2] = color.red();
            line[index + 1] = color.green();
            line[index + 0] = color.blue();
        }
    }

    QPainter painter(this);
    painter.drawImage(QPoint(0, 0), m_image);
}

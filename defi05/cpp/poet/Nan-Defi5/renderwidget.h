#ifndef RENDERWIDGET_H
#define RENDERWIDGET_H

#include <QWidget>

#include "matrix.h"
#include "gamerules.h"

class RenderWidget : public QWidget
{
    Q_OBJECT

public:
    RenderWidget(QWidget* parent = nullptr);

    void SetData(Matrix<Gamerules::TileType>* data){ m_data = data;}
    void SetGamerules(Gamerules* rules){m_gamerules = rules;}

protected:
    virtual void paintEvent(QPaintEvent* event) override;

private:
    Matrix<Gamerules::TileType>* m_data = nullptr;
    Gamerules* m_gamerules = nullptr;

    QImage m_image;
};

#endif // RENDERWIDGET_H

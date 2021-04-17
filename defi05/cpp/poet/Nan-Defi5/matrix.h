#ifndef MATRIX_H
#define MATRIX_H

#include <vector>

template <typename T>
class Matrix
{
public:
    Matrix(int width, int height, T value = T())
        : m_width(0), m_height(0)
    {
        Resize(width, height, value);
    }

    void Resize(int width, int height, T value = T())
    {
        std::vector<T> newData(width * height, value);
        int minX = std::min(width, m_width);
        int minY = std::min(height, m_height);

        for(int i = 0 ; i < minX; i++)
        {
            for(int j = 0 ; j < minY; j++)
            {
                int oldIndex = posToIndex(i, j);
                int newIndex = i + j * width;

                newData[newIndex] = std::move(m_data[oldIndex]);
            }
        }

        m_data = newData;
        m_width = width;
        m_height = height;
    }

    T& operator()(int x, int y)
    {
        return m_data[posToIndex(x, y)];
    }

    const T& operator()(int x, int y) const
    {
        return m_data[posToIndex(x, y)];
    }

    int width() const {return m_width;}
    int height() const {return m_height;}

private:
    int posToIndex(int x, int y) const
    {
        return x + y * m_width;
    }

    int m_width;
    int m_height;
    std::vector<T> m_data;
};

template <typename T>
class MatrixView
{
public:
    MatrixView(Matrix<T> & mat, int x, int y)
        : m_matrix(mat), m_x(x), m_y(y)
    { }

    bool ValidPos(int x, int y) const
    {
        int posX = x + m_x;
        int posY = y + m_y;

        if(posX < 0 || posX >= m_matrix.width())
            return false;
        if(posY < 0 || posY >= m_matrix.height())
            return false;
        return true;
    }

    T& operator()(int x, int y)
    {
        return m_matrix(x + m_x, y + m_y);
    }

    const T& operator()(int x, int y) const
    {
        return m_matrix(x + m_x, y + m_y);
    }

private:
    Matrix<T> & m_matrix;
    int m_x;
    int m_y;
};

#endif // MATRIX_H

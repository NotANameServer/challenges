#ifndef LABYRESOLVE_H
#define LABYRESOLVE_H

#include <string>
#include <vector>

struct Pos
{
    Pos():x(0), y(0){}
    Pos(int _x, int _y):x(_x), y(_y){}
    Pos(const Pos & p):x(p.x), y(p.y){}
    Pos& operator=(const Pos & p){x = p.x; y = p.y; return *this;}

    Pos& operator+=(const Pos & p){x += p.x; y += p.y; return *this;}
    Pos& operator-=(const Pos & p){x -= p.x; y -= p.y; return *this;}
    Pos& operator*=(int value){x *= value; y *= value; return *this;}
    Pos& operator/=(int value){x /= value; y /= value; return *this;}
    bool operator==(const Pos & p) const{return x == p.x && y == p.y;}
    bool operator!=(const Pos & p) const{return x != p.x || y != p.y;}

    int x;
    int y;
};

inline Pos operator+(const Pos & p1, const Pos & p2){Pos p = p1; return p += p2;}
inline Pos operator-(const Pos & p1, const Pos & p2){Pos p = p1; return p -= p2;}
inline Pos operator*(const Pos & p1, int value){Pos p = p1; return p *= value;}
inline Pos operator/(const Pos & p1, int value){Pos p = p1; return p /= value;}

template <typename T>
class Matrix
{
public:
    Matrix(unsigned int width, unsigned int height, T defaultValue = T())
        : m_width(width)
        , m_height(height)
        , m_data(width * height, defaultValue)
    { }

    const T & operator()(const Pos & p) const {return m_data[index(p.x, p.y)];}
    const T & operator()(unsigned int x, unsigned int y) const{return m_data[index(x, y)];}
    T & operator()(const Pos & p){return m_data[index(p.x, p.y)];}
    T & operator()(unsigned int x, unsigned int y){return m_data[index(x, y)];}
    unsigned int index(unsigned int x, unsigned int y) const {return x * m_height + y;}

    unsigned int width() const {return m_width;}
    unsigned int height() const {return m_height;}

private:
    unsigned int m_width;
    unsigned int m_height;
    std::vector<T> m_data;
};

class LabyResolve
{
    std::string ReadFile(std::string path);
    Pos GetSize(const std::string & data);
    Matrix<signed char> ConvertData(const std::string & data);
    Matrix<signed char> SimplifyMatrix(const Matrix<signed char> & data);
    std::vector<Pos> Resolve(const Matrix<signed char> & mat, const Pos & start, const Pos & end);
    void Draw(const Matrix<signed char> & mat, const std::vector<Pos> & data);

    static signed char Set(bool right, bool down);
    static bool Right(signed char data);
    static bool Down(signed char data);
public:
    LabyResolve(const std::string & filename, const Pos & start, const Pos & end);
};

#endif // LABYRESOLVE_H

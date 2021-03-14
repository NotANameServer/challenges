#include <QImage>

#include <iostream>
#include <fstream>
#include <queue>
#include <vector>
#include <array>
#include <algorithm>
#include <chrono>

#include "labyresolve.h"

LabyResolve::LabyResolve(const std::string & filename, const Pos & start, const Pos & end)
{
    auto startTime = std::chrono::system_clock::now();

    auto data = ReadFile(filename);

    std::chrono::duration<double> elapsed = std::chrono::system_clock::now() - startTime;
    std::cout << "Read: " << elapsed.count() << "s" << std::endl;

    auto mat = ConvertData(data);

    elapsed = std::chrono::system_clock::now() - startTime;
    std::cout << "Convert: " << elapsed.count() << "s" << std::endl;

    auto simplifiedMat = SimplifyMatrix(mat);

    elapsed = std::chrono::system_clock::now() - startTime;
    std::cout << "Simplify: " << elapsed.count() << "s" << std::endl;

    //start & end are not simplified
    Pos simplifiedStart((start.x - 1) / 2, (start.y - 1) / 2);
    Pos simplifiedEnd((end.x - 1) / 2, (end.y - 1) / 2);

    auto path = Resolve(simplifiedMat, simplifiedStart, simplifiedEnd);

    elapsed = std::chrono::system_clock::now() - startTime;
    std::cout << "Resolve: " << elapsed.count() << "s" << std::endl;

    Draw(simplifiedMat, path);

    elapsed = std::chrono::system_clock::now() - startTime;
    std::cout << "Draw: " << elapsed.count() << "s" << std::endl;
}

std::string LabyResolve::ReadFile(std::string path)
{
    constexpr auto read_size = std::size_t{4096};
    auto stream = std::ifstream{path.data()};
    stream.exceptions(std::ios_base::badbit);

    auto out = std::string{};
    auto buf = std::string(read_size, '\0');
    while (stream.read(& buf[0], read_size)) {
        out.append(buf, 0, stream.gcount());
    }
    out.append(buf, 0, stream.gcount());
    return out;
}

Pos LabyResolve::GetSize(const std::string & data)
{
    //we assume that all lines are the same size
    int lineSize = 0;
    int nbLine = 0;

    size_t index = 0;
    while(true)
    {
        auto newIndex = data.find('\n', index);
        nbLine++;
        if(newIndex == std::string::npos)
            break;
        index = newIndex;
        if(lineSize == 0)
            lineSize = newIndex;
        index = newIndex + 1;
   }

    return {lineSize, nbLine};
}


Matrix<signed char> LabyResolve::ConvertData(const std::string & data)
{
    auto size = GetSize(data);
    Matrix<signed char> mat(size.x, size.y);

    unsigned int x = 0;
    unsigned int y = 0;

    for(auto c : data)
    {
        switch (c)
        {
        case '#':
            if(x < mat.width() && y < mat.height())
                mat(x, y) = 1;
            x++;
            break;
        case ' ':
        case '.':
            if(x < mat.width() && y < mat.height())
                mat(x, y) = 0;
            x++;
            break;
        case '\n':
            x = 0;
            y++;
            break;
        }
    }

    return mat;
}

Matrix<signed char> LabyResolve::SimplifyMatrix(const Matrix<signed char> & data)
{
    //assume that the laby have a #### layer around it, and all the cases with odd x & y index are #wall, all cases with even x & y are ground

    Matrix<signed char> mat((data.width() - 1) / 2, (data.height() - 1) / 2);

    for(unsigned int i = 0 ; i < mat.width(); i++)
    {
        for(unsigned int j = 0 ; j < mat.height() ; j++)
        {
            unsigned int dataX = i * 2 + 1;
            unsigned int dataY = j * 2 + 1;

            bool right = data(dataX + 1, dataY) == 0;
            bool down = data(dataX, dataY + 1) == 0;

            mat(i, j) = Set(right, down);
        }
    }
    return mat;
}

signed char LabyResolve::Set(bool right, bool down)
{
    return right + (down << 1);
}

bool LabyResolve::Right(signed char data)
{
    return (data & 1) > 0;
}

bool LabyResolve::Down(signed char data)
{
    return (data & 2) > 0;
}

std::vector<Pos> LabyResolve::Resolve(const Matrix<signed char> & mat, const Pos & start, const Pos & end)
{
    //simple Dijkstra
    struct PosData
    {
        Pos pos;
        Pos previousPos;
        int weight;
    };

    std::queue<PosData> nextPos;
    std::vector<PosData> computedPos;
    computedPos.reserve(mat.width() * mat.height());
    std::vector<bool> inListPos(mat.width() * mat.height(), false);

    auto IsPosValid = [&mat, &inListPos](const Pos & start, const Pos & dir)
    {
        Pos end = start + dir;

        //first check matrix
        //we assume that dir.x == 1 or -1, or dir.y == 1 or -1, only one
        if(dir.x == 1)
        {
            if(!Right(mat(start)))
                return false;
        }
        else if(dir.x == -1)
        {
            if(end.x < 0 || !Right(mat(end)))
                return false;
        }
        else if(dir.y == 1)
        {
            if(!Down(mat(start)))
                return false;
        }
        else if(dir.y == -1)
        {
            if(end.y < 0 || !Down(mat(end)))
                return false;
        }

        //if direction valid, check if already used
        if(inListPos[mat.index(end.x, end.y)])
            return false;

        return true;
    };

    nextPos.push({start, start, 0});
    inListPos[mat.index(start.x, start.y)] = true;

    std::array<Pos, 4> directions{Pos(0, 1), Pos(0, -1), Pos(1, 0), Pos(-1, 0)};

    Pos tempEnd = end;

    while(!nextPos.empty())
    {
        auto pos = nextPos.front();
        nextPos.pop();

        computedPos.push_back(pos);

        //found the end
        if(pos.pos == end)
            break;

        for(const auto & dir : directions)
        {
            if(IsPosValid(pos.pos, dir))
            {
                Pos newPos = pos.pos + dir;
                nextPos.push({newPos, pos.pos, pos.weight + 1});
                inListPos[mat.index(newPos.x, newPos.y)] = true;
            }
        }
    }

    std::vector<Pos> path;

    if(computedPos.empty())
        return path;
    if(computedPos.back().pos != tempEnd)
        return path;

    Pos current = tempEnd;

    for(auto it = computedPos.rbegin(); it != computedPos.rend(); it++)
    {
        if(it->pos == current)
        {
            path.push_back(current);
            current = it->previousPos;
        }
    }

    std::reverse(path.begin(), path.end());

    return path;
}

void LabyResolve::Draw(const Matrix<signed char> & mat, const std::vector<Pos> & data)
{
    int width = mat.width() * 2 + 1;
    int height = mat.height() * 2 + 1;

    int wall = qRgb(20, 20, 20);
    int ground = qRgb(200, 200, 200);
    int path = qRgb(220, 30, 30);

    QImage image(width, height, QImage::Format_RGB32);
    image.fill(wall);

    //setPixel is slow but ... it work !
    for(unsigned int i = 0 ; i < mat.width() ; i++)
    {
        for(unsigned int j = 0 ; j < mat.height() ; j++)
        {
            int x = i * 2 + 1;
            int y = j * 2 + 1;

            image.setPixel(x + 1, y + 1, wall);
            size_t index = std::distance(data.begin(), std::find(data.begin(), data.end(), Pos(i, j)));
            if(index < data.size())
                image.setPixel(x, y, path);
            else image.setPixel(x, y, ground);

            if(!Right(mat(i, j)))
                image.setPixel(x+1, y, wall);
            else if(index < data.size())
            {
                if((index > 0 && data[index - 1] == Pos(i + 1, j)) || (index < data.size()-1 && data[index + 1] == Pos(i + 1, j)))
                    image.setPixel(x+1, y, path);
                else image.setPixel(x+1, y, ground);
            }
            else image.setPixel(x+1, y, ground);

            if(!Down(mat(i, j)))
                image.setPixel(x, y+1, wall);
            else if(index < data.size())
            {
                if((index > 0 && data[index - 1] == Pos(i, j + 1)) || (index < data.size()-1 && data[index + 1] == Pos(i, j + 1)))
                    image.setPixel(x, y+1, path);
                else image.setPixel(x, y+1, ground);
            }
            else image.setPixel(x, y+1, ground);
        }
    }

    image.save("maze.png");
}

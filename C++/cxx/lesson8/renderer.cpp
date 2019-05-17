#include "renderer.h"

#include <iostream>


namespace gfx {

void Renderer::draw_line(Point a, Point b)
{
    std::cout << "Drawing line ---: " << a.x() << ", " << a.y()
              << " to " << b.x() << ", " << b.y() << '\n';
}

void Renderer::draw_circle(Point center, double radius)
{
    std::cout << "Drawing circle O: " << center.x() << ", " << center.y()
              << " with radius " << radius << '\n';
}

void Renderer::draw_point(Point p)
{
    std::cout << "Drawing point .: " << p.x() << ", " << p.y() << '\n';
}


}

#include "renderer.h"

#include <iostream>


namespace gfx {

void Renderer::draw_line(Point a, Point b)
{
	std::cout << "Drawing line from "
	          << "(" << a.first << ", " << a.second << ")"
	          << " to "<< "(" << b.first << ", " << b.second << ")" << '\n';
}

void Renderer::draw_circle(Point center, double radius)
{
	std::cout << "Drawing circle at "
	          << "(" << center.first << ", " << center.second <<")"
			  << " with radius " << radius << '\n';
}

void Renderer::draw_point(Point p)
{
	std::cout << "Drawing point at "
	          << "(" << p.first << ", " << p.second << ")\n";
}

} // namespace gfx

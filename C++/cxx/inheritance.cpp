#include <vector>
#include <iostream>

#include "renderer.h"
#include "unique_ptr.hpp"


class Shape {
 public:
	virtual ~Shape() { std::cout << "Shape destroyed correctly.\n"; };
	virtual void draw(gfx::Renderer& r) const = 0;
};


class Point: public Shape {
 public:
	Point(const gfx::Point &p):
		coordinate{p}
	{}

	void draw(gfx::Renderer& r) const { r.draw_point(coordinate); }

 private:
	gfx::Point coordinate;
};


class Square: public Shape {
 public:
	Square(const gfx::Point& anchor, int side):
		top_left{anchor},
		side{side}
	{}

	void draw(gfx::Renderer& r) const
	{
		using gfx::Point;
		auto top_right = Point{top_left.first + side, top_left.second};
		auto bottom_left = Point{top_left.first, top_left.second + side};
		auto bottom_right = Point{top_left.first + side, top_left.second + side};
		r.draw_line(top_left, top_right);
		r.draw_line(top_right, bottom_right);
		r.draw_line(bottom_right, bottom_left);
		r.draw_line(bottom_left, top_left);
	}

 private:
	gfx::Point top_left;
	int side;
};


class Circle: public Shape {
 public:
	Circle(const gfx::Point& pos, double radius):
		center{pos},
		radius{radius}
	{}

	void draw(gfx::Renderer& r) const { r.draw_circle(center, radius); }

 private:
	gfx::Point center;
	double radius;
};


int main()
{
	using namespace gfx;
	Renderer r;

	std::vector<util::unique_ptr<Shape>> shapes;

	shapes.emplace_back(util::make_unique<Square>(gfx::Point{-3, 3}, 6));
	shapes.emplace_back(util::make_unique<Circle>(gfx::Point{3, 4}, 5.0));
	shapes.emplace_back(util::make_unique<::Point>(gfx::Point{-1, -1}));

	for (auto &s: shapes)
		s->draw(r);

	std::cout << "--- moving ---\n";
	auto shapes2 = std::move(shapes);

	for (auto &s: shapes)
		s->draw(r);

	for (auto &s: shapes2)
		s->draw(r);
}

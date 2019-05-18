#include <vector>
#include <iostream>

#include "renderer.h"
#include "unique_ptr.hpp"


class Shape {
 public:
	virtual ~Shape() { std::cout << "Shape destroyed correctly.\n"; };
	virtual void draw(gfx::Renderer& r) const = 0;
};


class Point {
 public:
	Point(const gfx::Point &p):
		coordinate{p}
	{}

	void draw(gfx::Renderer& r) const { r.draw_point(coordinate); }

 private:
	gfx::Point coordinate;
};


class Square {
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


class Circle {
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


template <typename D>
void draw(gfx::Renderer r, const D& d)
{
	d.draw(r);
}

// these must be defined after the generic draw and before Drawable class
void draw(gfx::Renderer& r, const gfx::Point& p)
{
	r.draw_point(p);
}

void draw(gfx::Renderer& r, const std::vector<gfx::Point>& points)
{
	for (int i = 0; i < points.size() - 1; ++i)
		r.draw_line(points[i], points[i+1]);

	r.draw_line(points.back(), points.front());
}

class Drawable {
	class Model;

 public:
	template <typename D>
	Drawable(D d):
		s{util::make_unique<ModelImpl<D>>(std::move(d))}
	{}

	// "polymorphic" type is now copyable
	Drawable(const Drawable& d):
		s{d.s->copy()}
	{}

	void draw(gfx::Renderer& r) const { s->draw(r); }

 private:
	util::unique_ptr<Model> s;

	class Model {
	 public:
		virtual ~Model() {}
		virtual void draw(gfx::Renderer& r) const = 0;
		virtual util::unique_ptr<Model> copy() const = 0;
	};

	// ModelImpl<D> contains each D's actual data, passed to constructor
	template <typename D>
	class ModelImpl: public Model {
	 public:
		ModelImpl(D d):
			d{std::move(d)}
		{}

		util::unique_ptr<Model> copy() const
		{
			return util::unique_ptr<ModelImpl>{ new ModelImpl{d} };
		}

		// achieves polymorphism through overloading of draw(r, d) for any d
		void draw(gfx::Renderer& r) const { ::draw(r, d); }

	 private:
		D d;
	};
};

int main()
{
	using namespace gfx;
	Renderer r;

	std::vector<Drawable> shapes;

	shapes.emplace_back(Square(gfx::Point{-3, 3}, 6));
	shapes.emplace_back(Circle(gfx::Point{3, 4}, 5.0));
	shapes.emplace_back(gfx::Point{-1, -1});
	shapes.emplace_back(std::vector({gfx::Point{1, 2}, gfx::Point{1, 4}, gfx::Point{4, 1}}));

	auto shapes2 = (shapes);

	for (auto &s: shapes)
		s.draw(r);

	std::cout << "--- copies ---\n";

	for (auto &s: shapes2)
		s.draw(r);
}

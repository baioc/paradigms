#include <iostream>
#include <vector>

#include "point.h"
#include "renderer.h"
#include "unique_ptr.hpp"


class Shape {
public:
    virtual ~Shape() {
        std::cout << "Shape destroyed correctly.\n";
    };
    virtual void draw(gfx::Renderer& r) const = 0;
};


class Point: public Shape {
    Point(const gfx::Point &p):
        coordinate{p}
    {}

    void draw(gfx::Renderer& r) const
    {
        r.draw_point(coordinate);
    }

private:
    gfx::Point coordinate;
};


class Square: public Shape {
public:
    Square(const gfx::Point& p, double side):
        top_left{p},
        side{side}
    {}

    void draw(gfx::Renderer& r) const
    {
        using gfx::Point;

        auto top_right = Point{top_left.x() + side, top_left.y()};
        auto bottom_left = Point{top_left.x(), top_left.y() + side};
        auto bottom_right = Point{top_left.x() + side, top_left.y() + side};
        r.draw_line(top_left, top_right);
        r.draw_line(top_right, bottom_right);
        r.draw_line(bottom_right, bottom_left);
        r.draw_line(bottom_left, top_left);
    }

private:
    gfx::Point top_left;
    double side;
};


class Circle: public Shape {
public:
    Circle(const gfx::Point& p, double radius):
        center{p},
        radius{radius}
    {}

    void draw(gfx::Renderer& r) const
    {
        r.draw_circle(center, radius);
    }

private:
    gfx::Point center;
    double radius;
};


enum class ShapeType {
    Square,
    Circle
};


util::unique_ptr<Shape> read_shape(ShapeType s)
{
    using gfx::Point;
    switch (s) {
        case ShapeType::Square:
            return util::make_unique<Square>(Point{1, 2}, 5.0);
        case ShapeType::Circle:
            return util::make_unique<Circle>(Point{20, 40}, 20.0);
    }
}

int main()
{
    using namespace gfx;
    Renderer r;

    auto shapes = std::vector<util::unique_ptr<Shape>>{2};

    shapes[0] = util::make_unique<Square>(gfx::Point{1, 2}, 5.0);
    shapes[1] = util::make_unique<Circle>(gfx::Point{20, 40}, 20.0);

    for (auto &s: shapes) {
        (*s).draw(r);
        s->draw(r);
    }

    std::cout << std::is_move_assignable_v<util::unique_ptr<Shape>>;
    std::cout << std::is_move_constructible_v<util::unique_ptr<Shape>>;
}

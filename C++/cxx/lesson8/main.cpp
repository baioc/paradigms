#include <iostream>
#include <memory>
#include <vector>

#include "point.h"
#include "renderer.h"


class Point {
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


class Square {
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


class Circle {
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


template <typename D>
void draw(gfx::Renderer r, const D& d)
{
    d.draw(r);
}

void draw(gfx::Renderer& r, const gfx::Point& p)
{
    r.draw_point(p);
}

void draw(gfx::Renderer& r, const std::vector<gfx::Point>& points)
{
    for (int i = 0; i < points.size() - 1; ++i) {
        r.draw_line(points[i], points[i+1]);
    }

    r.draw_line(points.back(), points.front());
}

class Drawable {
    class Model {
    public:
        virtual ~Model() {}
        virtual std::unique_ptr<Model> copy() const = 0;
        virtual void draw(gfx::Renderer& r) const = 0;
    };

    template <typename D>
    class ModelImpl: public Model {
    public:
        ModelImpl(D d):
            d{std::move(d)}
        {}

        std::unique_ptr<Model> copy() const
        {
            return std::unique_ptr<ModelImpl>{ new ModelImpl{d} };
        }

        void draw(gfx::Renderer& r) const
        {
            ::draw(r, d);
        }

    private:
        D d;
    };

public:
    template <typename D>
    Drawable(D d):
        s{std::make_unique<ModelImpl<D>>(std::move(d))}
    {}

    Drawable(const Drawable& d):
        s{d.s->copy()}
    {}

    void draw(gfx::Renderer& r) const
    {
        s->draw(r);
    }

private:
    std::unique_ptr<Model> s;
};


enum class ShapeType {
    Square,
    Circle
};


Drawable read_shape(ShapeType s)
{
    using gfx::Point;

    switch (s) {
        case ShapeType::Square:
            return Square(Point{1, 2}, 5.0);
        case ShapeType::Circle:
            return Circle(Point{20, 40}, 20.0);
    }
}

class Polygon {
public:
    Polygon(std::vector<gfx::Point> points):
        points{std::move(points)}
    {}

    void draw(gfx::Renderer& r) const
    {
        ::draw(r, points);
    }

private:
    std::vector<gfx::Point> points;
};

void f(std::unique_ptr<int>&& u)
{
    u;
}

int main()
{
    auto a = std::make_unique<int>(1);

    f(std::move(a));

    *a += 2;

    using namespace gfx;
    Renderer r;

    auto shapes = std::vector<Drawable>{};

    shapes.emplace_back(Square(gfx::Point{1, 2}, 5.0));
    shapes.emplace_back(Circle(gfx::Point{20, 40}, 20.0));
    shapes.emplace_back(Polygon({gfx::Point{1, 2}, gfx::Point{1, 4}, gfx::Point{4, 1}}));
    shapes.emplace_back(gfx::Point{1, 2});
    shapes.emplace_back(std::vector({gfx::Point{1, 2}, gfx::Point{1, 4}, gfx::Point{4, 1}}));

    auto shapes2 = shapes;

    for (auto &s: shapes) {
        s.draw(r);
    }

    /*
    for (auto &s: shapes2) {
        s.draw(r);
    }
    */
}

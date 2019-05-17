#ifndef GFX_POINT_H
#define GFX_POINT_H

namespace gfx {

class Point {
public:
    Point(double x, double y):
        x_{x},
        y_{y}
    {}

    double x() const { return x_; }
    double y() const { return y_; }

private:
    double x_, y_;
};

}

#endif

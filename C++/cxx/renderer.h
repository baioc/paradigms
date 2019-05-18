#ifndef GFX_RENDERER_H
#define GFX_RENDERER_H

#include <utility>


namespace gfx {

using Point = std::pair<int,int>;

class Renderer {
 public:
	void draw_line(Point a, Point b);
	void draw_circle(Point center, double radius);
	void draw_point(Point p);
};

} // namespace gfx

#endif

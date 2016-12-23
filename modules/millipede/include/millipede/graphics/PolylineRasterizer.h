/***
 * millipede: PolylineRasterizer.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_POLYLINERASTERIZER
#define H_MILLIPEDE_POLYLINERASTERIZER

#include <list>
#include <vector>

#include <millipede/math/Vector2.h>

namespace mp {

std::vector<Vector2i> rasterize_polyline(const std::list<Vector2i>& polyline);

}

#endif

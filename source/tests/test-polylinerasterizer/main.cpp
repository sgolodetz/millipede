/***
 * test-polylinerasterizer: main.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#define BOOST_TEST_MODULE PolylineRasterizer Test
#include <boost/test/included/unit_test.hpp>

#include <common/graphics/PolylineRasterizer.h>
using namespace mp;

BOOST_AUTO_TEST_CASE(rectangle_test)
{
	std::list<Vector2i> input;
	input.push_back(Vector2i(0,0));
	input.push_back(Vector2i(3,0));
	input.push_back(Vector2i(3,2));
	input.push_back(Vector2i(0,2));
	std::vector<Vector2i> output = rasterize_polyline(input);
	// TODO: Check the output.
}

BOOST_AUTO_TEST_CASE(triangle_test)
{
	std::list<Vector2i> input;
	input.push_back(Vector2i(1,0));
	input.push_back(Vector2i(0,1));
	input.push_back(Vector2i(2,2));
	std::vector<Vector2i> output = rasterize_polyline(input);
	// TODO: Check the output.
}

BOOST_AUTO_TEST_CASE(tricky_test)
{
	std::list<Vector2i> input;
	input.push_back(Vector2i(0,0));
	input.push_back(Vector2i(2,0));
	input.push_back(Vector2i(2,1));
	input.push_back(Vector2i(4,1));
	input.push_back(Vector2i(4,0));
	input.push_back(Vector2i(5,0));
	input.push_back(Vector2i(5,2));
	input.push_back(Vector2i(0,2));
	std::vector<Vector2i> output = rasterize_polyline(input);
	// TODO: Check the output.
}

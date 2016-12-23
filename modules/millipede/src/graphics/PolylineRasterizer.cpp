/***
 * millipede: PolylineRasterizer.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "graphics/PolylineRasterizer.h"

#include <algorithm>
#include <cassert>
#include <climits>
#include <map>
#include <set>
#include <utility>

#include <boost/optional.hpp>
#include <boost/utility.hpp>

#include "math/NumericUtil.h"

namespace {

using namespace mp;

struct Edge
{
	Vector2i u, v;	// u is the endpoint with the lower y coordinate, or the one with the lower x coordinate if it's a horizontal edge (note that u != v)

	Edge(const Vector2i& u_, const Vector2i& v_)
	:	u(u_), v(v_)
	{
		assert(u != v);
		if(u.y > v.y || (u.y == v.y && u.x > v.x))
		{
			std::swap(u, v);
		}
	}

	bool operator<(const Edge& rhs) const
	{
		// See 3D Graphics Programming and Beyond (Savchenko), p. 108. (Note that I made some changes to handle special cases.)
		return	u.y < rhs.u.y ||
				(u.y == rhs.u.y && v.x < rhs.v.x) ||
				(u.y == rhs.u.y && v.x == rhs.v.x && u.x < rhs.u.x) ||
				(u.y == rhs.u.y && v.x == rhs.v.x && u.x == rhs.u.x && v.y < rhs.v.y);
	}
};

struct ActiveEdge
{
	Edge e;
	double invGradient;
	mutable double x;	// this must be mutable so that it can be modified by propagate()

	explicit ActiveEdge(const Edge& e_)
	:	e(e_), x(e_.u.x)
	{
		double dx = e.v.x - e.u.x;
		double dy = e.v.y - e.u.y;
		invGradient = dx/dy;
	}

	bool operator<(const ActiveEdge& rhs) const
	{
		return e < rhs.e;
	}

	// Note: This must be const so that it can be called on set elements.
	void propagate() const
	{
		x += invGradient;
	}
};

}

namespace mp {

std::vector<Vector2i> rasterize_polyline(const std::list<Vector2i>& polyline)
{
	std::vector<Vector2i> output;

	// Step 1: Construct the edge set.
	int minY = INT_MAX, maxY = INT_MIN;
	std::set<Edge> edges;
	for(std::list<Vector2i>::const_iterator it=polyline.begin(), iend=polyline.end(); it!=iend; ++it)
	{
		// Add the edge to the edge set (provided it isn't degenerate).
		std::list<Vector2i>::const_iterator jt = boost::next(it);
		if(jt == polyline.end()) jt = polyline.begin();
		if(*it != *jt) edges.insert(Edge(*it, *jt));

		// Take the opportunity to calculate the minimum and maximum scanlines here.
		minY = std::min(minY, it->y);
		maxY = std::max(maxY, it->y);
	}

	// Step 2: Rasterize each scanline in turn.
	std::set<ActiveEdge> activeEdges;
	std::set<Edge>::const_iterator nextEdge = edges.begin();

	for(int y=minY; y<=maxY; ++y)
	{
		// (a) Add any new edges to the active edge list.
		while(nextEdge != edges.end() && nextEdge->u.y == y)
		{
			activeEdges.insert(ActiveEdge(*nextEdge));
			++nextEdge;
		}

		// (b) Construct edge points along the current scanline (handling horizontal edges as a special case).
		const int UP = 1, DOWN = 4, HORIZONTAL = 16;	// note: this is like the usual bit-packing technique, but we can add each twice
		typedef int EdgePointFlag;
		std::map<int,EdgePointFlag> edgePoints;
		for(std::set<ActiveEdge>::const_iterator it=activeEdges.begin(), iend=activeEdges.end(); it!=iend; ++it)
		{
			const ActiveEdge& activeEdge = *it;

			if(activeEdge.e.u.y != activeEdge.e.v.y)
			{
				// Up or down edge.
				int x = NumericUtil::round_to_nearest<int>(activeEdge.x);
				if(activeEdge.e.u.y < y)	edgePoints[x] += UP;
				else						edgePoints[x] += DOWN;
			}
			else
			{
				// Horizontal edge.
				edgePoints[activeEdge.e.u.x] += HORIZONTAL;
				edgePoints[activeEdge.e.v.x] += HORIZONTAL;
			}

			activeEdge.propagate();
		}

		// (c) Rasterize the scanline using the even-odd parity method.
		bool drawingOn = false;
		EdgePointFlag beginHorizontalFlag = 0;
		for(std::map<int,EdgePointFlag>::const_iterator it=edgePoints.begin(), iend=edgePoints.end(); it!=iend; ++it)
		{
			int x = it->first;
			EdgePointFlag flag = it->second;

			output.push_back(Vector2i(x,y));

			switch(flag)
			{
				case DOWN+UP:
				case UP:
				{
					drawingOn = !drawingOn;
					break;
				}
				case HORIZONTAL+DOWN:
				{
					if(beginHorizontalFlag == HORIZONTAL+UP) drawingOn = !drawingOn;
					if(beginHorizontalFlag != 0) beginHorizontalFlag = 0;
					else beginHorizontalFlag = flag;
					break;
				}
				case HORIZONTAL+UP:
				{
					if(beginHorizontalFlag == HORIZONTAL+DOWN) drawingOn = !drawingOn;
					if(beginHorizontalFlag != 0) beginHorizontalFlag = 0;
					else beginHorizontalFlag = flag;
					break;
				}
				default:
				{
					// This includes cases like DOWN+DOWN, HORIZONTAL+HORIZONTAL and UP+UP, as well as
					// unusual cases that we don't try and handle.
					break;
				}
			}

			std::map<int,EdgePointFlag>::const_iterator jt = boost::next(it);
			if(jt == iend) continue;

			if(drawingOn || beginHorizontalFlag != 0)
			{
				int nextX = jt->first;
				for(int k=x+1; k<nextX; ++k)
				{
					output.push_back(Vector2i(k,y));
				}
			}
		}

		// (d) Remove any old edges from the active edge list.
		for(std::set<ActiveEdge>::const_iterator it=activeEdges.begin(), iend=activeEdges.end(); it!=iend; /* No-op */)
		{
			if(it->e.v.y == y) activeEdges.erase(it++);
			else ++it;
		}
	}

	return output;
}

}

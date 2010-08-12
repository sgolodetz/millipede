/***
 * millipede: LineBasedDrawingTool.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "LineBasedDrawingTool.h"

#include <cmath>
#include <map>
#include <set>

#include <boost/utility.hpp>

#include <common/ogl/WrappedGL.h>
#include <common/util/ITKImageUtil.h>

namespace mp {

//#################### PUBLIC METHODS ####################
bool LineBasedDrawingTool::has_started() const
{
	return !m_drawnLocations.empty();
}

void LineBasedDrawingTool::render() const
{
	glColor3d(1.0, 1.0, 1.0);
	glBegin(GL_LINE_STRIP);
		for(std::list<std::pair<Vector2i,itk::Index<2> > >::const_iterator it=m_drawnLocations.begin(), iend=m_drawnLocations.end(); it!=iend; ++it)
		{
			glVertex2i(it->first.x, it->first.y);
		}
	glEnd();

	if(m_drawnLocations.size() >= 2)
	{
		const Vector2i& first = m_drawnLocations.front().first, last = m_drawnLocations.back().first;
		glColor3d(1.0, 0.0, 1.0);
		glBegin(GL_LINES);
			glVertex2i(first.x, first.y);
			glVertex2i(last.x, last.y);
		glEnd();
	}
}

void LineBasedDrawingTool::reset()
{
	m_drawnLocations.clear();
}

std::vector<itk::Index<2> > LineBasedDrawingTool::selected_positions() const
{
	// Step 1: Determine all the locations touched by lines.
	std::list<itk::Index<2> > locations;

	std::list<std::pair<Vector2i,itk::Index<2> > >::const_iterator it = m_drawnLocations.begin();
	std::list<std::pair<Vector2i,itk::Index<2> > >::const_iterator jt = boost::next(it);
	while(jt != m_drawnLocations.end())
	{
		std::list<itk::Index<2> > result = line_positions(it->second, jt->second);
		locations.splice(locations.end(), result);
		++it, ++jt;
	}
	std::list<itk::Index<2> > result = line_positions(it->second, m_drawnLocations.begin()->second);
	locations.splice(locations.end(), result);

	// Step 2: Make a map from scanlines (y = k) to x coordinates.
	std::map<long,std::set<long> > scanlines;
	for(std::list<itk::Index<2> >::const_iterator it=locations.begin(), iend=locations.end(); it!=iend; ++it)
	{
		scanlines[(*it)[1]].insert((*it)[0]);
	}

	// Step 3: Rasterize each of the scanlines in cunning fashion.
	std::vector<itk::Index<2> > selectedPositions;
	for(std::map<long,std::set<long> >::const_iterator it=scanlines.begin(), iend=scanlines.end(); it!=iend; ++it)
	{
		int y = it->first;
		const std::set<long>& scanline = it->second;

		long prevX = *scanline.begin() - 1;
		int gapsTraversed = 0;
		for(std::set<long>::const_iterator jt=scanline.begin(), jend=scanline.end(); jt!=jend; ++jt)
		{
			int x = *jt;
			selectedPositions.push_back(ITKImageUtil::make_index(x,y));

			// Process any gap we encounter.
			if(x != prevX + 1)
			{
				if(gapsTraversed % 2 == 0)
				{
					for(int k=prevX+1; k<x; ++k)
					{
						selectedPositions.push_back(ITKImageUtil::make_index(k,y));
					}
				}
				++gapsTraversed;
			}

			prevX = x;
		}
	}

	return selectedPositions;
}

//#################### PRIVATE METHODS ####################
std::list<itk::Index<2> > LineBasedDrawingTool::line_positions(const itk::Index<2>& start, const itk::Index<2>& end)
{
	std::list<itk::Index<2> > positions;

	const long& x0 = start[0];
	const long& y0 = start[1];
	const long& x1 = end[0];
	const long& y1 = end[1];

	const long dx = x1 - x0;
	const long dy = y1 - y0;
	const bool swapXY = abs(dy) > abs(dx);

	const long& a1 = swapXY ? y1 : x1;
	const long& da = swapXY ? dy : dx;
	const long& db = swapXY ? dx : dy;
	const long signA = da >= 0 ? 1 : -1;
	const long signB = db >= 0 ? 1 : -1;
	const long sign = signA * signB;

	long K = (da*signB - 2*db*signA) * sign;	// the decision variable
	const long ka = -2 * db * signA * sign;		// the change in K caused by movement in the a direction
	const long kb = 2 * da * signB * sign;		// the change in K caused by movement in the b direction

	itk::Index<2> cur = start;

	long& a = swapXY ? cur[1] : cur[0];
	long& b = swapXY ? cur[0] : cur[1];

	while(a != a1 + signA)
	{
		positions.push_back(cur);

		if(K < 0)
		{
			b += signB;
			K += kb;
		}

		a += signA;
		K += ka;
	}

	return positions;
}

}

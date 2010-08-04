/***
 * millipede: LineBasedDrawingTool.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "LineBasedDrawingTool.h"

#include <common/ogl/WrappedGL.h>

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
	// NYI
	return std::vector<itk::Index<2> >();
}

}

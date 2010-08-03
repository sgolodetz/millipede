/***
 * millipede: LineBasedDrawingTool.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "LineBasedDrawingTool.h"

#include <common/ogl/WrappedGL.h>

namespace mp {

//#################### PRIVATE ABSTRACT METHODS ####################
void LineBasedDrawingTool::render_sub() const
{
	// No-op by default
}

//#################### PUBLIC METHODS ####################
bool LineBasedDrawingTool::has_started() const
{
	return !m_drawnPixels.empty();
}

void LineBasedDrawingTool::render() const
{
	glColor3d(1.0, 1.0, 1.0);
	glBegin(GL_LINE_STRIP);
		for(std::list<Vector2i>::const_iterator it=m_drawnPixels.begin(), iend=m_drawnPixels.end(); it!=iend; ++it)
		{
			glVertex2i(it->x, it->y);
		}
	glEnd();

	if(m_drawnPixels.size() >= 2)
	{
		const Vector2i& first = m_drawnPixels.front(), last = m_drawnPixels.back();
		glColor3d(1.0, 0.0, 1.0);
		glBegin(GL_LINES);
			glVertex2i(first.x, first.y);
			glVertex2i(last.x, last.y);
		glEnd();
	}

	render_sub();
}

void LineBasedDrawingTool::reset()
{
	m_drawnPixels.clear();
}

std::vector<Vector2i> LineBasedDrawingTool::selected_pixels() const
{
	// NYI
	return std::vector<Vector2i>();
}

}

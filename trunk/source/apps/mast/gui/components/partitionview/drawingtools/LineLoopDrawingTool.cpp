/***
 * millipede: LineLoopDrawingTool.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "LineLoopDrawingTool.h"

#include <common/ogl/WrappedGL.h>

namespace mp {

//#################### PUBLIC METHODS ####################
void LineLoopDrawingTool::mouse_dragged(const Vector2i& p)
{
	if(m_grabbedPoint)
	{
		**m_grabbedPoint = p;
	}
}

void LineLoopDrawingTool::mouse_pressed(const Vector2i& p)
{
	const int MAX_DIST_SQUARED = 20*20;
	for(std::list<Vector2i>::iterator it=m_drawnPixels.begin(), iend=m_drawnPixels.end(); it!=iend; ++it)
	{
		if(it->distance_squared(p) < MAX_DIST_SQUARED)
		{
			m_grabbedPoint = it;
			return;
		}
	}

	m_drawnPixels.push_back(p);
}

void LineLoopDrawingTool::mouse_released(const Vector2i& p)
{
	m_grabbedPoint.reset();
}

void LineLoopDrawingTool::render() const
{
	LineBasedDrawingTool::render();

	// Render all the points in the line loop.
	glPushAttrib(GL_POINT_BIT);

	glPointSize(10.0f);
	glEnable(GL_POINT_SMOOTH);

	glColor3d(0.0, 1.0, 1.0);
	glBegin(GL_POINTS);
		for(std::list<Vector2i>::const_iterator it=m_drawnPixels.begin(), iend=m_drawnPixels.end(); it!=iend; ++it)
		{
			glVertex2i(it->x, it->y);
		}
	glEnd();

	glPopAttrib();
}

void LineLoopDrawingTool::reset()
{
	LineBasedDrawingTool::reset();
	m_grabbedPoint.reset();
}

DrawingTool::ToolStyle LineLoopDrawingTool::style() const
{
	return TOOLSTYLE_MULTICLICK;
}

}

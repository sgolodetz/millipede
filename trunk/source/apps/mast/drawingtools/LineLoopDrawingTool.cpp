/***
 * millipede: LineLoopDrawingTool.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "LineLoopDrawingTool.h"

#include <common/ogl/WrappedGL.h>

namespace mp {

//#################### PUBLIC METHODS ####################
void LineLoopDrawingTool::mouse_dragged(const Vector2i& p_Pixels, const Vector2i& p_Coords)
{
	if(m_grabbedPoint)
	{
		**m_grabbedPoint = std::make_pair(p_Pixels, p_Coords);
	}
}

void LineLoopDrawingTool::mouse_pressed(const Vector2i& p_Pixels, const Vector2i& p_Coords)
{
	const int MAX_DIST_SQUARED = 20*20;
	for(std::list<std::pair<Vector2i,Vector2i> >::iterator it=m_drawnLocations.begin(), iend=m_drawnLocations.end(); it!=iend; ++it)
	{
		if(it->first.distance_squared(p_Pixels) < MAX_DIST_SQUARED)
		{
			m_grabbedPoint = it;
			return;
		}
	}

	m_drawnLocations.push_back(std::make_pair(p_Pixels, p_Coords));
}

void LineLoopDrawingTool::mouse_released(const Vector2i& p_Pixels, const Vector2i& p_Coords)
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
		for(std::list<std::pair<Vector2i,Vector2i> >::const_iterator it=m_drawnLocations.begin(), iend=m_drawnLocations.end(); it!=iend; ++it)
		{
			glVertex2i(it->first.x, it->first.y);
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

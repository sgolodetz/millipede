/***
 * millipede: BoxDrawingTool.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "BoxDrawingTool.h"

#include <common/ogl/WrappedGL.h>

namespace mp {

//#################### PUBLIC METHODS ####################
bool BoxDrawingTool::is_single_pass() const
{
	return true;
}

void BoxDrawingTool::mouse_dragged(const Vector2i& p)
{
	m_other = p;
}

void BoxDrawingTool::mouse_pressed(const Vector2i& p)
{
	m_anchor = p;
	m_other = p;
}

void BoxDrawingTool::render() const
{
#if 1
	// TEMPORARY VERSION
	if(!m_anchor) return;

	const Vector2i& a = *m_anchor;
	const Vector2i& o = *m_other;

	glColor3d(0.0, 1.0, 0.0);
	glBegin(GL_LINE_LOOP);
		glVertex2i(a.x, a.y);
		glVertex2i(o.x, a.y);
		glVertex2i(o.x, o.y);
		glVertex2i(a.x, o.y);
	glEnd();
#endif

	// TODO
}

void BoxDrawingTool::reset()
{
	m_anchor.reset();
	m_other.reset();
}

std::deque<Vector2i> BoxDrawingTool::selected_pixels() const
{
	// NYI
	throw 23;
}

}

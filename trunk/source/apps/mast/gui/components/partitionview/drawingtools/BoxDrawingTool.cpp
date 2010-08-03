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
	if(!m_anchor) return;

	const Vector2i& a = *m_anchor;
	const Vector2i& o = *m_other;
	Vector2i smaller(std::min(a.x, o.x), std::min(a.y, o.y));
	Vector2i larger(std::max(a.x, o.x), std::max(a.y, o.y));

	glPushAttrib(GL_ENABLE_BIT);

	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glEnable(GL_BLEND);
	glColor4d(0.0, 0.0, 1.0, 0.25);
	glBegin(GL_QUADS);
		glVertex2i(smaller.x, smaller.y);
		glVertex2i(larger.x + 1, smaller.y);
		glVertex2i(larger.x + 1, larger.y + 1);
		glVertex2i(smaller.x, larger.y + 1);
	glEnd();

	glDisable(GL_BLEND);
	glColor3d(0.75, 0.0, 0.0);
	glBegin(GL_LINE_LOOP);
		glVertex2i(smaller.x, smaller.y);
		glVertex2i(larger.x + 1, smaller.y);
		glVertex2i(larger.x + 1, larger.y + 1);
		glVertex2i(smaller.x, larger.y + 1);
	glEnd();

	glPopAttrib();
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

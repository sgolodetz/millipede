/***
 * millipede: BoxDrawingTool.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "BoxDrawingTool.h"

#include <common/ogl/WrappedGL.h>

namespace mp {

//#################### PUBLIC METHODS ####################
bool BoxDrawingTool::has_started() const
{
	return m_anchor;
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

std::vector<Vector2i> BoxDrawingTool::selected_pixels() const
{
	std::vector<Vector2i> selectedPixels;
	if(has_started())
	{
		int minX = std::min(m_anchor->x, m_other->x), minY = std::min(m_anchor->y, m_other->y);
		int maxX = std::max(m_anchor->x, m_other->x), maxY = std::max(m_anchor->y, m_other->y);
		selectedPixels.reserve((maxX + 1 - minX) * (maxY + 1 - minY));
		for(int y=minY; y<=maxY; ++y)
			for(int x=minX; x<=maxX; ++x)
				selectedPixels.push_back(Vector2i(x,y));
	}
	return selectedPixels;
}

DrawingTool::ToolStyle BoxDrawingTool::style() const
{
	return TOOLSTYLE_CLICKANDDRAG;
}

}

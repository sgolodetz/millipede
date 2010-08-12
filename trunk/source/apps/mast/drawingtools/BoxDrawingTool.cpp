/***
 * millipede: BoxDrawingTool.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "BoxDrawingTool.h"

#include <common/ogl/WrappedGL.h>
#include <common/util/ITKImageUtil.h>

namespace mp {

//#################### PUBLIC METHODS ####################
bool BoxDrawingTool::has_started() const
{
	return m_anchor_Pixels;
}

void BoxDrawingTool::mouse_dragged(const Vector2i& p_Pixels, const Vector2i& p_Coords)
{
	m_other_Pixels = p_Pixels;
	m_other_Coords = p_Coords;
}

void BoxDrawingTool::mouse_pressed(const Vector2i& p_Pixels, const Vector2i& p_Coords)
{
	m_anchor_Pixels = m_other_Pixels = p_Pixels;
	m_anchor_Coords = m_other_Coords = p_Coords;
}

void BoxDrawingTool::render() const
{
	if(!has_started()) return;

	const Vector2i& a = *m_anchor_Pixels;
	const Vector2i& o = *m_other_Pixels;
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
	m_anchor_Pixels.reset();
	m_other_Pixels.reset();
	m_anchor_Coords.reset();
	m_other_Coords.reset();
}

std::vector<Vector2i> BoxDrawingTool::selected_positions() const
{
	std::vector<Vector2i> selectedPositions;
	if(has_started())
	{
		int minX = std::min((*m_anchor_Coords)[0], (*m_other_Coords)[0]), minY = std::min((*m_anchor_Coords)[1], (*m_other_Coords)[1]);
		int maxX = std::max((*m_anchor_Coords)[0], (*m_other_Coords)[0]), maxY = std::max((*m_anchor_Coords)[1], (*m_other_Coords)[1]);
		selectedPositions.reserve((maxX + 1 - minX) * (maxY + 1 - minY));
		for(int y=minY; y<=maxY; ++y)
			for(int x=minX; x<=maxX; ++x)
				selectedPositions.push_back(Vector2i(x,y));
	}
	return selectedPositions;
}

DrawingTool::ToolStyle BoxDrawingTool::style() const
{
	return TOOLSTYLE_CLICKANDDRAG;
}

}

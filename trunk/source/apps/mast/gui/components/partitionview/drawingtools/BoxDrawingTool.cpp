/***
 * millipede: BoxDrawingTool.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "BoxDrawingTool.h"

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
	// NYI
	throw 23;
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

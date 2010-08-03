/***
 * millipede: LassoDrawingTool.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "LassoDrawingTool.h"

namespace mp {

//#################### PUBLIC METHODS ####################
void LassoDrawingTool::mouse_dragged(const Vector2i& p)
{
	m_drawnPixels.push_back(p);
}

void LassoDrawingTool::mouse_pressed(const Vector2i& p)
{
	m_drawnPixels.push_back(p);
}

DrawingTool::ToolStyle LassoDrawingTool::style() const
{
	return TOOLSTYLE_CLICKANDDRAG;
}

}

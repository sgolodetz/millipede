/***
 * millipede: LineLoopDrawingTool.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "LineLoopDrawingTool.h"

namespace mp {

//#################### PUBLIC METHODS ####################
void LineLoopDrawingTool::mouse_pressed(const Vector2i& p)
{
	m_drawnPixels.push_back(p);
}

DrawingTool::ToolStyle LineLoopDrawingTool::style() const
{
	return TOOLSTYLE_MULTICLICK;
}

}

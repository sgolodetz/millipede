/***
 * millipede: LassoDrawingTool.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "LassoDrawingTool.h"

namespace mp {

//#################### PUBLIC METHODS ####################
void LassoDrawingTool::mouse_dragged(const Vector2i& p_Pixels, const Vector2i& p_Coords)
{
	m_drawnLocations.push_back(std::make_pair(p_Pixels, p_Coords));
}

void LassoDrawingTool::mouse_pressed(const Vector2i& p_Pixels, const Vector2i& p_Coords)
{
	m_drawnLocations.push_back(std::make_pair(p_Pixels, p_Coords));
}

DrawingTool::ToolStyle LassoDrawingTool::style() const
{
	return TOOLSTYLE_CLICKANDDRAG;
}

}

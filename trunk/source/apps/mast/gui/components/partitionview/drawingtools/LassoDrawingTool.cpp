/***
 * millipede: LassoDrawingTool.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "LassoDrawingTool.h"

namespace mp {

//#################### PUBLIC METHODS ####################
void LassoDrawingTool::mouse_dragged(const Vector2i& p_Pixels, const itk::Index<2>& position)
{
	m_drawnLocations.push_back(std::make_pair(p_Pixels, position));
}

void LassoDrawingTool::mouse_pressed(const Vector2i& p_Pixels, const itk::Index<2>& position)
{
	m_drawnLocations.push_back(std::make_pair(p_Pixels, position));
}

DrawingTool::ToolStyle LassoDrawingTool::style() const
{
	return TOOLSTYLE_CLICKANDDRAG;
}

}

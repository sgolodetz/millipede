/***
 * millipede: LassoDrawingTool.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_LASSODRAWINGTOOL
#define H_MILLIPEDE_LASSODRAWINGTOOL

#include "LineBasedDrawingTool.h"

namespace mp {

class LassoDrawingTool : public LineBasedDrawingTool
{
	//#################### PUBLIC METHODS ####################
public:
	void mouse_dragged(const Vector2i& p_Pixels, const Vector2i& position);
	void mouse_pressed(const Vector2i& p_Pixels, const Vector2i& position);
	ToolStyle style() const;
};

}

#endif

/***
 * millipede: LineLoopDrawingTool.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_LINELOOPDRAWINGTOOL
#define H_MILLIPEDE_LINELOOPDRAWINGTOOL

#include "LineBasedDrawingTool.h"

namespace mp {

class LineLoopDrawingTool : public LineBasedDrawingTool
{
	//#################### PUBLIC METHODS ####################
public:
	void mouse_pressed(const Vector2i& p);
	ToolStyle style() const;
};

}

#endif

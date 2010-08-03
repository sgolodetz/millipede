/***
 * millipede: LineBasedDrawingTool.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_LINEBASEDDRAWINGTOOL
#define H_MILLIPEDE_LINEBASEDDRAWINGTOOL

#include <list>

#include "DrawingTool.h"

namespace mp {

class LineBasedDrawingTool : public DrawingTool
{
	//#################### PROTECTED VARIABLES ####################
protected:
	std::list<Vector2i> m_drawnPixels;

	//#################### PUBLIC METHODS ####################
public:
	bool has_started() const;
	void render() const;
	void reset();
	std::vector<Vector2i> selected_pixels() const;
};

}

#endif

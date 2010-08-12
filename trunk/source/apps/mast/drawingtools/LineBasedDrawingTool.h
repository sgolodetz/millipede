/***
 * millipede: LineBasedDrawingTool.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_LINEBASEDDRAWINGTOOL
#define H_MILLIPEDE_LINEBASEDDRAWINGTOOL

#include <list>
#include <utility>

#include "DrawingTool.h"

namespace mp {

class LineBasedDrawingTool : public DrawingTool
{
	//#################### PROTECTED VARIABLES ####################
protected:
	std::list<std::pair<Vector2i,Vector2i> > m_drawnLocations;

	//#################### PUBLIC METHODS ####################
public:
	bool has_started() const;
	void render() const;
	void reset();
	std::vector<Vector2i> selected_positions() const;
};

}

#endif

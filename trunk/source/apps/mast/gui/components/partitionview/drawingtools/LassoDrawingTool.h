/***
 * millipede: LassoDrawingTool.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_LASSODRAWINGTOOL
#define H_MILLIPEDE_LASSODRAWINGTOOL

#include <list>

#include "DrawingTool.h"

namespace mp {

class LassoDrawingTool : public DrawingTool
{
	//#################### PRIVATE VARIABLES ####################
private:
	std::list<Vector2i> m_drawnPixels;

	//#################### PUBLIC METHODS ####################
public:
	bool has_started() const;
	void mouse_dragged(const Vector2i& p);
	void mouse_pressed(const Vector2i& p);
	void render() const;
	void reset();
	std::vector<Vector2i> selected_pixels() const;
};

}

#endif

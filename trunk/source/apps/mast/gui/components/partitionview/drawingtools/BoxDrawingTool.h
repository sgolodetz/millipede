/***
 * millipede: BoxDrawingTool.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_BOXDRAWINGTOOL
#define H_MILLIPEDE_BOXDRAWINGTOOL

#include <boost/optional.hpp>

#include "DrawingTool.h"

namespace mp {

class BoxDrawingTool : public DrawingTool
{
	//#################### PRIVATE VARIABLES ####################
private:
	boost::optional<Vector2i> m_anchor, m_other;

	//#################### PUBLIC METHODS ####################
public:
	bool is_single_pass() const;
	void mouse_dragged(const Vector2i& p);
	void mouse_pressed(const Vector2i& p);
	void render() const;
	void reset();
	std::deque<Vector2i> selected_pixels() const;
};

}

#endif

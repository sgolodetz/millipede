/***
 * millipede: LineLoopDrawingTool.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_LINELOOPDRAWINGTOOL
#define H_MILLIPEDE_LINELOOPDRAWINGTOOL

#include <boost/optional.hpp>

#include "LineBasedDrawingTool.h"

namespace mp {

class LineLoopDrawingTool : public LineBasedDrawingTool
{
	//#################### PRIVATE VARIABLES ####################
private:
	boost::optional<std::list<std::pair<Vector2i,Vector2i> >::iterator> m_grabbedPoint;

	//#################### PUBLIC METHODS ####################
public:
	void mouse_dragged(const Vector2i& p_Pixels, const Vector2i& position);
	void mouse_pressed(const Vector2i& p_Pixels, const Vector2i& position);
	void mouse_released(const Vector2i& p_Pixels, const Vector2i& position);
	void render() const;
	void reset();
	ToolStyle style() const;
};

}

#endif

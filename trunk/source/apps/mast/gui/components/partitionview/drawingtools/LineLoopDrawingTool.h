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
	boost::optional<std::list<std::pair<Vector2i,itk::Index<2> > >::iterator> m_grabbedPoint;

	//#################### PUBLIC METHODS ####################
public:
	void mouse_dragged(const Vector2i& p_Pixels, const itk::Index<2>& position);
	void mouse_pressed(const Vector2i& p_Pixels, const itk::Index<2>& position);
	void mouse_released(const Vector2i& p_Pixels, const itk::Index<2>& position);
	void render() const;
	void reset();
	ToolStyle style() const;
};

}

#endif

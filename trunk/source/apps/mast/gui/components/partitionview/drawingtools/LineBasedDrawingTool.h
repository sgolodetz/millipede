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
	std::list<std::pair<Vector2i,itk::Index<2> > > m_drawnLocations;

	//#################### PUBLIC METHODS ####################
public:
	bool has_started() const;
	void render() const;
	void reset();
	std::vector<itk::Index<2> > selected_positions() const;

	//#################### PRIVATE METHODS ####################
private:
	static std::list<itk::Index<2> > line_positions(const itk::Index<2>& start, const itk::Index<2>& end);
};

}

#endif

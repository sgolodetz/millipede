/***
 * millipede: DrawingTool.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_DRAWINGTOOL
#define H_MILLIPEDE_DRAWINGTOOL

#include <deque>

#include <common/math/Vector2.h>

namespace mp {

class DrawingTool
{
	//#################### DESTRUCTOR ####################
public:
	virtual ~DrawingTool() {}

	//#################### PUBLIC METHODS ####################
public:
	virtual bool is_single_pass() const = 0;
	virtual void mouse_dragged(const Vector2i& p) = 0;
	virtual void mouse_pressed(const Vector2i& p) = 0;
	virtual void render() const = 0;
	virtual void reset() = 0;
	virtual std::deque<Vector2i> selected_pixels() const = 0;
};

}

#endif

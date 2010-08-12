/***
 * millipede: DrawingTool.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_DRAWINGTOOL
#define H_MILLIPEDE_DRAWINGTOOL

#include <vector>

#include <itkIndex.h>

#include <common/math/Vector2.h>

namespace mp {

class DrawingTool
{
	//#################### ENUMERATIONS ####################
public:
	enum ToolStyle
	{
		TOOLSTYLE_CLICKANDDRAG,
		TOOLSTYLE_INSTANTANEOUS,
		TOOLSTYLE_MULTICLICK,
	};

	//#################### DESTRUCTOR ####################
public:
	virtual ~DrawingTool() {}

	//#################### PUBLIC ABSTRACT METHODS ####################
public:
	virtual bool has_started() const = 0;
	virtual void mouse_dragged(const Vector2i& p_Pixels, const itk::Index<2>& position) {}
	virtual void mouse_pressed(const Vector2i& p_Pixels, const itk::Index<2>& position) = 0;
	virtual void mouse_released(const Vector2i& p_Pixels, const itk::Index<2>& position) {}
	virtual void render() const = 0;
	virtual void reset() = 0;
	virtual std::vector<itk::Index<2> > selected_positions() const = 0;
	virtual ToolStyle style() const = 0;
};

}

#endif
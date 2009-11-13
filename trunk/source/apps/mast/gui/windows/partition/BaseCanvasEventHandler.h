/***
 * millipede: BaseCanvasEventHandler.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_BASECANVASEVENTHANDLER
#define H_MILLIPEDE_BASECANVASEVENTHANDLER

#include <mast/gui/base/CanvasEventHandler.h>

namespace mp {

class BaseCanvasEventHandler : public CanvasEventHandler
{
	//#################### PUBLIC METHODS ####################
public:
	void render(wxPaintDC& dc) const;
};

}

#endif

/***
 * millipede: CanvasEventHandler.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_CANVASEVENTHANDLER
#define H_MILLIPEDE_CANVASEVENTHANDLER

#include <wx/dcclient.h>
#include <wx/event.h>

namespace mp {

class CanvasEventHandler : public wxEvtHandler
{
	//#################### PUBLIC ABSTRACT METHODS ####################
public:
	virtual void render(wxPaintDC& dc) const = 0;

	//#################### EVENT HANDLERS ####################
public:
	void OnEraseBackground(wxEraseEvent&);
	void OnPaint(wxPaintEvent& e);

	//#################### EVENT TABLE ####################
	DECLARE_EVENT_TABLE()
};

}

#endif

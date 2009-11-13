/***
 * millipede: CanvasEventHandler.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "CanvasEventHandler.h"

#include <wx/glcanvas.h>

namespace mp {

//#################### EVENT HANDLERS ####################
void CanvasEventHandler::OnEraseBackground(wxEraseEvent&)
{
	// Implementing this is important in order to prevent flickering.
}

void CanvasEventHandler::OnPaint(wxPaintEvent& e)
{
	wxGLCanvas *canvas = static_cast<wxGLCanvas*>(e.GetEventObject());

	wxPaintDC dc(canvas);

	canvas->SetCurrent();
	render(dc);
	glFlush();
	canvas->SwapBuffers();
}

//#################### EVENT TABLE ####################
BEGIN_EVENT_TABLE(CanvasEventHandler, wxEvtHandler)
	EVT_ERASE_BACKGROUND(CanvasEventHandler::OnEraseBackground)
	EVT_PAINT(CanvasEventHandler::OnPaint)
END_EVENT_TABLE()

}

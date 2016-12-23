/***
 * millipede: Canvas.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "Canvas.h"

namespace mp {

//#################### CONSTRUCTORS ####################
Canvas::Canvas(wxWindow *parent, wxGLContext *context, int *attribList, wxWindowID id, const wxPoint& pos, const wxSize& size, long style)
:	wxGLCanvas(parent, context, id, pos, size, style, wxGLCanvasName, attribList)
{}

//#################### EVENT HANDLERS ####################
void Canvas::OnEraseBackground(wxEraseEvent&)
{
	// Implementing this is important in order to prevent flickering.
}

void Canvas::OnPaint(wxPaintEvent& e)
{
	wxPaintDC dc(this);

	SetCurrent();
	render(dc);
	glFlush();
	SwapBuffers();
}

//#################### EVENT TABLE ####################
BEGIN_EVENT_TABLE(Canvas, wxGLCanvas)
	EVT_ERASE_BACKGROUND(Canvas::OnEraseBackground)
	EVT_PAINT(Canvas::OnPaint)
END_EVENT_TABLE()

}

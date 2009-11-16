/***
 * millipede: Canvas.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_CANVAS
#define H_MILLIPEDE_CANVAS

#include <wx/dcclient.h>
#include <wx/glcanvas.h>

namespace mp {

class Canvas : public wxGLCanvas
{
	//#################### CONSTRUCTORS ####################
public:
	Canvas(wxWindow *parent, wxGLContext *context, int *attribList, wxWindowID id = -1, const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = wxFULL_REPAINT_ON_RESIZE|wxWANTS_CHARS);

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

/***
 * millipede: MeshCanvas.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MESHCANVAS
#define H_MILLIPEDE_MESHCANVAS

#include <mast/gui/components/canvas/Canvas.h>

namespace mp {

class MeshCanvas : public Canvas
{
	//#################### CONSTRUCTORS ####################
public:
	MeshCanvas(wxWindow *parent, wxGLContext *context, int *attribList, wxWindowID id = -1, const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = wxFULL_REPAINT_ON_RESIZE|wxWANTS_CHARS);

	//#################### PUBLIC METHODS ####################
public:
	void render(wxPaintDC&) const;
	void setup();
};

}

#endif

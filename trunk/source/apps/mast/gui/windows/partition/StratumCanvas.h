/***
 * millipede: StratumCanvas.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_STRATUMCANVAS
#define H_MILLIPEDE_STRATUMCANVAS

#include "BaseCanvas.h"

namespace mp {

class StratumCanvas : public BaseCanvas
{
	//#################### CONSTRUCTORS ####################
public:
	StratumCanvas(wxWindow *parent, wxGLContext *context, int *attribList, wxWindowID id = -1, const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = wxFULL_REPAINT_ON_RESIZE|wxWANTS_CHARS);

	//#################### PRIVATE METHODS ####################
private:
	SliceTextureSet_CPtr texture_set_to_display() const;
};

}

#endif

/***
 * millipede: StratumCanvas.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "StratumCanvas.h"

namespace mp {

//#################### CONSTRUCTORS ####################
StratumCanvas::StratumCanvas(wxWindow *parent, wxGLContext *context, int *attribList, wxWindowID id, const wxPoint& pos, const wxSize& size, long style)
:	BaseCanvas(parent, context, attribList, id, pos, size, style)
{}

}

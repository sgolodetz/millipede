/***
 * millipede: DICOMCanvas.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_DICOMCANVAS
#define H_MILLIPEDE_DICOMCANVAS

#include "BaseCanvas.h"

namespace mp {

class DICOMCanvas : public BaseCanvas
{
	//#################### CONSTRUCTORS ####################
public:
	DICOMCanvas(wxWindow *parent, wxGLContext *context, int *attribList, wxWindowID id = -1, const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = wxFULL_REPAINT_ON_RESIZE|wxWANTS_CHARS);

	//#################### PRIVATE METHODS ####################
private:
	SliceTextureSet_CPtr texture_set_to_display() const;
};

}

#endif

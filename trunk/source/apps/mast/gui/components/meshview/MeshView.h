/***
 * millipede: MeshView.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MESHVIEW
#define H_MILLIPEDE_MESHVIEW

#include <wx/glcanvas.h>
#include <wx/panel.h>

namespace mp {

class MeshView : public wxPanel
{
	//#################### CONSTRUCTORS ####################
public:
	explicit MeshView(wxWindow *parent, wxGLContext *context = NULL);

	//#################### PRIVATE METHODS ####################
private:
	void setup_gui(wxGLContext *context);
};

}

#endif

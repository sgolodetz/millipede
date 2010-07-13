/***
 * millipede: MeshView.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MESHVIEW
#define H_MILLIPEDE_MESHVIEW

#include <boost/shared_ptr.hpp>

#include <wx/glcanvas.h>
#include <wx/panel.h>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
class MeshCanvas;
typedef boost::shared_ptr<class MeshRenderer> MeshRenderer_Ptr;

class MeshView : public wxPanel
{
	//#################### PRIVATE VARIABLES ####################
private:
	MeshCanvas *m_canvas;
	MeshRenderer_Ptr m_meshRenderer;

	//#################### CONSTRUCTORS ####################
public:
	explicit MeshView(wxWindow *parent, const MeshRenderer_Ptr& meshRenderer, wxGLContext *context = NULL);

	//#################### PRIVATE METHODS ####################
private:
	void setup_gui(wxGLContext *context);
};

}

#endif

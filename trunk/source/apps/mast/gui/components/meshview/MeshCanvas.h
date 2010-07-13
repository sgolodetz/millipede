/***
 * millipede: MeshCanvas.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MESHCANVAS
#define H_MILLIPEDE_MESHCANVAS

#include <boost/shared_ptr.hpp>

#include <mast/gui/components/canvas/Canvas.h>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef boost::shared_ptr<class MeshRenderer> MeshRenderer_Ptr;

class MeshCanvas : public Canvas
{
	//#################### PRIVATE VARIABLES ####################
private:
	MeshRenderer_Ptr m_meshRenderer;

	//#################### CONSTRUCTORS ####################
public:
	MeshCanvas(const MeshRenderer_Ptr& meshRenderer, wxWindow *parent, wxGLContext *context, int *attribList, wxWindowID id = -1, const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = wxFULL_REPAINT_ON_RESIZE|wxWANTS_CHARS);

	//#################### PUBLIC METHODS ####################
public:
	void render(wxPaintDC&) const;
	void setup();
};

}

#endif

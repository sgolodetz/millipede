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
typedef boost::shared_ptr<class MeshCamera> MeshCamera_Ptr;
typedef boost::shared_ptr<class MeshRenderer> MeshRenderer_Ptr;
typedef boost::shared_ptr<class SphereMeshCamera> SphereMeshCamera_Ptr;

class MeshView : public wxPanel
{
	//#################### FRIENDS ####################
	friend class MeshCanvas;

	//#################### PRIVATE VARIABLES ####################
private:
	MeshCamera_Ptr m_camera;
	MeshCanvas *m_canvas;
	MeshRenderer_Ptr m_meshRenderer;
	SphereMeshCamera_Ptr m_sphereCamera;

	//#################### CONSTRUCTORS ####################
public:
	explicit MeshView(wxWindow *parent, const MeshRenderer_Ptr& meshRenderer, wxGLContext *context = NULL);

	//#################### PRIVATE METHODS ####################
private:
	void setup_gui(wxGLContext *context);
};

}

#endif

/***
 * millipede: VisualizationWindow.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VISUALIZATIONWINDOW
#define H_MILLIPEDE_VISUALIZATIONWINDOW

#include <string>

#include <boost/shared_ptr.hpp>

#include <itkVector.h>

#include <common/ogl/WrappedGL.h>

#include <wx/frame.h>
#include <wx/glcanvas.h>

#include <common/math/Vector3.h>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef boost::shared_ptr<class MeshRenderer> MeshRenderer_Ptr;

/**
@brief	A VisualizationWindow is a window in which the user can view a 3D representation
		of the segmentation result.
*/
class VisualizationWindow : public wxFrame
{
	//#################### CONSTRUCTORS ####################
public:
	VisualizationWindow(wxWindow *parent, const std::string& title, const MeshRenderer_Ptr& meshRenderer, const Vector3d& meshScale, wxGLContext *context = NULL);
};

}

#endif

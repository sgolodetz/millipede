/***
 * millipede: MeshView.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MESHVIEW
#define H_MILLIPEDE_MESHVIEW

#include <boost/shared_ptr.hpp>

#include <itkVector.h>

#include <wx/glcanvas.h>
#include <wx/panel.h>

//#################### FORWARD DECLARATIONS ####################
class wxSlider;

namespace mp {

class MeshCanvas;
typedef boost::shared_ptr<class MeshCamera> MeshCamera_Ptr;
typedef boost::shared_ptr<class MeshRenderer> MeshRenderer_Ptr;
typedef boost::shared_ptr<class SphereMeshCamera> SphereMeshCamera_Ptr;

class MeshView : public wxPanel
{
	//#################### FRIENDS ####################
	friend class MeshCanvas;

	//#################### LISTENERS ####################
private:
	struct CameraListener;

	//#################### PRIVATE VARIABLES ####################
private:
	MeshCamera_Ptr m_camera;
	MeshCanvas *m_canvas;
	MeshRenderer_Ptr m_meshRenderer;
	itk::Vector<double,3> m_meshScale;
	SphereMeshCamera_Ptr m_sphereCamera;

	wxSlider *m_azimuthSlider;
	wxSlider *m_centreXSlider;
	wxSlider *m_centreYSlider;
	wxSlider *m_centreZSlider;
	wxSlider *m_distanceSlider;
	wxSlider *m_inclinationSlider;

	//#################### CONSTRUCTORS ####################
public:
	explicit MeshView(wxWindow *parent, const MeshRenderer_Ptr& meshRenderer, const itk::Vector<double,3>& meshScale, wxGLContext *context = NULL);

	//#################### PRIVATE METHODS ####################
private:
	void setup_camera();
	void setup_gui(wxGLContext *context);

	//#################### EVENT HANDLERS ####################
public:
	//~~~~~~~~~~~~~~~~~~~~ CHECKBOXES ~~~~~~~~~~~~~~~~~~~~
	void OnCheckBoxSubmesh(wxCommandEvent& e);
	void OnCheckBoxWireframe(wxCommandEvent& e);

	//~~~~~~~~~~~~~~~~~~~~ SLIDERS ~~~~~~~~~~~~~~~~~~~~
	void OnSliderAzimuth(wxScrollEvent&);
	void OnSliderCentreX(wxScrollEvent&);
	void OnSliderCentreY(wxScrollEvent&);
	void OnSliderCentreZ(wxScrollEvent&);
	void OnSliderDistance(wxScrollEvent&);
	void OnSliderInclination(wxScrollEvent&);

	//#################### EVENT TABLE ####################
	DECLARE_EVENT_TABLE()
};

}

#endif

/***
 * millipede: MeshView.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MESHVIEW
#define H_MILLIPEDE_MESHVIEW

#include <boost/shared_ptr.hpp>

#include <itkVector.h>

#include <common/ogl/WrappedGL.h>

#include <wx/glcanvas.h>
#include <wx/panel.h>

#include <common/math/Vector3.h>

//#################### FORWARD DECLARATIONS ####################
class wxCheckBox;
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
	Vector3d m_meshScale;
	SphereMeshCamera_Ptr m_sphereCamera;

	wxSlider *m_azimuthSlider;
	wxSlider *m_centreXSlider;
	wxSlider *m_centreYSlider;
	wxSlider *m_centreZSlider;
	wxSlider *m_clipSliders[6];
	wxSlider *m_distanceSlider;
	wxSlider *m_inclinationSlider;
	wxCheckBox *m_phongCheckBox;

	//#################### CONSTRUCTORS ####################
public:
	explicit MeshView(wxWindow *parent, const MeshRenderer_Ptr& meshRenderer, const Vector3d& meshScale, wxGLContext *context = NULL);

	//#################### PRIVATE METHODS ####################
private:
	void setup_camera();
	void setup_gui(wxGLContext *context);

	//#################### EVENT HANDLERS ####################
public:
	//~~~~~~~~~~~~~~~~~~~~ CHECKBOXES ~~~~~~~~~~~~~~~~~~~~
	void OnCheckBoxPhong(wxCommandEvent&);
	void OnCheckBoxSubmesh(wxCommandEvent& e);
	void OnCheckBoxWireframe(wxCommandEvent& e);

	//~~~~~~~~~~~~~~~~~~~~ SLIDERS ~~~~~~~~~~~~~~~~~~~~
	void OnSliderAzimuth(wxScrollEvent&);
	void OnSliderCentreX(wxScrollEvent&);
	void OnSliderCentreY(wxScrollEvent&);
	void OnSliderCentreZ(wxScrollEvent&);
	void OnSliderClip(wxScrollEvent&);
	void OnSliderDistance(wxScrollEvent&);
	void OnSliderInclination(wxScrollEvent&);

	//#################### EVENT TABLE ####################
	DECLARE_EVENT_TABLE()
};

}

#endif

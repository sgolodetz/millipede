/***
 * millipede: MeshView.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "MeshView.h"

#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/sizer.h>
#include <wx/slider.h>
#include <wx/stattext.h>

#include <common/visualization/MeshRenderer.h>
#include <mast/util/StringConversion.h>
#include "MeshCanvas.h"
#include "SphereMeshCamera.h"

namespace {

//#################### LOCAL CONSTANTS ####################
enum
{
	ID_BASE = wxID_HIGHEST + 1000,	// a dummy value which is never used: subsequent values are guaranteed to be higher than this
	CHECKBOXID_SUBMESH,
	CHECKBOXID_WIREFRAME,
	SLIDERID_AZIMUTH,
	SLIDERID_CENTRE_X,
	SLIDERID_CENTRE_Y,
	SLIDERID_CENTRE_Z,
	SLIDERID_DISTANCE,
	SLIDERID_INCLINATION,
};

}

namespace mp {

//#################### LISTENERS ####################
struct MeshView::CameraListener : MeshCamera::Listener
{
	MeshView *base;

	explicit CameraListener(MeshView *base_)
	:	base(base_)
	{}

	void camera_changed()
	{
		base->m_canvas->Refresh();
		base->m_azimuthSlider->SetValue(base->m_sphereCamera->azimuth());
		base->m_distanceSlider->SetValue(base->m_sphereCamera->distance());
		base->m_inclinationSlider->SetValue(base->m_sphereCamera->inclination());

		const Vector3i& centre = base->m_sphereCamera->centre();
		base->m_centreXSlider->SetValue(centre.x);
		base->m_centreYSlider->SetValue(centre.y);
		base->m_centreZSlider->SetValue(centre.z);
	}
};

//#################### CONSTRUCTORS ####################
MeshView::MeshView(wxWindow *parent, const MeshRenderer_Ptr& meshRenderer, const itk::Vector<double,3>& meshScale, wxGLContext *context)
:	wxPanel(parent, wxID_ANY, wxDefaultPosition, wxSize(100,100)), m_meshRenderer(meshRenderer), m_meshScale(meshScale)
{
	setup_camera();
	setup_gui(context);
	m_canvas->setup();

	m_camera->add_shared_listener(boost::shared_ptr<CameraListener>(new CameraListener(this)));
}

//#################### PRIVATE METHODS ####################
void MeshView::setup_camera()
{
	Vector3i centre, minCentre, maxCentre;
	int distance, azimuth, inclination, minDistance, maxDistance;

	if(!m_meshRenderer->mesh_is_empty())
	{
		Vector3d lowerBound = m_meshRenderer->mesh_lower_bound(), upperBound = m_meshRenderer->mesh_upper_bound();
		centre = Vector3i((lowerBound + upperBound) / 2);
		distance = (NumericUtil::round_to_nearest<int>(upperBound.y) - centre.y) * 2;
		minCentre = Vector3i(lowerBound);
		maxCentre = Vector3i(upperBound);
	}
	else
	{
		centre = Vector3i(0,0,0);
		distance = 30;
		minCentre = Vector3i(-10,-10,-10);
		maxCentre = Vector3i(10,10,10);
	}

	azimuth = 0;
	inclination = 0;
	minDistance = 5;
	maxDistance = 512;

	m_sphereCamera.reset(new SphereMeshCamera(centre, distance, azimuth, inclination, minCentre, maxCentre, minDistance, maxDistance));
	m_camera = m_sphereCamera;
}

void MeshView::setup_gui(wxGLContext *context)
{
	const int BORDER_SIZE = 15;

	SetBackgroundColour(wxColour(240,240,240));

	wxFlexGridSizer *sizer = new wxFlexGridSizer(3, 3, 5, 5);
	SetSizer(sizer);

	int attribList[] =
	{
		WX_GL_RGBA,
		WX_GL_DEPTH_SIZE,
		16,
		WX_GL_DOUBLEBUFFER,
		0
	};

	// Top left
	sizer->Add(new wxPanel(this));

	// Top middle
	wxCheckBox *wireframeCheckBox = new wxCheckBox(this, CHECKBOXID_WIREFRAME, wxT("Enable Wireframe"));
	sizer->Add(wireframeCheckBox, 0, wxALIGN_CENTRE_HORIZONTAL|wxTOP, BORDER_SIZE);

	// Top right
	sizer->Add(new wxPanel(this));

	// Middle left
	wxPanel *submeshControls = new wxPanel(this);
	wxGridSizer *submeshControlsSizer = new wxGridSizer(0, 1, 5, 0);
	submeshControls->SetSizer(submeshControlsSizer);
		std::vector<std::string> submeshNames = m_meshRenderer->submesh_names();
		for(std::vector<std::string>::const_iterator it=submeshNames.begin(), iend=submeshNames.end(); it!=iend; ++it)
		{
			if(m_meshRenderer->has_submesh(*it))
			{
				wxCheckBox *submeshControl = new wxCheckBox(submeshControls, CHECKBOXID_SUBMESH, string_to_wxString(*it));
				submeshControl->SetValue(m_meshRenderer->submesh_enabled(*it));
				submeshControlsSizer->Add(submeshControl);
			}
		}
		Connect(CHECKBOXID_SUBMESH, wxEVT_COMMAND_CHECKBOX_CLICKED, wxCommandEventHandler(MeshView::OnCheckBoxSubmesh));
	sizer->Add(submeshControls, 0, wxALIGN_CENTRE|wxLEFT, BORDER_SIZE);

	// Middle
	m_canvas = new MeshCanvas(this, context, attribList, wxID_ANY, wxDefaultPosition, wxSize(512,512));
	sizer->Add(m_canvas, 0, wxALL, BORDER_SIZE);

	// Middle right
	// TODO: Get the slider ranges right.
	wxPanel *clippingControls = new wxPanel(this);
	wxFlexGridSizer *clippingControlsSizer = new wxFlexGridSizer(0, 2, 0, 0);
	clippingControls->SetSizer(clippingControlsSizer);
		clippingControlsSizer->Add(new wxStaticText(clippingControls, wxID_ANY, wxT("X Clip Lower:")), 0, wxALIGN_CENTRE_VERTICAL);
		clippingControlsSizer->Add(new wxSlider(clippingControls, wxID_ANY, 0, 0, 100, wxDefaultPosition, wxSize(100,50), wxHORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP));

		clippingControlsSizer->Add(new wxStaticText(clippingControls, wxID_ANY, wxT("X Clip Upper:")), 0, wxALIGN_CENTRE_VERTICAL);
		clippingControlsSizer->Add(new wxSlider(clippingControls, wxID_ANY, 0, 0, 100, wxDefaultPosition, wxSize(100,50), wxHORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP));

		clippingControlsSizer->Add(new wxStaticText(clippingControls, wxID_ANY, wxT("Y Clip Lower:")), 0, wxALIGN_CENTRE_VERTICAL);
		clippingControlsSizer->Add(new wxSlider(clippingControls, wxID_ANY, 0, 0, 100, wxDefaultPosition, wxSize(100,50), wxHORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP));

		clippingControlsSizer->Add(new wxStaticText(clippingControls, wxID_ANY, wxT("Y Clip Upper:")), 0, wxALIGN_CENTRE_VERTICAL);
		clippingControlsSizer->Add(new wxSlider(clippingControls, wxID_ANY, 0, 0, 100, wxDefaultPosition, wxSize(100,50), wxHORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP));

		clippingControlsSizer->Add(new wxStaticText(clippingControls, wxID_ANY, wxT("Z Clip Lower:")), 0, wxALIGN_CENTRE_VERTICAL);
		clippingControlsSizer->Add(new wxSlider(clippingControls, wxID_ANY, 0, 0, 100, wxDefaultPosition, wxSize(100,50), wxHORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP));

		clippingControlsSizer->Add(new wxStaticText(clippingControls, wxID_ANY, wxT("Z Clip Upper:")), 0, wxALIGN_CENTRE_VERTICAL);
		clippingControlsSizer->Add(new wxSlider(clippingControls, wxID_ANY, 0, 0, 100, wxDefaultPosition, wxSize(100,50), wxHORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP));
	sizer->Add(clippingControls, 0, wxALIGN_CENTRE|wxRIGHT, BORDER_SIZE);

	// Bottom left
	sizer->Add(new wxPanel(this));

	// Bottom middle
	wxPanel *cameraControls = new wxPanel(this);
	wxBoxSizer *cameraControlsSizer = new wxBoxSizer(wxHORIZONTAL);
	cameraControls->SetSizer(cameraControlsSizer);
		wxPanel *centreControls = new wxPanel(cameraControls);
		wxFlexGridSizer *centreControlsSizer = new wxFlexGridSizer(0, 2, 0, 0);
		centreControls->SetSizer(centreControlsSizer);
			centreControlsSizer->Add(new wxStaticText(centreControls, wxID_ANY, wxT("Centre X:")), 0, wxALIGN_CENTRE_VERTICAL);
			m_centreXSlider = new wxSlider(centreControls, SLIDERID_CENTRE_X, m_sphereCamera->centre().x, m_sphereCamera->min_centre().x, m_sphereCamera->max_centre().x, wxDefaultPosition, wxSize(100,50), wxHORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
			centreControlsSizer->Add(m_centreXSlider);

			centreControlsSizer->Add(new wxStaticText(centreControls, wxID_ANY, wxT("Centre Y:")), 0, wxALIGN_CENTRE_VERTICAL);
			m_centreYSlider = new wxSlider(centreControls, SLIDERID_CENTRE_Y, m_sphereCamera->centre().y, m_sphereCamera->min_centre().y, m_sphereCamera->max_centre().y, wxDefaultPosition, wxSize(100,50), wxHORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
			centreControlsSizer->Add(m_centreYSlider);

			centreControlsSizer->Add(new wxStaticText(centreControls, wxID_ANY, wxT("Centre Z:")), 0, wxALIGN_CENTRE_VERTICAL);
			m_centreZSlider = new wxSlider(centreControls, SLIDERID_CENTRE_Z, m_sphereCamera->centre().z, m_sphereCamera->min_centre().z, m_sphereCamera->max_centre().z, wxDefaultPosition, wxSize(100,50), wxHORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
			centreControlsSizer->Add(m_centreZSlider);
		cameraControlsSizer->Add(centreControls);

		cameraControlsSizer->AddSpacer(30);

		wxPanel *eyeControls = new wxPanel(cameraControls);
		wxFlexGridSizer *eyeControlsSizer = new wxFlexGridSizer(0, 2, 0, 0);
		eyeControls->SetSizer(eyeControlsSizer);
			eyeControlsSizer->Add(new wxStaticText(eyeControls, wxID_ANY, wxT("Distance:")), 0, wxALIGN_CENTRE_VERTICAL);
			m_distanceSlider = new wxSlider(eyeControls, SLIDERID_DISTANCE, m_sphereCamera->distance(), m_sphereCamera->min_distance(), m_sphereCamera->max_distance(), wxDefaultPosition, wxSize(100,50), wxHORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
			eyeControlsSizer->Add(m_distanceSlider);

			eyeControlsSizer->Add(new wxStaticText(eyeControls, wxID_ANY, wxT("Azimuth:")), 0, wxALIGN_CENTRE_VERTICAL);
			m_azimuthSlider = new wxSlider(eyeControls, SLIDERID_AZIMUTH, m_sphereCamera->azimuth(), -179, 180, wxDefaultPosition, wxSize(100,50), wxHORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
			eyeControlsSizer->Add(m_azimuthSlider);

			eyeControlsSizer->Add(new wxStaticText(eyeControls, wxID_ANY, wxT("Inclination:")), 0, wxALIGN_CENTRE_VERTICAL);
			m_inclinationSlider = new wxSlider(eyeControls, SLIDERID_INCLINATION, m_sphereCamera->inclination(), -89, 89, wxDefaultPosition, wxSize(100,50), wxHORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
			eyeControlsSizer->Add(m_inclinationSlider);
		cameraControlsSizer->Add(eyeControls);
	sizer->Add(cameraControls, 0, wxALIGN_CENTRE_HORIZONTAL|wxBOTTOM, BORDER_SIZE);

	sizer->Fit(this);
}

//#################### EVENT HANDLERS ####################

//~~~~~~~~~~~~~~~~~~~~ CHECKBOXES ~~~~~~~~~~~~~~~~~~~~
void MeshView::OnCheckBoxSubmesh(wxCommandEvent& e)
{
	wxCheckBox *submeshControl = static_cast<wxCheckBox*>(e.GetEventObject());
	m_meshRenderer->set_submesh_enabled(wxString_to_string(submeshControl->GetLabel()), e.IsChecked());
	m_canvas->Refresh();
}

void MeshView::OnCheckBoxWireframe(wxCommandEvent& e)
{
	m_meshRenderer->set_wireframe_enabled(e.IsChecked());
	m_canvas->Refresh();
}

//~~~~~~~~~~~~~~~~~~~~ SLIDERS ~~~~~~~~~~~~~~~~~~~~
void MeshView::OnSliderAzimuth(wxScrollEvent&)
{
	m_sphereCamera->set_azimuth(m_azimuthSlider->GetValue());
}

void MeshView::OnSliderCentreX(wxScrollEvent&)
{
	Vector3i centre = m_sphereCamera->centre();
	centre.x = m_centreXSlider->GetValue();
	m_sphereCamera->set_centre(centre);
}

void MeshView::OnSliderCentreY(wxScrollEvent&)
{
	Vector3i centre = m_sphereCamera->centre();
	centre.y = m_centreYSlider->GetValue();
	m_sphereCamera->set_centre(centre);
}

void MeshView::OnSliderCentreZ(wxScrollEvent&)
{
	Vector3i centre = m_sphereCamera->centre();
	centre.z = m_centreZSlider->GetValue();
	m_sphereCamera->set_centre(centre);
}

void MeshView::OnSliderDistance(wxScrollEvent&)
{
	m_sphereCamera->set_distance(m_distanceSlider->GetValue());
}

void MeshView::OnSliderInclination(wxScrollEvent&)
{
	m_sphereCamera->set_inclination(m_inclinationSlider->GetValue());
}

//#################### EVENT TABLE ####################
BEGIN_EVENT_TABLE(MeshView, wxPanel)
	//~~~~~~~~~~~~~~~~~~~~ CHECKBOXES ~~~~~~~~~~~~~~~~~~~~
	EVT_CHECKBOX(CHECKBOXID_WIREFRAME, MeshView::OnCheckBoxWireframe)

	//~~~~~~~~~~~~~~~~~~~~ SLIDERS ~~~~~~~~~~~~~~~~~~~~
	EVT_COMMAND_SCROLL(SLIDERID_AZIMUTH, MeshView::OnSliderAzimuth)
	EVT_COMMAND_SCROLL(SLIDERID_CENTRE_X, MeshView::OnSliderCentreX)
	EVT_COMMAND_SCROLL(SLIDERID_CENTRE_Y, MeshView::OnSliderCentreY)
	EVT_COMMAND_SCROLL(SLIDERID_CENTRE_Z, MeshView::OnSliderCentreZ)
	EVT_COMMAND_SCROLL(SLIDERID_DISTANCE, MeshView::OnSliderDistance)
	EVT_COMMAND_SCROLL(SLIDERID_INCLINATION, MeshView::OnSliderInclination)
END_EVENT_TABLE()

}

/***
 * millipede: PartitionWindow_GUI.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "PartitionWindow.h"

#include <wx/button.h>
#include <wx/sizer.h>
#include <wx/stattext.h>

#include <common/dicom/volumes/VolumeTextureSet.h>
#include "PartitionCanvas.h"
#include "StratumCanvas.h"
#include "ViewedVolumeModel.h"

namespace {

//#################### LOCAL CONSTANTS ####################
enum
{
	ID_BASE = wxID_HIGHEST,		// a dummy value which is never used: subsequent values are guaranteed to be higher than this
	BUTTONID_CREATE_TEXTURES,
};

}

namespace mp {

//#################### PRIVATE METHODS ####################
void PartitionWindow::setup_gui(wxGLContext *context)
{
	SetBackgroundColour(wxColour(240,240,240));

	wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
	SetSizer(sizer);

	//~~~~~~~~~~~~~~~~~~~~~~~~
	// Construct the top panel
	//~~~~~~~~~~~~~~~~~~~~~~~~

	wxPanel *top = new wxPanel(this);
	wxBoxSizer *topSizer = new wxBoxSizer(wxHORIZONTAL);
	top->SetSizer(topSizer);
	sizer->Add(top, 0, wxALIGN_CENTER_HORIZONTAL);

	int attribList[] =
	{
		WX_GL_RGBA,
		WX_GL_DEPTH_SIZE,
		16,
		WX_GL_DOUBLEBUFFER
	};

	// Top left
	wxPanel *topLeft = new wxPanel(top);
	wxBoxSizer *topLeftSizer = new wxBoxSizer(wxVERTICAL);
	topLeft->SetSizer(topLeftSizer);
		wxButton *createTexturesButton = new wxButton(topLeft, BUTTONID_CREATE_TEXTURES, wxT("Create Textures"));
		topLeftSizer->Add(createTexturesButton, 0, wxALIGN_CENTER_HORIZONTAL);

		m_stratumCanvas = new StratumCanvas(topLeft, context, attribList, wxID_ANY, wxDefaultPosition, wxSize(m_canvasWidth, m_canvasHeight));
		topLeftSizer->Add(m_stratumCanvas);
	topSizer->Add(topLeft);

	// Space between the panels
	topSizer->AddSpacer(10);

	// Top right
	wxPanel *topRight = new wxPanel(top);
	wxBoxSizer *topRightSizer = new wxBoxSizer(wxVERTICAL);
	topRight->SetSizer(topRightSizer);
		wxButton *segmentVolumeButton = new wxButton(topRight, wxID_ANY, wxT("Segment Volume"));
		topRightSizer->Add(segmentVolumeButton, 0, wxALIGN_CENTER_HORIZONTAL);

		m_partitionCanvas = new PartitionCanvas(topRight, get_context(), attribList, wxID_ANY, wxDefaultPosition, wxSize(m_canvasWidth, m_canvasHeight));
		topRightSizer->Add(m_partitionCanvas);
	topSizer->Add(topRight);

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// Construct the middle panel
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~

	wxPanel *middle = new wxPanel(this);
	wxBoxSizer *middleSizer = new wxBoxSizer(wxHORIZONTAL);
	middle->SetSizer(middleSizer);
	sizer->Add(middle, 0, wxALIGN_CENTER_HORIZONTAL);

	// Middle left
	wxPanel *middleLeft = new wxPanel(middle);
	wxFlexGridSizer *middleLeftSizer = new wxFlexGridSizer(2, 3);
	middleLeft->SetSizer(middleLeftSizer);
		wxStaticText *xText = new wxStaticText(middleLeft, wxID_ANY, wxT("X: "));
		middleLeftSizer->Add(xText, 0, wxALIGN_CENTER_VERTICAL);
		m_xSlider = new wxSlider(middleLeft, wxID_ANY, 999, 999, 999, wxDefaultPosition, wxDefaultSize, wxHORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
		middleLeftSizer->Add(m_xSlider, 0, wxALIGN_CENTER);

		wxStaticText *yText = new wxStaticText(middleLeft, wxID_ANY, wxT("Y: "));
		middleLeftSizer->Add(yText, 0, wxALIGN_CENTER_VERTICAL);
		m_ySlider = new wxSlider(middleLeft, wxID_ANY, 999, 999, 999, wxDefaultPosition, wxDefaultSize, wxHORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
		middleLeftSizer->Add(m_ySlider, 0, wxALIGN_CENTER);

		wxStaticText *zText = new wxStaticText(middleLeft, wxID_ANY, wxT("Z: "));
		middleLeftSizer->Add(zText, 0, wxALIGN_CENTER_VERTICAL);
		m_zSlider = new wxSlider(middleLeft, wxID_ANY, 999, 999, 999, wxDefaultPosition, wxDefaultSize, wxHORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
		middleLeftSizer->Add(m_zSlider, 0, wxALIGN_CENTER);
	middleSizer->Add(middleLeft, 0, wxALIGN_CENTER_VERTICAL);

	// Space between the panels
	middleSizer->AddSpacer(20);

	// Middle right
	wxPanel *middleRight = new wxPanel(middle);
	wxBoxSizer *middleRightSizer = new wxBoxSizer(wxVERTICAL);
	middleRight->SetSizer(middleRightSizer);
		wxButton *viewXYButton = new wxButton(middleRight, wxID_ANY, wxT("View X-Y (usually Axial)"));
		middleRightSizer->Add(viewXYButton);

		wxButton *viewXZButton = new wxButton(middleRight, wxID_ANY, wxT("View X-Z (usually Coronal)"));
		middleRightSizer->Add(viewXZButton);

		wxButton *viewYZButton = new wxButton(middleRight, wxID_ANY, wxT("View Y-Z (usually Sagittal)"));
		middleRightSizer->Add(viewYZButton);
	middleSizer->Add(middleRight, 0, wxALIGN_CENTER_VERTICAL);

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// Construct the bottom panel
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~

	// TODO

	sizer->Fit(this);
}

//#################### EVENT HANDLERS ####################
void PartitionWindow::OnButtonCreateTextures(wxCommandEvent&)
{
	m_model->m_textureSet.reset(new VolumeTextureSet(m_model->m_volume, m_volumeChoice.windowSettings));
}

//#################### EVENT TABLE ####################
BEGIN_EVENT_TABLE(PartitionWindow, wxFrame)
	//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
	EVT_BUTTON(BUTTONID_CREATE_TEXTURES, PartitionWindow::OnButtonCreateTextures)
END_EVENT_TABLE()

}

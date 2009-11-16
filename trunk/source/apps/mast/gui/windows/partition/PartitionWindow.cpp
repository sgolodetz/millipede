/***
 * millipede: PartitionWindow.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "PartitionWindow.h"

#include <wx/sizer.h>
#include <wx/stattext.h>

#include <common/dicom/volumes/Volume.h>
#include <mast/util/StringConversion.h>
#include "PartitionCanvas.h"
#include "StratumCanvas.h"

namespace mp {

//#################### CONSTRUCTORS ####################
PartitionWindow::PartitionWindow(wxWindow *parent, const std::string& title, const Volume_Ptr& volume, wxGLContext *context)
:	wxFrame(parent, -1, string_to_wxString(title), wxDefaultPosition, wxSize(100,100)),
	m_oldViewLocation(-1, -1), m_viewLocation(new ViewLocation(-1, -1)), m_volume(volume)
{
	calculate_canvas_size();

	Show();

	wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
	SetSizer(sizer);

	//~~~~~~~~~~~~~~~~~~~~~~~
	// Construct the top half
	//~~~~~~~~~~~~~~~~~~~~~~~

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
	m_stratumCanvas = new StratumCanvas(top, context, attribList, wxID_ANY, wxDefaultPosition, wxSize(m_canvasWidth, m_canvasHeight));
	topSizer->Add(m_stratumCanvas);

	// Top middle
	wxPanel *middle = new wxPanel(top);
	wxBoxSizer *middleSizer = new wxBoxSizer(wxVERTICAL);
	middle->SetSizer(middleSizer);
		wxStaticText *stratumText = new wxStaticText(middle, wxID_ANY, wxT("Stratum"));
		middleSizer->Add(stratumText, 0, wxALIGN_CENTER_HORIZONTAL);
		m_stratumSlider = new wxSlider(middle, wxID_ANY, 999, 999, 999, wxDefaultPosition, wxDefaultSize, wxVERTICAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_LEFT);
		middleSizer->Add(m_stratumSlider, 0, wxALIGN_CENTER_HORIZONTAL);
	topSizer->Add(middle, 0, wxALIGN_CENTER_VERTICAL);

	// Top right
	wxPanel *right = new wxPanel(top);
	wxBoxSizer *rightSizer = new wxBoxSizer(wxVERTICAL);
	right->SetSizer(rightSizer);
		m_partitionCanvas = new PartitionCanvas(right, get_context(), attribList, wxID_ANY, wxDefaultPosition, wxSize(m_canvasWidth, m_canvasHeight));
		rightSizer->Add(m_partitionCanvas);

		wxPanel *bottom = new wxPanel(right);
		wxBoxSizer *bottomSizer = new wxBoxSizer(wxVERTICAL);
		bottom->SetSizer(bottomSizer);
			wxStaticText *layerText = new wxStaticText(bottom, wxID_ANY, wxT("Layer"));
			bottomSizer->Add(layerText, 0, wxALIGN_CENTER_HORIZONTAL);
			m_layerSlider = new wxSlider(bottom, wxID_ANY, 999, 999, 999, wxDefaultPosition, wxDefaultSize, wxHORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
			bottomSizer->Add(m_layerSlider, 0, wxALIGN_CENTER_HORIZONTAL);
		rightSizer->Add(bottom, 0, wxALIGN_CENTER_HORIZONTAL);
	topSizer->Add(right);

	//~~~~~~~~~~~~~~~~~~~~~~~~~~
	// Construct the bottom half
	//~~~~~~~~~~~~~~~~~~~~~~~~~~

	// TODO

	sizer->Fit(this);

	setup_canvas(m_stratumCanvas);
	setup_canvas(m_partitionCanvas);
}

//#################### PUBLIC METHODS ####################
wxGLContext *PartitionWindow::get_context() const
{
	return m_stratumCanvas->GetContext();
}

//#################### PRIVATE METHODS ####################
void PartitionWindow::calculate_canvas_size()
{
	// We want our canvases to be at least 512x512, but beyond that their size should be dictated by the sizes
	// of the images. We want to be able to show the images in axial (X-Y), sagittal (Y-Z) and coronal (X-Z)
	// orientations, which dictates which dimensions we need to take into account for the canvas sizes.

	Volume::Size volumeSize = m_volume->size();
	m_canvasWidth = std::max<int>(512, std::max(volumeSize[0], volumeSize[1]));
	m_canvasHeight = std::max<int>(512, std::max(volumeSize[1], volumeSize[2]));
}

void PartitionWindow::setup_canvas(wxGLCanvas *canvas)
{
	canvas->SetCurrent();

	// Enable back-face culling.
	glCullFace(GL_BACK);
	glFrontFace(GL_CW);
	glEnable(GL_CULL_FACE);

	// Set up the z-buffer.
	glDepthFunc(GL_LEQUAL);
	glEnable(GL_DEPTH_TEST);

	// Set up alpha testing.
	glAlphaFunc(GL_NOTEQUAL, 0);
	glEnable(GL_ALPHA_TEST);

	glClearColor(0, 0, 0, 0);

	glViewport(0, 0, m_canvasWidth, m_canvasHeight);

	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();

	glOrtho(0, m_canvasWidth, m_canvasHeight, 0, 0.0, 2048.0);
}

}

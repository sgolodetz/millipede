/***
 * millipede: PartitionWindow.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "PartitionWindow.h"

#include <wx/sizer.h>
#include <wx/stattext.h>

#include <mast/util/StringConversion.h>

namespace mp {

//#################### CONSTRUCTORS ####################
PartitionWindow::PartitionWindow(wxWindow *parent, const std::string& title, const Volume_Ptr& volume)
:	wxFrame(parent, -1, string_to_wxString(title), wxDefaultPosition, wxSize(100,100)),
	m_oldViewLocation(-1, -1), m_viewLocation(-1, -1), m_volume(volume)
{
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
	m_stratumCanvas = new wxGLCanvas(top, wxID_ANY, attribList, wxDefaultPosition, wxSize(512, 512), wxFULL_REPAINT_ON_RESIZE|wxWANTS_CHARS);
	// TODO: Event handler
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
		m_partitionCanvas = new wxGLCanvas(right, wxID_ANY, attribList, wxDefaultPosition, wxSize(512, 512), wxFULL_REPAINT_ON_RESIZE|wxWANTS_CHARS);
		// TODO: Event handler
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

	// Set the OpenGL contexts for the canvases.
	wxGLContext *context = new wxGLContext(m_stratumCanvas);
	m_stratumCanvas->SetCurrent(*context);
	m_partitionCanvas->SetCurrent(*context);

	//~~~~~~~~~~~~~~~~~~~~~~~~~~
	// Construct the bottom half
	//~~~~~~~~~~~~~~~~~~~~~~~~~~

	// TODO

	sizer->Fit(this);
}

}

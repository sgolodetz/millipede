/***
 * millipede: VisualizationWindow.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "VisualizationWindow.h"

#include <wx/sizer.h>

#include <mast/gui/components/meshview/MeshView.h>
#include <mast/util/StringConversion.h>

namespace mp {

//#################### CONSTRUCTORS ####################
VisualizationWindow::VisualizationWindow(wxWindow *parent, const std::string& title, wxGLContext *context)
:	wxFrame(parent, wxID_ANY, string_to_wxString(title))
{
	SetBackgroundColour(wxColour(240,240,240));

	wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
	SetSizer(sizer);

	Show();

	MeshView *view = new MeshView(this, context);
	sizer->Add(view, 0, wxALIGN_CENTRE_HORIZONTAL);

	sizer->Fit(this);
	CenterOnScreen();
}

}

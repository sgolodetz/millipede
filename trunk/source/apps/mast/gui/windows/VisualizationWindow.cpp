/***
 * millipede: VisualizationWindow.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "VisualizationWindow.h"

#include <mast/util/StringConversion.h>

namespace mp {

//#################### CONSTRUCTORS ####################
VisualizationWindow::VisualizationWindow(wxWindow *parent, const std::string& title)
:	wxFrame(parent, wxID_ANY, string_to_wxString(title))
{}

}

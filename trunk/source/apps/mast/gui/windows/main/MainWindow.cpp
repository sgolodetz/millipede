/***
 * mast: MainWindow.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "MainWindow.h"

#include <mast/util/StringConversion.h>

namespace mp {

//#################### CONSTRUCTORS ####################
MainWindow::MainWindow(const std::string& title)
:	wxFrame(NULL, wxID_ANY, string_to_wxString(title), wxDefaultPosition, wxSize(600,400))
{
	setup_menus();
}

}

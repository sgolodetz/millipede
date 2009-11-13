/***
 * millipede: PartitionWindow.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "PartitionWindow.h"

#include <mast/util/StringConversion.h>

namespace mp {

//#################### CONSTRUCTORS ####################
PartitionWindow::PartitionWindow(const std::string& title, const Volume_Ptr& volume)
:	wxFrame(NULL, -1, string_to_wxString(title), wxDefaultPosition, wxSize(100,100))
{
	// TODO
}

}

/***
 * mast: MainWindow.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "MainWindow.h"

#include <wx/progdlg.h>

#include <common/io/files/VolumeLoader.h>
#include <mast/util/StringConversion.h>

namespace mp {

//#################### CONSTRUCTORS ####################
MainWindow::MainWindow(const std::string& title)
:	wxFrame(NULL, wxID_ANY, string_to_wxString(title), wxDefaultPosition, wxDefaultSize, wxDEFAULT_FRAME_STYLE & ~(wxMAXIMIZE_BOX | wxRESIZE_BORDER))
{
	setup_menus();
}

//#################### PUBLIC METHODS ####################
void MainWindow::setup()
{
	// NYI
}

//#################### PRIVATE METHODS ####################
void MainWindow::show_progress_dialog(const VolumeLoader_Ptr& loader)
{
	wxProgressDialog dialog(wxT("Loading Volume"), wxEmptyString, loader->max(), NULL, wxPD_CAN_ABORT|wxPD_ELAPSED_TIME|wxPD_REMAINING_TIME|wxPD_SMOOTH);
	dialog.SetSize(500, 200);

	while(loader->progress() < loader->max())
	{
		if(!dialog.Update(loader->progress(), string_to_wxString(loader->status())))
		{
			loader->abort();
			break;
		}
	}
}

void MainWindow::volume_loader_thread(const VolumeLoader_Ptr& loader)
{
	loader->load();
	while(loader->progress() != loader->max() && !loader->aborted());

	if(!loader->aborted())
	{
		// TODO: Create a window with the new volume
	}
}

}

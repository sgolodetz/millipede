/***
 * mast: MainWindow.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "MainWindow.h"

#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/sizer.h>

#include <common/io/files/VolumeLoader.h>
#include <mast/gui/windows/partition/PartitionWindow.h>
#include <mast/util/StringConversion.h>

namespace mp {

//#################### CONSTRUCTORS ####################
MainWindow::MainWindow(const std::string& title)
:	wxFrame(NULL, wxID_ANY, string_to_wxString(title), wxDefaultPosition, wxSize(600,400))
{
	setup_menus();
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
			// The user clicked Cancel.
			loader->abort();
			break;
		}

		if(loader->aborted())
		{
			// The loading process failed (i.e. the loader aborted itself).
			wxMessageBox(string_to_wxString(loader->status()), wxT("Error"), wxOK|wxICON_ERROR|wxCENTRE, this);
			break;
		}
	}

	if(!loader->aborted())
	{
		// Create a window for the user to interact with the new volume.
		PartitionWindow *partitionWindow = new PartitionWindow(this, "Untitled", loader->volume(), loader->volume_choice());
		partitionWindow->Show(true);
	}
}

void MainWindow::volume_loader_thread(const VolumeLoader_Ptr& loader)
{
	loader->load();
	while(loader->progress() != loader->max() && !loader->aborted());
}

}

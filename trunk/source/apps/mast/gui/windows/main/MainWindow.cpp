/***
 * mast: MainWindow.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "MainWindow.h"

#include <wx/msgdlg.h>
#include <wx/progdlg.h>

#include <common/jobs/Job.h>
#include <mast/util/StringConversion.h>

namespace mp {

//#################### CONSTRUCTORS ####################
MainWindow::MainWindow(const std::string& title)
:	wxFrame(NULL, wxID_ANY, string_to_wxString(title), wxDefaultPosition, wxSize(600,400))
{
	setup_menus();
}

//#################### PRIVATE METHODS ####################
void MainWindow::show_progress_dialog(const Job_Ptr& job, const std::string& caption)
{
	wxProgressDialog dialog(string_to_wxString(caption), wxEmptyString, job->length(), NULL, wxPD_CAN_ABORT|wxPD_ELAPSED_TIME|wxPD_REMAINING_TIME|wxPD_SMOOTH);
	dialog.SetSize(500, 200);

	while(!job->is_finished())
	{
		if(!dialog.Update(job->progress(), string_to_wxString(job->status())))
		{
			// The user clicked Cancel.
			job->abort();
			break;
		}

		if(job->is_aborted())
		{
			// The loading process failed (i.e. the job aborted itself).
			wxMessageBox(string_to_wxString(job->status()), wxT("Error"), wxOK|wxICON_ERROR|wxCENTRE, this);
			break;
		}
	}
}

}

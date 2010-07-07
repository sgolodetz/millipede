/***
 * millipede: DialogUtil.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "DialogUtil.h"

#include <fstream>

#include <wx/msgdlg.h>
#include <wx/progdlg.h>

#include <common/exceptions/FileNotFoundException.h>
#include <common/jobs/Job.h>
#include <common/jobs/MainThreadJobQueue.h>
#include <mast/util/StringConversion.h>

namespace mp {

//#################### FUNCTIONS ####################
void check_file_exists(const std::string& path)
{
	std::ifstream fs(path.c_str());
	if(fs.fail()) throw FileNotFoundException(path);
}

wxFileDialog_Ptr construct_open_dialog(wxWindow *parent, const std::string& caption, const std::string& wildcard,
									   const std::string& defaultDir, const std::string& defaultFilename)
{
	return wxFileDialog_Ptr(new wxFileDialog(parent, string_to_wxString(caption), string_to_wxString(defaultDir),
							string_to_wxString(defaultFilename), string_to_wxString(wildcard),
							wxOPEN));
}

wxFileDialog_Ptr construct_save_dialog(wxWindow *parent, const std::string& caption, const std::string& wildcard,
									   const std::string& defaultDir, const std::string& defaultFilename)
{
	return wxFileDialog_Ptr(new wxFileDialog(parent, string_to_wxString(caption), string_to_wxString(defaultDir),
							string_to_wxString(defaultFilename), string_to_wxString(wildcard),
							wxSAVE | wxOVERWRITE_PROMPT));
}

bool execute_with_progress_dialog(const Job_Ptr& job, wxWindow *parent, const std::string& caption, bool canAbort)
{
	Job::execute_in_thread(job);

	int style = wxPD_AUTO_HIDE|wxPD_ELAPSED_TIME|wxPD_REMAINING_TIME|wxPD_SMOOTH;
	if(canAbort) style |= wxPD_CAN_ABORT;
	wxProgressDialog dialog(string_to_wxString(caption), wxEmptyString, job->length(), parent, style);
	dialog.SetSize(600, 200);
	dialog.CenterOnParent();

	MainThreadJobQueue_Ptr mtjq = job->main_thread_job_queue();

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
			// The job failed (i.e. the job aborted itself).
			wxMessageBox(string_to_wxString(job->status()), wxT("Error"), wxOK|wxICON_ERROR|wxCENTRE, parent);
			break;
		}

		// If any sub-jobs have been queued to run in the main thread, run the next one in the queue.
		if(mtjq->has_jobs())
		{
			mtjq->run_next_job();
		}
	}

	return job->is_finished();
}

}

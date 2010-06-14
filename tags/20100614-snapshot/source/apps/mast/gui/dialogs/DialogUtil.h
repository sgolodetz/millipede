/***
 * millipede: DialogUtil.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_DIALOGUTIL
#define H_MILLIPEDE_DIALOGUTIL

#include <string>

#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

#include <wx/filedlg.h>
#include <wx/window.h>

namespace mp {

//#################### TYPEDEFS ####################
typedef shared_ptr<class Job> Job_Ptr;
typedef shared_ptr<wxFileDialog> wxFileDialog_Ptr;

//#################### FUNCTIONS ####################
void check_file_exists(const std::string& path);
wxFileDialog_Ptr construct_open_dialog(wxWindow *parent, const std::string& caption, const std::string& wildcard,
									   const std::string& defaultDir = ".", const std::string& defaultFilename = "");
wxFileDialog_Ptr construct_save_dialog(wxWindow *parent, const std::string& caption, const std::string& wildcard,
									   const std::string& defaultDir = ".", const std::string& defaultFilename = "");
bool show_progress_dialog(wxWindow *parent, const std::string& caption, const Job_Ptr& job, bool canAbort = true);

}

#endif

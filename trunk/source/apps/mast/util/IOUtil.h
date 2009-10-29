/***
 * millipede: IOUtil.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_IOUTIL
#define H_MILLIPEDE_IOUTIL

#include <string>

#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

#include <wx/filedlg.h>
#include <wx/window.h>

namespace mp {

//#################### TYPEDEFS ####################
typedef shared_ptr<wxFileDialog> wxFileDialog_Ptr;

//#################### FUNCTIONS ####################
void check_file_exists(const std::string& path);
wxFileDialog_Ptr construct_open_dialog(wxWindow *parent, const std::string& caption, const std::string& wildcard,
									   const std::string& defaultDir = ".", const std::string& defaultFilename = "");
wxFileDialog_Ptr construct_save_dialog(wxWindow *parent, const std::string& caption, const std::string& wildcard,
									   const std::string& defaultDir = ".", const std::string& defaultFilename = "");

}

#endif

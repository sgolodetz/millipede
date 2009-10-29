/***
 * millipede: IOUtil.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "IOUtil.h"

#include <fstream>

#include <common/exceptions/FileNotFoundException.h>
#include "StringConversion.h"

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

}

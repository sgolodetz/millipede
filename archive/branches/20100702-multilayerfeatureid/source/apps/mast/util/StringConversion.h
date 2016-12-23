/***
 * mast: StringConversion.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MAST_STRINGCONVERSION
#define H_MAST_STRINGCONVERSION

#include <string>
#include <wx/string.h>

namespace mp {

wxString string_to_wxString(const std::string& s);
std::string wxString_to_string(const wxString& wxs);

}

#endif

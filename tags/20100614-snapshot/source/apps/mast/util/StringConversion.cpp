/***
 * mast: StringConversion.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "StringConversion.h"

namespace mp {

wxString string_to_wxString(const std::string& s)
{
	return wxString(s.c_str(), wxConvLocal);
}

std::string wxString_to_string(const wxString& wxs)
{
	// FIXME:	This is almost certainly not the right way to go about things
	//			and will need fixing in due course. The reason I've done it
	//			this way for the moment is that I'm not sure yet how to do
	//			it properly.
	return std::string(wxs.ToAscii());
}

}

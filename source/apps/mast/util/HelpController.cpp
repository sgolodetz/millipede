/***
 * millipede: HelpController.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "HelpController.h"

#include <wx/fs_zip.h>

#include "StringConversion.h"

namespace mp {

//#################### SINGLETON IMPLEMENTATION ####################
HelpController::HelpController()
:	m_base(NULL)
{}

HelpController& HelpController::instance()
{
	static HelpController s_instance;
	return s_instance;
}

//#################### PUBLIC METHODS ####################
void HelpController::display_contents()
{
	m_base->DisplayContents();
}

void HelpController::display_section(const std::string& section)
{
	m_base->DisplaySection(string_to_wxString(section));
}

void HelpController::initialize()
{
	wxFileSystem::AddHandler(new wxZipFSHandler);
	m_base = new wxHelpController;

#ifdef __WXMSW__
	m_base->Initialize("../resources/millipede.chm");
#else
	m_base->Initialize("../resources/millipede.htb");
#endif
}

void HelpController::shutdown()
{
	m_base->Quit();
	delete m_base;
	m_base = NULL;
}

}

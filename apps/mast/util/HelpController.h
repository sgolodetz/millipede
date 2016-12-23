/***
 * millipede: HelpController.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_HELPCONTROLLER
#define H_MILLIPEDE_HELPCONTROLLER

#include <string>

#include <wx/help.h>

namespace mp {

class HelpController
{
	//#################### PRIVATE VARIABLES ####################
private:
	wxHelpController *m_base;

	//#################### SINGLETON IMPLEMENTATION ####################
private:
	HelpController();
public:
	static HelpController& instance();

	//#################### PUBLIC METHODS ####################
public:
	void display_contents();
	void display_section(const std::string& section);
	void initialize();
	void shutdown();
};

}

#endif

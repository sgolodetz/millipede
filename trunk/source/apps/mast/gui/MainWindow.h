/***
 * mast: MainWindow.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MAST_MAINWINDOW
#define H_MAST_MAINWINDOW

#include <wx/frame.h>

namespace mp {

class MainWindow : public wxFrame
{
	//#################### CONSTANTS ####################
private:
	enum
	{
		MENUID_BASE = wxID_HIGHEST,		// a dummy value which is never used: subsequent values are guaranteed to be higher than this
		MENUID_EDIT_CLEARUNDOHISTORY,
		MENUID_EDIT_REDO,
		MENUID_EDIT_UNDO,
		MENUID_FILE_EXIT,
		MENUID_FILE_OPEN,
		MENUID_FILE_OPEN_DICOMDIR,
		MENUID_FILE_OPEN_MODEL,
		MENUID_FILE_OPEN_VOLUMECHOICE,
		MENUID_FILE_SAVE_MODEL,
		MENUID_FILE_SAVE_VOLUMECHOICE,
		MENUID_HELP_ABOUT,
		MENUID_TOOLS_FEATURESQUANTIFIER,
	};

	//#################### PRIVATE VARIABLES ####################
private:
	wxMenuBar *m_menuBar;
	wxMenu *m_viewMenu;

	//#################### CONSTRUCTORS ####################
public:
	explicit MainWindow(const std::string& title);

	//#################### PUBLIC METHODS ####################
public:
	void setup();

	//#################### PRIVATE METHODS ####################
private:
	void setup_menus();
};

}

#endif

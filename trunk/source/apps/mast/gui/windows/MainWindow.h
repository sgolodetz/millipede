/***
 * mast: MainWindow.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MAST_MAINWINDOW
#define H_MAST_MAINWINDOW

#include <string>

#include <wx/frame.h>

namespace mp {

class MainWindow : public wxFrame
{
	//#################### PRIVATE VARIABLES ####################
private:
	wxMenuBar *m_menuBar;

	//#################### CONSTRUCTORS ####################
public:
	explicit MainWindow(const std::string& title);

	//#################### PRIVATE METHODS ####################
private:
	void setup_menus();

	//#################### EVENT HANDLERS ####################
public:
	//~~~~~~~~~~~~~~~~~~~~ MENUS ~~~~~~~~~~~~~~~~~~~~
	void OnMenuFileExit(wxCommandEvent&);
	void OnMenuFileOpenDICOMDIR(wxCommandEvent&);
	void OnMenuHelpAbout(wxCommandEvent&);

	//#################### EVENT TABLE ####################
	DECLARE_EVENT_TABLE()
};

}

#endif

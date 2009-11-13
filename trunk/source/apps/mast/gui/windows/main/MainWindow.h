/***
 * mast: MainWindow.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MAST_MAINWINDOW
#define H_MAST_MAINWINDOW

#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

#include <wx/frame.h>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef shared_ptr<class VolumeLoader> VolumeLoader_Ptr;

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
	void show_progress_dialog(const VolumeLoader_Ptr& loader);
	void volume_loader_thread(const VolumeLoader_Ptr& loader);

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

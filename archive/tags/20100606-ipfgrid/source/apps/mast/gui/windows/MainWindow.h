/***
 * mast: MainWindow.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MAST_MAINWINDOW
#define H_MAST_MAINWINDOW

#include <string>

#include <boost/shared_ptr.hpp>

#include <wx/frame.h>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef boost::shared_ptr<const class DICOMDirectory> DICOMDirectory_CPtr;
struct DICOMVolumeChoice;

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
	void load_test_volume(const std::string& volumeChoiceFilename);
	void load_volume(const DICOMVolumeChoice& volumeChoice);
	void load_volume(const DICOMDirectory_CPtr& dicomdir, const DICOMVolumeChoice& volumeChoice);
	void setup_gui();
	void setup_menus();

	//#################### EVENT HANDLERS ####################
public:
	//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
	void OnButtonOpenTestVolume1(wxCommandEvent&);

	//~~~~~~~~~~~~~~~~~~~~ COMMON ~~~~~~~~~~~~~~~~~~~~
	void OnCommonExit(wxCommandEvent&);
	void OnCommonOpenDICOMDIR(wxCommandEvent&);

	//~~~~~~~~~~~~~~~~~~~~ MENUS ~~~~~~~~~~~~~~~~~~~~
	void OnMenuHelpAbout(wxCommandEvent&);

	//#################### EVENT TABLE ####################
	DECLARE_EVENT_TABLE()
};

}

#endif

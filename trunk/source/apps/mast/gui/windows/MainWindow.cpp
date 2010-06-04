/***
 * mast: MainWindow.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "MainWindow.h"

#include <wx/menu.h>
#include <wx/msgdlg.h>

#include <common/dicom/volumes/DICOMVolumeLoader.h>
#include <mast/gui/dialogs/DialogUtil.h>
#include <mast/gui/dialogs/VolumeChooserDialog.h>
#include <mast/gui/windows/PartitionWindow.h>
#include <mast/util/StringConversion.h>

namespace {

//#################### LOCAL CONSTANTS ####################
enum
{
	ID_BASE = wxID_HIGHEST,		// a dummy value which is never used: subsequent values are guaranteed to be higher than this
	MENUID_FILE_EXIT,
	MENUID_FILE_OPEN,
	MENUID_FILE_OPEN_DICOMDIR,
	MENUID_FILE_OPEN_MODEL,
	MENUID_FILE_OPEN_VOLUMECHOICE,
	MENUID_FILE_REPLACE_VOLUMECHOICESECTION,
	MENUID_FILE_SAVE_MODEL,
	MENUID_FILE_SAVE_VOLUMECHOICE,
	MENUID_HELP_ABOUT,
};

}

namespace mp {

//#################### CONSTRUCTORS ####################
MainWindow::MainWindow(const std::string& title)
:	wxFrame(NULL, wxID_ANY, string_to_wxString(title), wxDefaultPosition, wxSize(600,400))
{
	setup_menus();
}

//#################### PRIVATE METHODS ####################
void MainWindow::setup_menus()
{
	wxMenu *fileMenu = new wxMenu;
	wxMenu *fileOpenMenu = new wxMenu;
	fileMenu->AppendSubMenu(fileOpenMenu, wxT("&Open"));
		fileOpenMenu->Append(MENUID_FILE_OPEN_DICOMDIR, wxT("&DICOMDIR...\tCtrl+Shift+O"));
		fileOpenMenu->Append(MENUID_FILE_OPEN_MODEL, wxT("&Model...\tCtrl+O"));
		fileOpenMenu->Append(MENUID_FILE_OPEN_VOLUMECHOICE, wxT("&Volume Choice...\tCtrl+Alt+O"));
	wxMenu *fileSaveMenu = new wxMenu;
	fileMenu->AppendSubMenu(fileSaveMenu, wxT("&Save"));
		fileSaveMenu->Append(MENUID_FILE_SAVE_MODEL, wxT("&Model...\tCtrl+S"));
		fileSaveMenu->Append(MENUID_FILE_SAVE_VOLUMECHOICE, wxT("&Volume Choice...\tCtrl+Alt+S"));
	fileMenu->AppendSeparator();
	fileMenu->Append(MENUID_FILE_REPLACE_VOLUMECHOICESECTION, wxT("&Replace Volume Choice Section..."));
	fileMenu->AppendSeparator();
	fileMenu->Append(MENUID_FILE_EXIT, wxT("E&xit\tAlt+F4"));

	wxMenu *helpMenu = new wxMenu;
	helpMenu->Append(MENUID_HELP_ABOUT, wxT("&About...\tF2"));

	m_menuBar = new wxMenuBar;
	m_menuBar->Append(fileMenu, wxT("&File"));
	m_menuBar->Append(helpMenu, wxT("&Help"));

	SetMenuBar(m_menuBar);
}

//#################### EVENT HANDLERS ####################

//~~~~~~~~~~~~~~~~~~~~ MENUS ~~~~~~~~~~~~~~~~~~~~
void MainWindow::OnMenuFileExit(wxCommandEvent&)
{
	Close();
}

void MainWindow::OnMenuFileOpenDICOMDIR(wxCommandEvent&)
{
	wxFileDialog_Ptr dialog = construct_open_dialog(this, "Open DICOMDIR", "DICOMDIR Files|DICOMDIR");
	if(dialog->ShowModal() == wxID_OK)
	{
		std::string path = wxString_to_string(dialog->GetPath());
		try
		{
			check_file_exists(path);

			// Display a volume chooser dialog to allow the user to choose which volume to load.
			VolumeChooserDialog dialog(path);
			dialog.ShowModal();

			if(dialog.volume_choice())
			{
				DICOMVolumeLoader_Ptr loader(new DICOMVolumeLoader(dialog.dicomdir(), *dialog.volume_choice()));
				Job::execute_in_thread(loader);
				show_progress_dialog(this, "Loading DICOM Volume", loader);

				if(!loader->is_aborted())
				{
					// Create a window for the user to interact with the new volume.
					std::string caption = "MAST - " + loader->volume_choice().description() + " - Untitled";
					PartitionWindow *partitionWindow = new PartitionWindow(this, caption, loader->volume(), loader->volume_choice());
					partitionWindow->Show(true);
				}
			}
		}
		catch(std::exception& e)
		{
			wxMessageBox(string_to_wxString(e.what()), wxT("Error"), wxOK|wxICON_ERROR|wxCENTRE, this);
		}
	}
}

void MainWindow::OnMenuHelpAbout(wxCommandEvent&)
{
	std::ostringstream oss;
	oss << "MAST (the Millipede Automatic Segmentation Tool) is a program I am developing as part of "
		<< "my computing doctorate at Oxford University, under the watchful guidance and supervision "
		<< "of Dr Stephen Cameron and Dr Irina Voiculescu. I am extremely grateful to both of them for "
		<< "their help and support throughout.\n"
		<< '\n'
		<< "MAST is the successor application to FAST, the automatic segmentation tool which I wrote "
		<< "to accompany my centipede image processing library.\n"
		<< '\n'
		<< "- Stuart Golodetz, November 2009";
	wxMessageBox(string_to_wxString(oss.str()), wxT("About MAST"), wxOK|wxCENTRE, this);
}

//#################### EVENT TABLE ####################
BEGIN_EVENT_TABLE(MainWindow, wxFrame)
	//~~~~~~~~~~~~~~~~~~~~ MENUS ~~~~~~~~~~~~~~~~~~~~
	EVT_MENU(MENUID_FILE_EXIT, MainWindow::OnMenuFileExit)
	EVT_MENU(MENUID_FILE_OPEN_DICOMDIR, MainWindow::OnMenuFileOpenDICOMDIR)
	EVT_MENU(MENUID_HELP_ABOUT, MainWindow::OnMenuHelpAbout)
END_EVENT_TABLE()

}

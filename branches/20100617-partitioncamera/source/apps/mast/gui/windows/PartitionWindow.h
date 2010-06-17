/***
 * millipede: PartitionWindow.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PARTITIONWINDOW
#define H_MILLIPEDE_PARTITIONWINDOW

#include <boost/shared_ptr.hpp>

#include <wx/frame.h>
#include <wx/glcanvas.h>

#include <common/dicom/volumes/DICOMVolumeChoice.h>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef boost::shared_ptr<class DICOMVolume> DICOMVolume_Ptr;
typedef boost::shared_ptr<class ICommandManager> ICommandManager_Ptr;
class PartitionView;

class PartitionWindow : public wxFrame
{
	//#################### PRIVATE VARIABLES ####################
private:
	ICommandManager_Ptr m_commandManager;
	wxMenuBar *m_menuBar;
	PartitionView *m_view;

	//#################### CONSTRUCTORS ####################
public:
	PartitionWindow(wxWindow *parent, const std::string& title, const DICOMVolume_Ptr& volume, const DICOMVolumeChoice& volumeChoice, wxGLContext *context = NULL);

	//#################### PUBLIC METHODS ####################
public:
	wxGLContext *get_context() const;

	//#################### PRIVATE METHODS ####################
private:
	void setup_gui(const DICOMVolume_Ptr& volume, const DICOMVolumeChoice& volumeChoice, wxGLContext *context);
	void setup_menus();

	//#################### EVENT HANDLERS ####################
public:
	//~~~~~~~~~~~~~~~~~~~~ MENUS ~~~~~~~~~~~~~~~~~~~~
	void OnMenuActionsClearHistory(wxCommandEvent&);
	void OnMenuActionsRedo(wxCommandEvent&);
	void OnMenuActionsUndo(wxCommandEvent&);
	void OnMenuFileExit(wxCommandEvent&);

	//~~~~~~~~~~~~~~~~~~~~ UI UPDATES ~~~~~~~~~~~~~~~~~~~~
	void OnUpdateMenuActionsClearHistory(wxUpdateUIEvent& e);
	void OnUpdateMenuActionsRedo(wxUpdateUIEvent& e);
	void OnUpdateMenuActionsUndo(wxUpdateUIEvent& e);

	//#################### EVENT TABLE ####################
	DECLARE_EVENT_TABLE()
};

}

#endif

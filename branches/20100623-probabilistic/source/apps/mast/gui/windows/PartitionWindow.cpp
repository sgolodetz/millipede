/***
 * millipede: PartitionWindow.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "PartitionWindow.h"

#include <wx/menu.h>
#include <wx/sizer.h>

#include <common/commands/UndoableCommandManager.h>
#include <mast/gui/components/partitionview/PartitionView.h>
#include <mast/util/StringConversion.h>

namespace {

//#################### LOCAL CONSTANTS ####################
enum
{
	MENUID_BASE = wxID_HIGHEST,		// a dummy value which is never used: subsequent values are guaranteed to be higher than this
	MENUID_ACTIONS_CLEARHISTORY,
	MENUID_ACTIONS_REDO,
	MENUID_ACTIONS_UNDO,
	MENUID_FILE_EXIT,
	MENUID_NAVIGATION_CENTRECAMERA,
	MENUID_NAVIGATION_FITTOVIEW,
	MENUID_NAVIGATION_GOTOSLICE,
	MENUID_NAVIGATION_NEXTLAYER,
	MENUID_NAVIGATION_NEXTSLICE,
	MENUID_NAVIGATION_PANDOWN,
	MENUID_NAVIGATION_PANLEFT,
	MENUID_NAVIGATION_PANRIGHT,
	MENUID_NAVIGATION_PANUP,
	MENUID_NAVIGATION_PREVIOUSLAYER,
	MENUID_NAVIGATION_PREVIOUSSLICE,
	MENUID_NAVIGATION_ZOOMIN,
	MENUID_NAVIGATION_ZOOMOUT,
	MENUID_SEGMENTATION_SEGMENTCTVOLUME,
};

}

namespace mp {

//#################### CONSTRUCTORS ####################
PartitionWindow::PartitionWindow(wxWindow *parent, const std::string& title, const DICOMVolume_Ptr& volume, const DICOMVolumeChoice& volumeChoice,
								 wxGLContext *context)
:	wxFrame(parent, -1, string_to_wxString(title)), m_commandManager(new UndoableCommandManager)
{
	setup_menus();
	setup_gui(volume, volumeChoice, context);
}

//#################### PUBLIC METHODS ####################
wxGLContext *PartitionWindow::get_context() const
{
	return m_view->get_context();
}

//#################### PRIVATE METHODS ####################
void PartitionWindow::setup_gui(const DICOMVolume_Ptr& volume, const DICOMVolumeChoice& volumeChoice, wxGLContext *context)
{
	SetBackgroundColour(wxColour(240,240,240));

	wxGridSizer *sizer = new wxGridSizer(1, 1, 0, 0);
	SetSizer(sizer);

	Show();

	m_view = new PartitionView(this, volume, volumeChoice, m_commandManager, context);
	sizer->Add(m_view);

	sizer->Fit(this);
}

void PartitionWindow::setup_menus()
{
	wxMenu *fileMenu = new wxMenu;
	fileMenu->Append(wxID_ANY, wxT("&Save\tCtrl+S"));
	fileMenu->Append(wxID_ANY, wxT("Save &As..."));
	fileMenu->AppendSeparator();
	fileMenu->Append(MENUID_FILE_EXIT, wxT("E&xit\tAlt+F4"));

	wxMenu *actionsMenu = new wxMenu;
	actionsMenu->Append(MENUID_ACTIONS_UNDO, wxT("&Undo\tCtrl+Z"));
	actionsMenu->Append(MENUID_ACTIONS_REDO, wxT("&Redo\tCtrl+Y"));
	actionsMenu->AppendSeparator();
	actionsMenu->Append(MENUID_ACTIONS_CLEARHISTORY, wxT("&Clear History"));

	wxMenu *navigationMenu = new wxMenu;
	navigationMenu->Append(MENUID_NAVIGATION_PANDOWN, wxT("Pan &Down\tKP_2"));
	navigationMenu->Append(MENUID_NAVIGATION_PANLEFT, wxT("Pan &Left\tKP_4"));
	navigationMenu->Append(MENUID_NAVIGATION_PANRIGHT, wxT("Pan &Right\tKP_6"));
	navigationMenu->Append(MENUID_NAVIGATION_PANUP, wxT("Pan &Up\tKP_8"));
	navigationMenu->AppendSeparator();
	navigationMenu->Append(MENUID_NAVIGATION_NEXTSLICE, wxT("&Next Slice\tKP_9"));
	navigationMenu->Append(MENUID_NAVIGATION_PREVIOUSSLICE, wxT("&Previous Slice\tKP_7"));
	navigationMenu->Append(MENUID_NAVIGATION_GOTOSLICE, wxT("&Goto Slice...\tCtrl+G"));
	navigationMenu->AppendSeparator();
	navigationMenu->Append(MENUID_NAVIGATION_NEXTLAYER, wxT("N&ext Layer\tKP_3"));
	navigationMenu->Append(MENUID_NAVIGATION_PREVIOUSLAYER, wxT("Previous L&ayer\tKP_1"));
	navigationMenu->AppendSeparator();
	navigationMenu->Append(MENUID_NAVIGATION_ZOOMIN, wxT("Zoom &In\t["));
	navigationMenu->Append(MENUID_NAVIGATION_ZOOMOUT, wxT("Zoom &Out\t]"));
	navigationMenu->Append(MENUID_NAVIGATION_CENTRECAMERA, wxT("&Centre Camera\tKP_5"));
	navigationMenu->Append(MENUID_NAVIGATION_FITTOVIEW, wxT("&Fit to View\tCtrl+KP_5"));

	wxMenu *selectionMenu = new wxMenu;
	selectionMenu->Append(wxID_ANY, wxT("&Select Nodes By ID..."));
	selectionMenu->AppendSeparator();
	selectionMenu->Append(wxID_ANY, wxT("&Clear Selection"));

	wxMenu *segmentationMenu = new wxMenu;
	segmentationMenu->Append(MENUID_SEGMENTATION_SEGMENTCTVOLUME, wxT("Segment CT &Volume..."));
	segmentationMenu->AppendSeparator();
	segmentationMenu->Append(wxID_ANY, wxT("&Clone Current Layer"));
	segmentationMenu->Append(wxID_ANY, wxT("&Delete Current Layer"));
	segmentationMenu->Append(wxID_ANY, wxT("&Merge Selected Nodes"));
	wxMenu *splitNodeMenu = new wxMenu;
	segmentationMenu->AppendSubMenu(splitNodeMenu, wxT("&Split Node"));
		splitNodeMenu->Append(wxID_ANY, wxT("Set &Node"));
		splitNodeMenu->Append(wxID_ANY, wxT("&Add Subgroup"));
		splitNodeMenu->Append(wxID_ANY, wxT("&Manage Subgroups..."));
		splitNodeMenu->Append(wxID_ANY, wxT("&Finalize Split"));
		splitNodeMenu->AppendSeparator();
		splitNodeMenu->Append(wxID_ANY, wxT("&Start Again"));
	segmentationMenu->AppendSeparator();
	segmentationMenu->Append(wxID_ANY, wxT("&Unzip Selected Node..."));
	segmentationMenu->AppendSeparator();
	wxMenu *switchParentMenu = new wxMenu;
	segmentationMenu->AppendSubMenu(switchParentMenu, wxT("Switch &Parent"));
		switchParentMenu->Append(wxID_ANY, wxT("Set &Child"));
		switchParentMenu->Append(wxID_ANY, wxT("Set New &Parent"));
		switchParentMenu->AppendSeparator();
		switchParentMenu->Append(wxID_ANY, wxT("&Start Again"));

	wxMenu *featureMenu = new wxMenu;
	wxMenu *autoMarkMenu = new wxMenu;
	featureMenu->AppendSubMenu(autoMarkMenu, wxT("&Automatically Mark"));
		autoMarkMenu->Append(wxID_ANY, wxT("Using &Default Identifier"));
		autoMarkMenu->Append(wxID_ANY, wxT("Using &Script..."));
	wxMenu *manuMarkMenu = new wxMenu;
	featureMenu->AppendSubMenu(manuMarkMenu, wxT("&Manually Mark"));
		manuMarkMenu->Append(wxID_ANY, wxT("&Kidney"));
		// TODO: Other features (possibly via iterating over the feature ID enumeration)
	featureMenu->AppendSeparator();
	wxMenu *selectMarkedMenu = new wxMenu;
	featureMenu->AppendSubMenu(selectMarkedMenu, wxT("&Select Marked"));
		selectMarkedMenu->Append(wxID_ANY, wxT("&Kidney"));
		// TODO: Other features (possibly via iterating over the feature ID enumeration)
	featureMenu->AppendSeparator();
	featureMenu->Append(wxID_ANY, wxT("&Customise Colour Scheme..."));

	wxMenu *toolsMenu = new wxMenu;
	toolsMenu->Append(wxID_ANY, wxT("&Visualize in 3D..."));

	wxMenu *helpMenu = new wxMenu;
	helpMenu->Append(wxID_ANY, wxT("&Contents..."));

	m_menuBar = new wxMenuBar;
	m_menuBar->Append(fileMenu, wxT("&File"));
	m_menuBar->Append(actionsMenu, wxT("&Actions"));
	m_menuBar->Append(navigationMenu, wxT("&Navigation"));
	m_menuBar->Append(selectionMenu, wxT("S&election"));
	m_menuBar->Append(segmentationMenu, wxT("&Segmentation"));
	m_menuBar->Append(featureMenu, wxT("Feature &Identification"));
	m_menuBar->Append(toolsMenu, wxT("&Tools"));
	m_menuBar->Append(helpMenu, wxT("&Help"));

	SetMenuBar(m_menuBar);
}

//#################### EVENT HANDLERS ####################

//~~~~~~~~~~~~~~~~~~~~ MENUS ~~~~~~~~~~~~~~~~~~~~
void PartitionWindow::OnMenuActionsClearHistory(wxCommandEvent&)
{
	m_commandManager->clear_history();
}

void PartitionWindow::OnMenuActionsRedo(wxCommandEvent&)
{
	m_commandManager->redo();
}

void PartitionWindow::OnMenuActionsUndo(wxCommandEvent&)
{
	m_commandManager->undo();
}

void PartitionWindow::OnMenuFileExit(wxCommandEvent&)
{
	Close();
}

void PartitionWindow::OnMenuNavigationCentreCamera(wxCommandEvent&)
{
	m_view->camera()->centre();
}

void PartitionWindow::OnMenuNavigationFitToView(wxCommandEvent&)
{
	m_view->fit_image_to_view();
}

void PartitionWindow::OnMenuNavigationGotoSlice(wxCommandEvent&)
{
	m_view->goto_slice();
}

void PartitionWindow::OnMenuNavigationNextLayer(wxCommandEvent&)
{
	m_view->camera()->goto_next_layer();
}

void PartitionWindow::OnMenuNavigationNextSlice(wxCommandEvent&)
{
	m_view->camera()->goto_next_slice();
}

void PartitionWindow::OnMenuNavigationPanDown(wxCommandEvent&)
{
	m_view->camera()->pan_down();
}

void PartitionWindow::OnMenuNavigationPanLeft(wxCommandEvent&)
{
	m_view->camera()->pan_left();
}

void PartitionWindow::OnMenuNavigationPanRight(wxCommandEvent&)
{
	m_view->camera()->pan_right();
}

void PartitionWindow::OnMenuNavigationPanUp(wxCommandEvent&)
{
	m_view->camera()->pan_up();
}

void PartitionWindow::OnMenuNavigationPreviousLayer(wxCommandEvent&)
{
	m_view->camera()->goto_previous_layer();
}

void PartitionWindow::OnMenuNavigationPreviousSlice(wxCommandEvent&)
{
	m_view->camera()->goto_previous_slice();
}

void PartitionWindow::OnMenuNavigationZoomIn(wxCommandEvent&)
{
	m_view->camera()->set_zoom_level(m_view->camera()->zoom_level() + 1);
}

void PartitionWindow::OnMenuNavigationZoomOut(wxCommandEvent&)
{
	m_view->camera()->set_zoom_level(m_view->camera()->zoom_level() - 1);
}

void PartitionWindow::OnMenuSegmentationSegmentCTVolume(wxCommandEvent&)
{
	m_view->segment_volume();
}

//~~~~~~~~~~~~~~~~~~~~ UI UPDATES ~~~~~~~~~~~~~~~~~~~~
void PartitionWindow::OnUpdateMenuActionsClearHistory(wxUpdateUIEvent& e)
{
	e.Enable(m_commandManager->can_undo() || m_commandManager->can_redo());
}

void PartitionWindow::OnUpdateMenuActionsRedo(wxUpdateUIEvent& e)
{
	if(m_commandManager->can_redo())
	{
		e.Enable(true);
		e.SetText(string_to_wxString("&Redo " + m_commandManager->redo_description() + "\tCtrl+Y"));
	}
	else
	{
		e.Enable(false);
		e.SetText("Cannot Redo\tCtrl+Y");
	}
}

void PartitionWindow::OnUpdateMenuActionsUndo(wxUpdateUIEvent& e)
{
	if(m_commandManager->can_undo())
	{
		e.Enable(true);
		e.SetText(string_to_wxString("&Undo " + m_commandManager->undo_description() + "\tCtrl+Z"));
	}
	else
	{
		e.Enable(false);
		e.SetText("Cannot Undo\tCtrl+Z");
	}
}

void PartitionWindow::OnUpdateMenuNavigationNextLayer(wxUpdateUIEvent& e)
{
	e.Enable(m_view->camera()->has_next_layer());
}

void PartitionWindow::OnUpdateMenuNavigationNextSlice(wxUpdateUIEvent& e)
{
	e.Enable(m_view->camera()->has_next_slice());
}

void PartitionWindow::OnUpdateMenuNavigationPreviousLayer(wxUpdateUIEvent& e)
{
	e.Enable(m_view->camera()->has_previous_layer());
}

void PartitionWindow::OnUpdateMenuNavigationPreviousSlice(wxUpdateUIEvent& e)
{
	e.Enable(m_view->camera()->has_previous_slice());
}

//#################### EVENT TABLE ####################
BEGIN_EVENT_TABLE(PartitionWindow, wxFrame)
	//~~~~~~~~~~~~~~~~~~~~ MENUS ~~~~~~~~~~~~~~~~~~~~
	EVT_MENU(MENUID_ACTIONS_CLEARHISTORY, PartitionWindow::OnMenuActionsClearHistory)
	EVT_MENU(MENUID_ACTIONS_REDO, PartitionWindow::OnMenuActionsRedo)
	EVT_MENU(MENUID_ACTIONS_UNDO, PartitionWindow::OnMenuActionsUndo)
	EVT_MENU(MENUID_FILE_EXIT, PartitionWindow::OnMenuFileExit)
	EVT_MENU(MENUID_NAVIGATION_CENTRECAMERA, PartitionWindow::OnMenuNavigationCentreCamera)
	EVT_MENU(MENUID_NAVIGATION_FITTOVIEW, PartitionWindow::OnMenuNavigationFitToView)
	EVT_MENU(MENUID_NAVIGATION_GOTOSLICE, PartitionWindow::OnMenuNavigationGotoSlice)
	EVT_MENU(MENUID_NAVIGATION_NEXTLAYER, PartitionWindow::OnMenuNavigationNextLayer)
	EVT_MENU(MENUID_NAVIGATION_NEXTSLICE, PartitionWindow::OnMenuNavigationNextSlice)
	EVT_MENU(MENUID_NAVIGATION_PANDOWN, PartitionWindow::OnMenuNavigationPanDown)
	EVT_MENU(MENUID_NAVIGATION_PANLEFT, PartitionWindow::OnMenuNavigationPanLeft)
	EVT_MENU(MENUID_NAVIGATION_PANRIGHT, PartitionWindow::OnMenuNavigationPanRight)
	EVT_MENU(MENUID_NAVIGATION_PANUP, PartitionWindow::OnMenuNavigationPanUp)
	EVT_MENU(MENUID_NAVIGATION_PREVIOUSLAYER, PartitionWindow::OnMenuNavigationPreviousLayer)
	EVT_MENU(MENUID_NAVIGATION_PREVIOUSSLICE, PartitionWindow::OnMenuNavigationPreviousSlice)
	EVT_MENU(MENUID_NAVIGATION_ZOOMIN, PartitionWindow::OnMenuNavigationZoomIn)
	EVT_MENU(MENUID_NAVIGATION_ZOOMOUT, PartitionWindow::OnMenuNavigationZoomOut)
	EVT_MENU(MENUID_SEGMENTATION_SEGMENTCTVOLUME, PartitionWindow::OnMenuSegmentationSegmentCTVolume)

	//~~~~~~~~~~~~~~~~~~~~ UI UPDATES ~~~~~~~~~~~~~~~~~~~~
	EVT_UPDATE_UI(MENUID_ACTIONS_CLEARHISTORY, PartitionWindow::OnUpdateMenuActionsClearHistory)
	EVT_UPDATE_UI(MENUID_ACTIONS_REDO, PartitionWindow::OnUpdateMenuActionsRedo)
	EVT_UPDATE_UI(MENUID_ACTIONS_UNDO, PartitionWindow::OnUpdateMenuActionsUndo)
	EVT_UPDATE_UI(MENUID_NAVIGATION_NEXTLAYER, PartitionWindow::OnUpdateMenuNavigationNextLayer)
	EVT_UPDATE_UI(MENUID_NAVIGATION_NEXTSLICE, PartitionWindow::OnUpdateMenuNavigationNextSlice)
	EVT_UPDATE_UI(MENUID_NAVIGATION_PREVIOUSLAYER, PartitionWindow::OnUpdateMenuNavigationPreviousLayer)
	EVT_UPDATE_UI(MENUID_NAVIGATION_PREVIOUSSLICE, PartitionWindow::OnUpdateMenuNavigationPreviousSlice)
END_EVENT_TABLE()

}

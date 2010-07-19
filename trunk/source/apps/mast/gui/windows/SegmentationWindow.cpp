/***
 * millipede: SegmentationWindow.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "SegmentationWindow.h"

#include <wx/menu.h>
#include <wx/sizer.h>

#include <common/commands/UndoableCommandManager.h>
#include <mast/gui/components/partitionview/PartitionCamera.h>
#include <mast/gui/components/partitionview/PartitionView.h>
#include <mast/gui/components/selectionview/SelectionView.h>
#include <mast/gui/dialogs/FeatureVolumesDialog.h>
#include <mast/util/StringConversion.h>

namespace {

//#################### LOCAL CONSTANTS ####################
enum
{
	MENUID_BASE = wxID_HIGHEST,		// a dummy value which is never used: subsequent values are guaranteed to be higher than this
	MENUID_ACTIONS_CLEARHISTORY,
	MENUID_ACTIONS_REDO,
	MENUID_ACTIONS_UNDO,
	MENUID_FEATURES_TOGGLE_BASE,
	MENUID_FEATURES_TOGGLE_LAST = (MENUID_FEATURES_TOGGLE_BASE+1) + 50,	// reserve enough IDs for 50 different feature types
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
	MENUID_SEGMENTATION_SEGMENTVOLUME,
	MENUID_SELECTION_CLEARSELECTION,
	MENUID_SELECTION_SELECTMARKED_BASE,
	MENUID_SELECTION_SELECTMARKED_LAST = (MENUID_SELECTION_SELECTMARKED_BASE+1) + 50,	// reserve enough IDs for 50 different feature types
	MENUID_TOOLS_QUANTIFYFEATUREVOLUMES,
	MENUID_TOOLS_VISUALIZEIN3D,
};

}

namespace mp {

//#################### CONSTRUCTORS ####################
SegmentationWindow::SegmentationWindow(wxWindow *parent, const std::string& title, const PartitionModel_Ptr& model, wxGLContext *context)
:	wxFrame(parent, wxID_ANY, string_to_wxString(title)), m_commandManager(new UndoableCommandManager), m_model(model)
{
	setup_menus();
	setup_gui(context);
}

//#################### PUBLIC METHODS ####################
wxGLContext *SegmentationWindow::get_context() const
{
	return m_view->get_context();
}

//#################### PRIVATE METHODS ####################
void SegmentationWindow::connect_special_menu_items()
{
	std::vector<AbdominalFeature::Enum> featureTypes = enum_values<AbdominalFeature::Enum>();
	for(size_t i=0, size=featureTypes.size(); i<size; ++i)
	{
		{
			int id = (MENUID_FEATURES_TOGGLE_BASE+1) + i;
			Connect(id, wxEVT_COMMAND_MENU_SELECTED, wxCommandEventHandler(SegmentationWindow::OnMenuFeatureToggle));
			Connect(id, wxEVT_UPDATE_UI, wxUpdateUIEventHandler(SegmentationWindow::OnUpdateNonEmptySelectionNeeder));
		}

		{
			int id = (MENUID_SELECTION_SELECTMARKED_BASE+1) + i;
			Connect(id, wxEVT_COMMAND_MENU_SELECTED, wxCommandEventHandler(SegmentationWindow::OnMenuSelectionSelectMarked));
			Connect(id, wxEVT_UPDATE_UI, wxUpdateUIEventHandler(SegmentationWindow::OnUpdateMenuSelectionSelectMarked));
		}
	}
}

void SegmentationWindow::setup_gui(wxGLContext *context)
{
	SetBackgroundColour(wxColour(240,240,240));

	wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
	SetSizer(sizer);

	Show();

	m_view = new PartitionView(this, m_model, m_commandManager, context);
	sizer->Add(m_view, 0, wxALIGN_CENTRE_HORIZONTAL);

	sizer->AddSpacer(10);

	sizer->Add(new SelectionView<LeafLayer,BranchLayer,Feature>(this, m_model), 0, wxALIGN_CENTRE_HORIZONTAL);

	sizer->Fit(this);
	CenterOnScreen();
}

void SegmentationWindow::setup_menus()
{
	std::vector<AbdominalFeature::Enum> featureTypes = enum_values<AbdominalFeature::Enum>();

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
	wxMenu *selectMarkedMenu = new wxMenu;
	selectionMenu->AppendSubMenu(selectMarkedMenu, wxT("&Select Marked"));
		for(size_t i=0, size=featureTypes.size(); i<size; ++i)
		{
			std::ostringstream oss;
			oss << feature_name(featureTypes[i]);
			std::string key = feature_key(featureTypes[i]);
			if(key != "") oss << " (&" << key << ")";
			selectMarkedMenu->Append((MENUID_SELECTION_SELECTMARKED_BASE+1) + i, string_to_wxString(oss.str()));
		}
	selectionMenu->AppendSeparator();
	selectionMenu->Append(MENUID_SELECTION_CLEARSELECTION, wxT("&Clear Selection"));

	wxMenu *segmentationMenu = new wxMenu;
	segmentationMenu->Append(MENUID_SEGMENTATION_SEGMENTVOLUME, wxT("Segment &Volume..."));
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

	wxMenu *featuresMenu = new wxMenu;
	wxMenu *autoMarkMenu = new wxMenu;
	featuresMenu->AppendSubMenu(autoMarkMenu, wxT("&Automatically Mark"));
		autoMarkMenu->Append(wxID_ANY, wxT("Using &Default Identifier"));
		autoMarkMenu->Append(wxID_ANY, wxT("Using &Script..."));
	wxMenu *toggleMenu = new wxMenu;
	featuresMenu->AppendSubMenu(toggleMenu, wxT("&Toggle Selected Nodes"));
		for(size_t i=0, size=featureTypes.size(); i<size; ++i)
		{
			std::ostringstream oss;
			oss << feature_name(featureTypes[i]);
			std::string key = feature_key(featureTypes[i]);
			std::string shortcut = feature_shortcut(featureTypes[i]);
			if(key != "") oss << " (&" << key << ")";
			if(shortcut != "") oss << '\t' << shortcut;
			toggleMenu->Append((MENUID_FEATURES_TOGGLE_BASE+1) + i, string_to_wxString(oss.str()));
		}
	featuresMenu->AppendSeparator();
	featuresMenu->Append(wxID_ANY, wxT("&Customise Colour Scheme..."));

	wxMenu *toolsMenu = new wxMenu;
	toolsMenu->Append(MENUID_TOOLS_QUANTIFYFEATUREVOLUMES, wxT("&Quantify Feature Volumes...\tCtrl+Q"));
	toolsMenu->Append(MENUID_TOOLS_VISUALIZEIN3D, wxT("&Visualize in 3D..."));

	wxMenu *helpMenu = new wxMenu;
	helpMenu->Append(wxID_ANY, wxT("&Contents..."));

	m_menuBar = new wxMenuBar;
	m_menuBar->Append(fileMenu, wxT("&File"));
	m_menuBar->Append(actionsMenu, wxT("&Actions"));
	m_menuBar->Append(navigationMenu, wxT("&Navigation"));
	m_menuBar->Append(selectionMenu, wxT("S&election"));
	m_menuBar->Append(segmentationMenu, wxT("&Segmentation"));
	m_menuBar->Append(featuresMenu, wxT("Feat&ures"));
	m_menuBar->Append(toolsMenu, wxT("&Tools"));
	m_menuBar->Append(helpMenu, wxT("&Help"));

	SetMenuBar(m_menuBar);

	connect_special_menu_items();
}

//#################### EVENT HANDLERS ####################

//~~~~~~~~~~~~~~~~~~~~ MENUS ~~~~~~~~~~~~~~~~~~~~
void SegmentationWindow::OnMenuActionsClearHistory(wxCommandEvent&)
{
	m_commandManager->clear_history();
}

void SegmentationWindow::OnMenuActionsRedo(wxCommandEvent&)
{
	m_commandManager->redo();
}

void SegmentationWindow::OnMenuActionsUndo(wxCommandEvent&)
{
	m_commandManager->undo();
}

void SegmentationWindow::OnMenuFeatureToggle(wxCommandEvent& e)
{
	AbdominalFeature::Enum feature = AbdominalFeature::Enum(e.GetId() - (MENUID_FEATURES_TOGGLE_BASE+1));
	m_model->multi_feature_selection()->toggle_selection(m_model->selection(), feature);
}

void SegmentationWindow::OnMenuFileExit(wxCommandEvent&)
{
	Close();
}

void SegmentationWindow::OnMenuNavigationCentreCamera(wxCommandEvent&)
{
	m_view->camera()->centre();
}

void SegmentationWindow::OnMenuNavigationFitToView(wxCommandEvent&)
{
	m_view->fit_image_to_view();
}

void SegmentationWindow::OnMenuNavigationGotoSlice(wxCommandEvent&)
{
	m_view->goto_slice();
}

void SegmentationWindow::OnMenuNavigationNextLayer(wxCommandEvent&)
{
	m_view->camera()->goto_next_layer();
}

void SegmentationWindow::OnMenuNavigationNextSlice(wxCommandEvent&)
{
	m_view->camera()->goto_next_slice();
}

void SegmentationWindow::OnMenuNavigationPanDown(wxCommandEvent&)
{
	m_view->camera()->pan_down();
}

void SegmentationWindow::OnMenuNavigationPanLeft(wxCommandEvent&)
{
	m_view->camera()->pan_left();
}

void SegmentationWindow::OnMenuNavigationPanRight(wxCommandEvent&)
{
	m_view->camera()->pan_right();
}

void SegmentationWindow::OnMenuNavigationPanUp(wxCommandEvent&)
{
	m_view->camera()->pan_up();
}

void SegmentationWindow::OnMenuNavigationPreviousLayer(wxCommandEvent&)
{
	m_view->camera()->goto_previous_layer();
}

void SegmentationWindow::OnMenuNavigationPreviousSlice(wxCommandEvent&)
{
	m_view->camera()->goto_previous_slice();
}

void SegmentationWindow::OnMenuNavigationZoomIn(wxCommandEvent&)
{
	m_view->camera()->set_zoom_level(m_view->camera()->zoom_level() + 1);
}

void SegmentationWindow::OnMenuNavigationZoomOut(wxCommandEvent&)
{
	m_view->camera()->set_zoom_level(m_view->camera()->zoom_level() - 1);
}

void SegmentationWindow::OnMenuSegmentationSegmentVolume(wxCommandEvent&)
{
	m_model->segment_volume(this);
}

void SegmentationWindow::OnMenuSelectionClearSelection(wxCommandEvent&)
{
	m_model->selection()->clear();
}

void SegmentationWindow::OnMenuSelectionSelectMarked(wxCommandEvent& e)
{
	AbdominalFeature::Enum feature = AbdominalFeature::Enum(e.GetId() - (MENUID_SELECTION_SELECTMARKED_BASE+1));
	m_model->selection()->replace_with_selection(m_model->multi_feature_selection()->selection(feature));
}

void SegmentationWindow::OnMenuToolsQuantifyFeatureVolumes(wxCommandEvent&)
{
	FeatureVolumesDialog dialog(this, PartitionModel_CPtr(m_model));
	dialog.ShowModal();
}

void SegmentationWindow::OnMenuToolsVisualizeIn3D(wxCommandEvent&)
{
	m_model->visualize_in_3d(this, get_context());
}

//~~~~~~~~~~~~~~~~~~~~ UI UPDATES ~~~~~~~~~~~~~~~~~~~~
void SegmentationWindow::OnUpdateMenuActionsClearHistory(wxUpdateUIEvent& e)
{
	e.Enable(m_commandManager->can_undo() || m_commandManager->can_redo());
}

void SegmentationWindow::OnUpdateMenuActionsRedo(wxUpdateUIEvent& e)
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

void SegmentationWindow::OnUpdateMenuActionsUndo(wxUpdateUIEvent& e)
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

void SegmentationWindow::OnUpdateMenuNavigationNextLayer(wxUpdateUIEvent& e)
{
	e.Enable(m_view->camera()->has_next_layer());
}

void SegmentationWindow::OnUpdateMenuNavigationNextSlice(wxUpdateUIEvent& e)
{
	e.Enable(m_view->camera()->has_next_slice());
}

void SegmentationWindow::OnUpdateMenuNavigationPreviousLayer(wxUpdateUIEvent& e)
{
	e.Enable(m_view->camera()->has_previous_layer());
}

void SegmentationWindow::OnUpdateMenuNavigationPreviousSlice(wxUpdateUIEvent& e)
{
	e.Enable(m_view->camera()->has_previous_slice());
}

void SegmentationWindow::OnUpdateMenuNavigationZoomIn(wxUpdateUIEvent& e)
{
	e.Enable(m_view->camera()->zoom_level() < m_view->camera()->max_zoom_level());
}

void SegmentationWindow::OnUpdateMenuNavigationZoomOut(wxUpdateUIEvent& e)
{
	e.Enable(m_view->camera()->zoom_level() > m_view->camera()->min_zoom_level());
}

void SegmentationWindow::OnUpdateMenuSelectionSelectMarked(wxUpdateUIEvent& e)
{
	AbdominalFeature::Enum feature = AbdominalFeature::Enum(e.GetId() - (MENUID_SELECTION_SELECTMARKED_BASE+1));
	e.Enable(m_model->multi_feature_selection() && !m_model->multi_feature_selection()->selection(feature)->empty());
}

void SegmentationWindow::OnUpdateForestNeeder(wxUpdateUIEvent& e)
{
	e.Enable(m_model->volume_ipf());
}

void SegmentationWindow::OnUpdateNonEmptySelectionNeeder(wxUpdateUIEvent& e)
{
	e.Enable(m_model->selection() && !m_model->selection()->empty());
}

//#################### EVENT TABLE ####################
BEGIN_EVENT_TABLE(SegmentationWindow, wxFrame)
	//~~~~~~~~~~~~~~~~~~~~ MENUS ~~~~~~~~~~~~~~~~~~~~
	EVT_MENU(MENUID_ACTIONS_CLEARHISTORY, SegmentationWindow::OnMenuActionsClearHistory)
	EVT_MENU(MENUID_ACTIONS_REDO, SegmentationWindow::OnMenuActionsRedo)
	EVT_MENU(MENUID_ACTIONS_UNDO, SegmentationWindow::OnMenuActionsUndo)
	EVT_MENU(MENUID_FILE_EXIT, SegmentationWindow::OnMenuFileExit)
	EVT_MENU(MENUID_NAVIGATION_CENTRECAMERA, SegmentationWindow::OnMenuNavigationCentreCamera)
	EVT_MENU(MENUID_NAVIGATION_FITTOVIEW, SegmentationWindow::OnMenuNavigationFitToView)
	EVT_MENU(MENUID_NAVIGATION_GOTOSLICE, SegmentationWindow::OnMenuNavigationGotoSlice)
	EVT_MENU(MENUID_NAVIGATION_NEXTLAYER, SegmentationWindow::OnMenuNavigationNextLayer)
	EVT_MENU(MENUID_NAVIGATION_NEXTSLICE, SegmentationWindow::OnMenuNavigationNextSlice)
	EVT_MENU(MENUID_NAVIGATION_PANDOWN, SegmentationWindow::OnMenuNavigationPanDown)
	EVT_MENU(MENUID_NAVIGATION_PANLEFT, SegmentationWindow::OnMenuNavigationPanLeft)
	EVT_MENU(MENUID_NAVIGATION_PANRIGHT, SegmentationWindow::OnMenuNavigationPanRight)
	EVT_MENU(MENUID_NAVIGATION_PANUP, SegmentationWindow::OnMenuNavigationPanUp)
	EVT_MENU(MENUID_NAVIGATION_PREVIOUSLAYER, SegmentationWindow::OnMenuNavigationPreviousLayer)
	EVT_MENU(MENUID_NAVIGATION_PREVIOUSSLICE, SegmentationWindow::OnMenuNavigationPreviousSlice)
	EVT_MENU(MENUID_NAVIGATION_ZOOMIN, SegmentationWindow::OnMenuNavigationZoomIn)
	EVT_MENU(MENUID_NAVIGATION_ZOOMOUT, SegmentationWindow::OnMenuNavigationZoomOut)
	EVT_MENU(MENUID_SEGMENTATION_SEGMENTVOLUME, SegmentationWindow::OnMenuSegmentationSegmentVolume)
	EVT_MENU(MENUID_SELECTION_CLEARSELECTION, SegmentationWindow::OnMenuSelectionClearSelection)
	EVT_MENU(MENUID_TOOLS_QUANTIFYFEATUREVOLUMES, SegmentationWindow::OnMenuToolsQuantifyFeatureVolumes)
	EVT_MENU(MENUID_TOOLS_VISUALIZEIN3D, SegmentationWindow::OnMenuToolsVisualizeIn3D)

	//~~~~~~~~~~~~~~~~~~~~ UI UPDATES ~~~~~~~~~~~~~~~~~~~~
	EVT_UPDATE_UI(MENUID_ACTIONS_CLEARHISTORY, SegmentationWindow::OnUpdateMenuActionsClearHistory)
	EVT_UPDATE_UI(MENUID_ACTIONS_REDO, SegmentationWindow::OnUpdateMenuActionsRedo)
	EVT_UPDATE_UI(MENUID_ACTIONS_UNDO, SegmentationWindow::OnUpdateMenuActionsUndo)
	EVT_UPDATE_UI(MENUID_NAVIGATION_NEXTLAYER, SegmentationWindow::OnUpdateMenuNavigationNextLayer)
	EVT_UPDATE_UI(MENUID_NAVIGATION_NEXTSLICE, SegmentationWindow::OnUpdateMenuNavigationNextSlice)
	EVT_UPDATE_UI(MENUID_NAVIGATION_PREVIOUSLAYER, SegmentationWindow::OnUpdateMenuNavigationPreviousLayer)
	EVT_UPDATE_UI(MENUID_NAVIGATION_PREVIOUSSLICE, SegmentationWindow::OnUpdateMenuNavigationPreviousSlice)
	EVT_UPDATE_UI(MENUID_NAVIGATION_ZOOMIN, SegmentationWindow::OnUpdateMenuNavigationZoomIn)
	EVT_UPDATE_UI(MENUID_NAVIGATION_ZOOMOUT, SegmentationWindow::OnUpdateMenuNavigationZoomOut)
	EVT_UPDATE_UI(MENUID_SELECTION_CLEARSELECTION, SegmentationWindow::OnUpdateNonEmptySelectionNeeder)
	EVT_UPDATE_UI(MENUID_TOOLS_QUANTIFYFEATUREVOLUMES, SegmentationWindow::OnUpdateForestNeeder)
	EVT_UPDATE_UI(MENUID_TOOLS_VISUALIZEIN3D, SegmentationWindow::OnUpdateForestNeeder)
END_EVENT_TABLE()

}

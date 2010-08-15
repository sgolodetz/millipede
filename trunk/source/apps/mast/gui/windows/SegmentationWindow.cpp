/***
 * millipede: SegmentationWindow.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "SegmentationWindow.h"

#include <iterator>

#include <wx/menu.h>
#include <wx/sizer.h>

#include <common/commands/UndoableCommandManager.h>
#include <common/featureid/MultiFeatureIdentifier3D.h>
#include <common/featureid/SpineIdentifier3D.h>
#include <mast/gui/components/partitionview/PartitionCamera.h>
#include <mast/gui/components/partitionview/PartitionView.h>
#include <mast/gui/components/selectionview/SelectionView.h>
#include <mast/gui/dialogs/DialogUtil.h>
#include <mast/gui/dialogs/FeatureVolumesDialog.h>
#include <mast/gui/dialogs/ManageFeatureSelectionsDialog.h>
#include <mast/gui/dialogs/ValidateFeatureSelectionDialog.h>
#include <mast/util/HelpController.h>
#include <mast/util/StringConversion.h>

namespace {

//#################### LOCAL CONSTANTS ####################
enum
{
	MENUID_BASE = wxID_HIGHEST,		// a dummy value which is never used: subsequent values are guaranteed to be higher than this
	MENUID_ACTIONS_CLEARHISTORY,
	MENUID_ACTIONS_REDO,
	MENUID_ACTIONS_UNDO,
	MENUID_FEATURES_AUTOIDENTIFY_MULTIFEATURE3D,
	MENUID_FEATURES_AUTOIDENTIFY_SPINE3D,
	MENUID_FEATURES_CLEAR_ALL,
	MENUID_FEATURES_CLEAR_BASE,
	MENUID_FEATURES_CLEAR_LAST = (MENUID_FEATURES_CLEAR_BASE+1) + 50,	// reserve enough IDs for 50 different feature types
	MENUID_FEATURES_IDENTIFY_BASE,
	MENUID_FEATURES_IDENTIFY_LAST = (MENUID_FEATURES_IDENTIFY_BASE+1) + 50,	// reserve enough IDs for 50 different feature types
	MENUID_FEATURES_MANAGEFEATURESELECTIONS,
	MENUID_FEATURES_TOGGLE_BASE,
	MENUID_FEATURES_TOGGLE_LAST = (MENUID_FEATURES_TOGGLE_BASE+1) + 50,	// reserve enough IDs for 50 different feature types
	MENUID_FEATURES_UNIDENTIFY_BASE,
	MENUID_FEATURES_UNIDENTIFY_LAST = (MENUID_FEATURES_UNIDENTIFY_BASE+1) + 50,	// reserve enough IDs for 50 different feature types
	MENUID_FILE_EXIT,
	MENUID_HELP_CONTENTS,
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
	MENUID_SEGMENTATION_CLONECURRENTLAYER,
	MENUID_SEGMENTATION_DELETECURRENTLAYER,
	MENUID_SEGMENTATION_MERGESELECTEDNODES,
	MENUID_SEGMENTATION_SEGMENTVOLUME,
	MENUID_SEGMENTATION_SPLITNODE_ADDSUBGROUP,
	MENUID_SEGMENTATION_SPLITNODE_FINALIZESPLIT,
	MENUID_SEGMENTATION_SPLITNODE_REMOVESUBGROUP,
	MENUID_SEGMENTATION_SPLITNODE_SETNODE,
	MENUID_SEGMENTATION_SPLITNODE_STARTAGAIN,
	MENUID_SEGMENTATION_SWITCHPARENT_SETCHILD,
	MENUID_SEGMENTATION_SWITCHPARENT_SETNEWPARENT,
	MENUID_SEGMENTATION_SWITCHPARENT_STARTAGAIN,
	MENUID_SEGMENTATION_UNZIPSELECTEDNODE,
	MENUID_SELECTION_CLEARSELECTION,
	MENUID_SELECTION_SELECTMARKED_BASE,
	MENUID_SELECTION_SELECTMARKED_LAST = (MENUID_SELECTION_SELECTMARKED_BASE+1) + 50,	// reserve enough IDs for 50 different feature types
	MENUID_TOOLS_QUANTIFYFEATUREVOLUMES,
	MENUID_TOOLS_VALIDATEFEATURESELECTION,
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
	std::vector<Feature> featureTypes = enum_values<Feature>();
	for(size_t i=0, size=featureTypes.size(); i<size; ++i)
	{
		{
			int id = (MENUID_FEATURES_CLEAR_BASE+1) + i;
			Connect(id, wxEVT_COMMAND_MENU_SELECTED, wxCommandEventHandler(SegmentationWindow::OnMenuFeaturesClearFeature));
			Connect(id, wxEVT_UPDATE_UI, wxUpdateUIEventHandler(SegmentationWindow::OnUpdateMenuFeaturesClearFeature));
		}
		{
			int id = (MENUID_FEATURES_IDENTIFY_BASE+1) + i;
			Connect(id, wxEVT_COMMAND_MENU_SELECTED, wxCommandEventHandler(SegmentationWindow::OnMenuFeaturesIdentify));
			Connect(id, wxEVT_UPDATE_UI, wxUpdateUIEventHandler(SegmentationWindow::OnUpdateNonEmptySelectionNeeder));
		}
		{
			int id = (MENUID_FEATURES_TOGGLE_BASE+1) + i;
			Connect(id, wxEVT_COMMAND_MENU_SELECTED, wxCommandEventHandler(SegmentationWindow::OnMenuFeaturesToggle));
			Connect(id, wxEVT_UPDATE_UI, wxUpdateUIEventHandler(SegmentationWindow::OnUpdateNonEmptySelectionNeeder));
		}
		{
			int id = (MENUID_FEATURES_UNIDENTIFY_BASE+1) + i;
			Connect(id, wxEVT_COMMAND_MENU_SELECTED, wxCommandEventHandler(SegmentationWindow::OnMenuFeaturesUnidentify));
			Connect(id, wxEVT_UPDATE_UI, wxUpdateUIEventHandler(SegmentationWindow::OnUpdateNonEmptySelectionNeeder));
		}
		{
			int id = (MENUID_SELECTION_SELECTMARKED_BASE+1) + i;
			Connect(id, wxEVT_COMMAND_MENU_SELECTED, wxCommandEventHandler(SegmentationWindow::OnMenuSelectionSelectMarked));
			Connect(id, wxEVT_UPDATE_UI, wxUpdateUIEventHandler(SegmentationWindow::OnUpdateMenuSelectionSelectMarked));
		}
	}
}

wxString SegmentationWindow::make_feature_menu_item(const std::vector<Feature>& featureTypes, size_t i, bool useShortcut)
{
	std::ostringstream oss;
	oss << feature_name(featureTypes[i]);
	std::string key = feature_key(featureTypes[i]);
	if(key != "") oss << " (&" << key << ")";

	if(useShortcut)
	{
		std::string shortcut = feature_shortcut(featureTypes[i]);
		if(shortcut != "") oss << '\t' << shortcut;
	}

	return string_to_wxString(oss.str());
}

void SegmentationWindow::setup_gui(wxGLContext *context)
{
	SetBackgroundColour(wxColour(240,240,240));

	wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
	SetSizer(sizer);

	Show();

	m_view = new PartitionView(this, m_model, m_commandManager, context);
	sizer->Add(m_view, 0, wxALIGN_CENTRE_HORIZONTAL|wxALL, 10);

	sizer->AddSpacer(10);

	sizer->Add(new SelectionView<LeafLayer,BranchLayer,Feature>(this, m_model), 0, wxALIGN_CENTRE_HORIZONTAL);

	sizer->Fit(this);
	CenterOnScreen();
}

void SegmentationWindow::setup_menus()
{
	std::vector<Feature> featureTypes = enum_values<Feature>();

	wxMenu *fileMenu = new wxMenu;
#if NYI
	fileMenu->Append(wxID_ANY, wxT("&Save\tCtrl+S"));
	fileMenu->Append(wxID_ANY, wxT("Save &As..."));
	fileMenu->AppendSeparator();
#endif
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
			selectMarkedMenu->Append((MENUID_SELECTION_SELECTMARKED_BASE+1) + i, make_feature_menu_item(featureTypes, i, false));
		}
	selectionMenu->AppendSeparator();
	selectionMenu->Append(MENUID_SELECTION_CLEARSELECTION, wxT("&Clear Selection\tCtrl+Shift+Z"));

	wxMenu *segmentationMenu = new wxMenu;
	segmentationMenu->Append(MENUID_SEGMENTATION_SEGMENTVOLUME, wxT("Segment &Volume...\tCtrl+Alt+Shift+S"));
	segmentationMenu->AppendSeparator();
	segmentationMenu->Append(MENUID_SEGMENTATION_CLONECURRENTLAYER, wxT("&Clone Current Layer\tCtrl+Shift+C"));
	segmentationMenu->Append(MENUID_SEGMENTATION_DELETECURRENTLAYER, wxT("&Delete Current Layer\tCtrl+Shift+D"));
	segmentationMenu->Append(MENUID_SEGMENTATION_MERGESELECTEDNODES, wxT("&Merge Selected Nodes\tCtrl+Shift+M"));
	wxMenu *splitNodeMenu = new wxMenu;
	segmentationMenu->AppendSubMenu(splitNodeMenu, wxT("&Split Node"));
		splitNodeMenu->Append(MENUID_SEGMENTATION_SPLITNODE_SETNODE, wxT("Set &Node\tCtrl+Shift+S"));
		splitNodeMenu->Append(MENUID_SEGMENTATION_SPLITNODE_ADDSUBGROUP, wxT("&Add Subgroup\tCtrl+Shift+A"));
		splitNodeMenu->Append(MENUID_SEGMENTATION_SPLITNODE_REMOVESUBGROUP, wxT("&Remove Subgroup\tCtrl+Shift+R"));
		splitNodeMenu->Append(MENUID_SEGMENTATION_SPLITNODE_FINALIZESPLIT, wxT("&Finalize Split\tCtrl+Shift+F"));
		splitNodeMenu->AppendSeparator();
		splitNodeMenu->Append(MENUID_SEGMENTATION_SPLITNODE_STARTAGAIN, wxT("&Start Again"));
	segmentationMenu->AppendSeparator();
	segmentationMenu->Append(MENUID_SEGMENTATION_UNZIPSELECTEDNODE, wxT("&Unzip Selected Node...\tCtrl+Shift+U"));
	segmentationMenu->AppendSeparator();
	wxMenu *switchParentMenu = new wxMenu;
	segmentationMenu->AppendSubMenu(switchParentMenu, wxT("Switch &Parent"));
		switchParentMenu->Append(MENUID_SEGMENTATION_SWITCHPARENT_SETCHILD, wxT("Set &Child\tCtrl+Shift+X"));
		switchParentMenu->Append(MENUID_SEGMENTATION_SWITCHPARENT_SETNEWPARENT, wxT("Set New &Parent\tCtrl+Shift+V"));
		switchParentMenu->AppendSeparator();
		switchParentMenu->Append(MENUID_SEGMENTATION_SWITCHPARENT_STARTAGAIN, wxT("&Start Again"));

	wxMenu *featuresMenu = new wxMenu;
	featuresMenu->Append(MENUID_FEATURES_MANAGEFEATURESELECTIONS, wxT("&Manage Feature Selections...\tCtrl+M"));
#if NYI
	featuresMenu->Append(wxID_ANY, wxT("&Customise Colour Scheme..."));
#endif
	featuresMenu->AppendSeparator();
	wxMenu *autoIdentifyMenu = new wxMenu;
	featuresMenu->AppendSubMenu(autoIdentifyMenu, wxT("&Automatically Identify Features"));
		autoIdentifyMenu->Append(MENUID_FEATURES_AUTOIDENTIFY_MULTIFEATURE3D, wxT("Using Multi-Feature 3D &Identifier\tAlt+Shift+I"));
		autoIdentifyMenu->Append(MENUID_FEATURES_AUTOIDENTIFY_SPINE3D, wxT("Using &Spine 3D Identifier\tAlt+Shift+S"));
#if NYI
		autoIdentifyMenu->Append(wxID_ANY, wxT("&Using Script..."));
#endif
	featuresMenu->AppendSeparator();
	wxMenu *identifyMenu = new wxMenu;
	featuresMenu->AppendSubMenu(identifyMenu, wxT("&Identify Selection As"));
		for(size_t i=0, size=featureTypes.size(); i<size; ++i)
		{
			identifyMenu->Append((MENUID_FEATURES_IDENTIFY_BASE+1) + i, make_feature_menu_item(featureTypes, i, false));
		}
	wxMenu *toggleMenu = new wxMenu;
	featuresMenu->AppendSubMenu(toggleMenu, wxT("&Toggle Selection As"));
		for(size_t i=0, size=featureTypes.size(); i<size; ++i)
		{
			toggleMenu->Append((MENUID_FEATURES_TOGGLE_BASE+1) + i, make_feature_menu_item(featureTypes, i, true));
		}
	wxMenu *unidentifyMenu = new wxMenu;
	featuresMenu->AppendSubMenu(unidentifyMenu, wxT("&Unidentify Selection As"));
		for(size_t i=0, size=featureTypes.size(); i<size; ++i)
		{
			unidentifyMenu->Append((MENUID_FEATURES_UNIDENTIFY_BASE+1) + i, make_feature_menu_item(featureTypes, i, false));
		}
	featuresMenu->AppendSeparator();
	wxMenu *clearFeatureMenu = new wxMenu;
	featuresMenu->AppendSubMenu(clearFeatureMenu, wxT("&Clear Feature"));
		clearFeatureMenu->Append(MENUID_FEATURES_CLEAR_ALL, wxT("All Features"));
		clearFeatureMenu->AppendSeparator();
		for(size_t i=0, size=featureTypes.size(); i<size; ++i)
		{
			clearFeatureMenu->Append((MENUID_FEATURES_CLEAR_BASE+1) + i, make_feature_menu_item(featureTypes, i, false));
		}

	wxMenu *toolsMenu = new wxMenu;
	toolsMenu->Append(MENUID_TOOLS_QUANTIFYFEATUREVOLUMES, wxT("&Quantify Feature Volumes...\tCtrl+F"));
	toolsMenu->Append(MENUID_TOOLS_VALIDATEFEATURESELECTION, wxT("&Validate Feature Selection...\tCtrl+V"));
	toolsMenu->Append(MENUID_TOOLS_VISUALIZEIN3D, wxT("Visualize in &3D...\tCtrl+3"));

	wxMenu *helpMenu = new wxMenu;
	helpMenu->Append(MENUID_HELP_CONTENTS, wxT("&Contents...\tF1"));

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

void SegmentationWindow::OnMenuFeaturesAutoIdentifyMultiFeature(wxCommandEvent&)
{
	boost::shared_ptr<MultiFeatureIdentifier3D> identifier(new MultiFeatureIdentifier3D(m_model->dicom_volume(), m_model->volume_ipf()));
	execute_with_progress_dialog(identifier, this, "Identifying Features", false);
	m_model->active_multi_feature_selection()->identify_multi_feature_selection(identifier->get_output());
}

void SegmentationWindow::OnMenuFeaturesAutoIdentifySpine(wxCommandEvent&)
{
	boost::shared_ptr<SpineIdentifier3D> identifier(new SpineIdentifier3D(m_model->dicom_volume(), m_model->volume_ipf()));
	execute_with_progress_dialog(identifier, this, "Identifying Spine", false);
	m_model->active_multi_feature_selection()->identify_multi_feature_selection(identifier->get_output());
}

void SegmentationWindow::OnMenuFeaturesClearAll(wxCommandEvent&)
{
	m_model->active_multi_feature_selection()->clear_all();
}

void SegmentationWindow::OnMenuFeaturesClearFeature(wxCommandEvent& e)
{
	Feature feature = Feature(e.GetId() - (MENUID_FEATURES_CLEAR_BASE+1));
	m_model->active_multi_feature_selection()->clear_feature(feature);
}

void SegmentationWindow::OnMenuFeaturesIdentify(wxCommandEvent& e)
{
	Feature feature = Feature(e.GetId() - (MENUID_FEATURES_IDENTIFY_BASE+1));
	m_model->active_multi_feature_selection()->identify_selection(m_model->selection(), feature);
}

void SegmentationWindow::OnMenuFeaturesManageFeatureSelections(wxCommandEvent&)
{
	typedef ManageFeatureSelectionsDialog<PartitionModelT::VolumeIPFMultiFeatureSelectionT,PartitionModelT::VolumeIPFT> Dialog;
	Dialog dialog(this, m_model->multi_feature_selection_manager(), m_model->volume_ipf());
	dialog.ShowModal();
}

void SegmentationWindow::OnMenuFeaturesToggle(wxCommandEvent& e)
{
	Feature feature = Feature(e.GetId() - (MENUID_FEATURES_TOGGLE_BASE+1));
	m_model->active_multi_feature_selection()->toggle_selection(m_model->selection(), feature);
}

void SegmentationWindow::OnMenuFeaturesUnidentify(wxCommandEvent& e)
{
	Feature feature = Feature(e.GetId() - (MENUID_FEATURES_UNIDENTIFY_BASE+1));
	m_model->active_multi_feature_selection()->unidentify_selection(m_model->selection(), feature);
}

void SegmentationWindow::OnMenuFileExit(wxCommandEvent&)
{
	Close();
}

void SegmentationWindow::OnMenuHelpContents(wxCommandEvent&)
{
	HelpController::instance().display_contents();
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

void SegmentationWindow::OnMenuSegmentationCloneCurrentLayer(wxCommandEvent&)
{
	m_view->clone_current_layer();
}

void SegmentationWindow::OnMenuSegmentationDeleteCurrentLayer(wxCommandEvent&)
{
	m_view->delete_current_layer();
}

void SegmentationWindow::OnMenuSegmentationMergeSelectedNodes(wxCommandEvent&)
{
	m_view->merge_selected_nodes();
}

void SegmentationWindow::OnMenuSegmentationSegmentVolume(wxCommandEvent&)
{
	m_model->segment_volume(this);
}

void SegmentationWindow::OnMenuSegmentationSplitNodeAddSubgroup(wxCommandEvent&)
{
	int layerIndex = m_view->camera()->slice_location().layer;
	std::set<PFNodeID> subgroup(m_model->selection()->view_at_layer_cbegin(layerIndex), m_model->selection()->view_at_layer_cend(layerIndex));
	m_view->node_split_manager()->add_subgroup(subgroup);
}

void SegmentationWindow::OnMenuSegmentationSplitNodeFinalizeSplit(wxCommandEvent&)
{
	m_view->node_split_manager()->finalize_split();
	m_view->camera()->goto_next_layer();
}

void SegmentationWindow::OnMenuSegmentationSplitNodeRemoveSubgroup(wxCommandEvent&)
{
	PFNodeID node = *m_model->selection()->view_at_layer_cbegin(m_view->camera()->slice_location().layer);
	m_view->node_split_manager()->remove_subgroup_containing(node);
}

void SegmentationWindow::OnMenuSegmentationSplitNodeSetNode(wxCommandEvent&)
{
	PFNodeID splitNode = *m_model->selection()->view_at_layer_cbegin(m_view->camera()->slice_location().layer);
	m_view->node_split_manager()->set_split_node(splitNode);
	m_view->camera()->goto_previous_layer();
}

void SegmentationWindow::OnMenuSegmentationSplitNodeStartAgain(wxCommandEvent&)
{
	m_view->node_split_manager()->reset();
}

void SegmentationWindow::OnMenuSegmentationSwitchParentSetChild(wxCommandEvent&)
{
	PFNodeID child = *m_model->selection()->view_at_layer_cbegin(m_view->camera()->slice_location().layer);
	m_view->parent_switch_manager()->set_child(child);
	m_view->camera()->goto_next_layer();
}

void SegmentationWindow::OnMenuSegmentationSwitchParentSetNewParent(wxCommandEvent&)
{
	PFNodeID newParent = *m_model->selection()->view_at_layer_cbegin(m_view->camera()->slice_location().layer);
	m_view->parent_switch_manager()->perform_switch(newParent);
}

void SegmentationWindow::OnMenuSegmentationSwitchParentStartAgain(wxCommandEvent&)
{
	m_view->parent_switch_manager()->reset();
}

void SegmentationWindow::OnMenuSegmentationUnzipSelectedNode(wxCommandEvent&)
{
	m_view->unzip_selected_node();
}

void SegmentationWindow::OnMenuSelectionClearSelection(wxCommandEvent&)
{
	m_model->selection()->clear();
}

void SegmentationWindow::OnMenuSelectionSelectMarked(wxCommandEvent& e)
{
	Feature feature = Feature(e.GetId() - (MENUID_SELECTION_SELECTMARKED_BASE+1));
	m_model->selection()->replace_with_selection(m_model->active_multi_feature_selection()->selection(feature));
}

void SegmentationWindow::OnMenuToolsQuantifyFeatureVolumes(wxCommandEvent&)
{
	FeatureVolumesDialog dialog(this, PartitionModel_CPtr(m_model));
	dialog.ShowModal();
}

void SegmentationWindow::OnMenuToolsValidateFeatureSelection(wxCommandEvent&)
{
	typedef ValidateFeatureSelectionDialog<PartitionModelT::VolumeIPFMultiFeatureSelectionT,PartitionModelT::VolumeIPFT> Dialog;
	Dialog dialog(this, m_model->multi_feature_selection_manager(), m_model->volume_ipf());
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

void SegmentationWindow::OnUpdateMenuFeaturesClearAll(wxUpdateUIEvent& e)
{
	e.Enable(m_model->active_multi_feature_selection() && !m_model->active_multi_feature_selection()->empty());
}

void SegmentationWindow::OnUpdateMenuFeaturesClearFeature(wxUpdateUIEvent& e)
{
	PartitionModelT::VolumeIPFMultiFeatureSelection_CPtr mfs = m_model->active_multi_feature_selection();
	Feature feature = Feature(e.GetId() - (MENUID_FEATURES_CLEAR_BASE+1));
	e.Enable(mfs && mfs->has_selection(feature) && !mfs->selection(feature)->empty());
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

void SegmentationWindow::OnUpdateMenuSegmentationDeleteCurrentLayer(wxUpdateUIEvent& e)
{
	e.Enable(m_model->volume_ipf() && m_model->volume_ipf()->highest_layer() > 1);
}

void SegmentationWindow::OnUpdateMenuSegmentationMergeSelectedNodes(wxUpdateUIEvent& e)
{
	if(m_model->selection())
	{
		int mergeLayer = m_model->selection()->merge_layer(m_view->camera()->slice_location().layer);
		if(mergeLayer > 0)
		{
			int nodeCount = std::distance(m_model->selection()->view_at_layer_cbegin(mergeLayer), m_model->selection()->view_at_layer_cend(mergeLayer));
			e.Enable(nodeCount > 1);
		}
	}
	else e.Enable(false);
}

void SegmentationWindow::OnUpdateMenuSegmentationSplitNodeAddSubgroup(wxUpdateUIEvent& e)
{
	e.Enable(false);

	// Check that the selection is available.
	if(!m_model->selection()) return;

	// Check that we're in the process of splitting a node.
	PartitionView::NodeSplitManager_Ptr nodeSplitManager = m_view->node_split_manager();
	PFNodeID splitNode = nodeSplitManager->split_node();
	if(splitNode == PFNodeID::invalid()) return;

	// Check that we're viewing the layer containing the split children, and that all the selected nodes are in this layer.
	int viewLayer = m_view->camera()->slice_location().layer;
	if(viewLayer != splitNode.layer() - 1) return;

	int mergeLayer = m_model->selection()->merge_layer(viewLayer);
	if(mergeLayer != viewLayer) return;

	// Check that all the selected nodes are as-yet-unallocated children of the split node.
	std::set<PFNodeID> selectedNodes(m_model->selection()->view_at_layer_cbegin(mergeLayer), m_model->selection()->view_at_layer_cend(mergeLayer));
	const std::set<PFNodeID>& unallocatedChildren = nodeSplitManager->unallocated_children();
	if(!std::includes(unallocatedChildren.begin(), unallocatedChildren.end(), selectedNodes.begin(), selectedNodes.end())) return;

	// Check that the selected nodes are connected.
	std::set<int> selectedNodeIndices;
	for(std::set<PFNodeID>::const_iterator it=selectedNodes.begin(), iend=selectedNodes.end(); it!=iend; ++it)
	{
		selectedNodeIndices.insert(it->index());
	}
	e.Enable(m_model->volume_ipf()->are_connected(selectedNodeIndices, splitNode.layer() - 1));
}

void SegmentationWindow::OnUpdateMenuSegmentationSplitNodeFinalizeSplit(wxUpdateUIEvent& e)
{
	e.Enable(m_view->node_split_manager() && m_view->node_split_manager()->split_node() != PFNodeID::invalid());
}

void SegmentationWindow::OnUpdateMenuSegmentationSplitNodeRemoveSubgroup(wxUpdateUIEvent& e)
{
	e.Enable(false);

	// Check that the node split manager is available.
	PartitionView::NodeSplitManager_Ptr nodeSplitManager = m_view->node_split_manager();
	if(!nodeSplitManager) return;

	// Check that only one node is selected (when viewed from the current layer).
	int layerIndex = m_view->camera()->slice_location().layer;
	int nodeCount = std::distance(m_model->selection()->view_at_layer_cbegin(layerIndex), m_model->selection()->view_at_layer_cend(layerIndex));
	if(nodeCount != 1) return;

	// Check the node is actually in the layer being viewed.
	PFNodeID node = *m_model->selection()->view_at_layer_cbegin(layerIndex);
	if(node.layer() != layerIndex) return;

	// Check that the node is an allocated child of the node currently being split.
	const std::set<PFNodeID>& allocatedChildren = nodeSplitManager->allocated_children();
	e.Enable(allocatedChildren.find(node) != allocatedChildren.end());
}

void SegmentationWindow::OnUpdateMenuSegmentationSplitNodeStartAgain(wxUpdateUIEvent& e)
{
	e.Enable(m_view->node_split_manager() && !m_view->node_split_manager()->unallocated_children().empty());
}

void SegmentationWindow::OnUpdateMenuSegmentationSwitchParentSetNewParent(wxUpdateUIEvent& e)
{
	e.Enable(false);

	PartitionView::ParentSwitchManager_Ptr parentSwitchManager = m_view->parent_switch_manager();
	if(parentSwitchManager)
	{
		int layerIndex = m_view->camera()->slice_location().layer;
		int nodeCount = std::distance(m_model->selection()->view_at_layer_cbegin(layerIndex), m_model->selection()->view_at_layer_cend(layerIndex));
		if(nodeCount == 1)
		{
			PFNodeID newParent = *m_model->selection()->view_at_layer_cbegin(layerIndex);
			const std::set<PFNodeID>& potentialNewParents = parentSwitchManager->potential_new_parents();
			e.Enable(potentialNewParents.find(newParent) != potentialNewParents.end());
		}
	}
}

void SegmentationWindow::OnUpdateMenuSegmentationSwitchParentStartAgain(wxUpdateUIEvent& e)
{
	e.Enable(m_view->parent_switch_manager() && m_view->parent_switch_manager()->has_child());
}

void SegmentationWindow::OnUpdateMenuSelectionSelectMarked(wxUpdateUIEvent& e)
{
	Feature feature = Feature(e.GetId() - (MENUID_SELECTION_SELECTMARKED_BASE+1));
	e.Enable(m_model->active_multi_feature_selection() && !m_model->active_multi_feature_selection()->selection(feature)->empty());
}

void SegmentationWindow::OnUpdateMenuToolsValidateFeatureSelection(wxUpdateUIEvent& e)
{
	e.Enable(m_model->multi_feature_selection_manager() && m_model->multi_feature_selection_manager()->multi_feature_selections().size() >= 2);
}

void SegmentationWindow::OnUpdateForestNeeder(wxUpdateUIEvent& e)
{
	e.Enable(m_model->volume_ipf());
}

void SegmentationWindow::OnUpdateNonEmptySelectionNeeder(wxUpdateUIEvent& e)
{
	e.Enable(m_model->selection() && !m_model->selection()->empty());
}

void SegmentationWindow::OnUpdateSingleNonHighestNodeSelectionNeeder(wxUpdateUIEvent& e)
{
	e.Enable(false);
	if(m_model->selection())
	{
		int layerIndex = m_view->camera()->slice_location().layer;
		if(layerIndex < m_model->volume_ipf()->highest_layer())
		{
			int nodeCount = std::distance(m_model->selection()->view_at_layer_cbegin(layerIndex), m_model->selection()->view_at_layer_cend(layerIndex));
			if(nodeCount == 1)
			{
				PFNodeID node = *m_model->selection()->view_at_layer_cbegin(layerIndex);
				e.Enable(node.layer() == layerIndex);
			}
		}
	}
}

void SegmentationWindow::OnUpdateSingleNonLowestNodeSelectionNeeder(wxUpdateUIEvent& e)
{
	e.Enable(false);
	if(m_model->selection())
	{
		int layerIndex = m_view->camera()->slice_location().layer;
		if(layerIndex > 1)
		{
			int nodeCount = std::distance(m_model->selection()->view_at_layer_cbegin(layerIndex), m_model->selection()->view_at_layer_cend(layerIndex));
			if(nodeCount == 1)
			{
				PFNodeID node = *m_model->selection()->view_at_layer_cbegin(layerIndex);
				e.Enable(node.layer() == layerIndex);
			}
		}
	}
}

//#################### EVENT TABLE ####################
BEGIN_EVENT_TABLE(SegmentationWindow, wxFrame)
	//~~~~~~~~~~~~~~~~~~~~ MENUS ~~~~~~~~~~~~~~~~~~~~
	EVT_MENU(MENUID_ACTIONS_CLEARHISTORY, SegmentationWindow::OnMenuActionsClearHistory)
	EVT_MENU(MENUID_ACTIONS_REDO, SegmentationWindow::OnMenuActionsRedo)
	EVT_MENU(MENUID_ACTIONS_UNDO, SegmentationWindow::OnMenuActionsUndo)
	EVT_MENU(MENUID_FEATURES_AUTOIDENTIFY_MULTIFEATURE3D, SegmentationWindow::OnMenuFeaturesAutoIdentifyMultiFeature)
	EVT_MENU(MENUID_FEATURES_AUTOIDENTIFY_SPINE3D, SegmentationWindow::OnMenuFeaturesAutoIdentifySpine)
	EVT_MENU(MENUID_FEATURES_CLEAR_ALL, SegmentationWindow::OnMenuFeaturesClearAll)
	EVT_MENU(MENUID_FEATURES_MANAGEFEATURESELECTIONS, SegmentationWindow::OnMenuFeaturesManageFeatureSelections)
	EVT_MENU(MENUID_FILE_EXIT, SegmentationWindow::OnMenuFileExit)
	EVT_MENU(MENUID_HELP_CONTENTS, SegmentationWindow::OnMenuHelpContents)
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
	EVT_MENU(MENUID_SEGMENTATION_CLONECURRENTLAYER, SegmentationWindow::OnMenuSegmentationCloneCurrentLayer)
	EVT_MENU(MENUID_SEGMENTATION_DELETECURRENTLAYER, SegmentationWindow::OnMenuSegmentationDeleteCurrentLayer)
	EVT_MENU(MENUID_SEGMENTATION_MERGESELECTEDNODES, SegmentationWindow::OnMenuSegmentationMergeSelectedNodes)
	EVT_MENU(MENUID_SEGMENTATION_SEGMENTVOLUME, SegmentationWindow::OnMenuSegmentationSegmentVolume)
	EVT_MENU(MENUID_SEGMENTATION_SPLITNODE_ADDSUBGROUP, SegmentationWindow::OnMenuSegmentationSplitNodeAddSubgroup)
	EVT_MENU(MENUID_SEGMENTATION_SPLITNODE_FINALIZESPLIT, SegmentationWindow::OnMenuSegmentationSplitNodeFinalizeSplit)
	EVT_MENU(MENUID_SEGMENTATION_SPLITNODE_REMOVESUBGROUP, SegmentationWindow::OnMenuSegmentationSplitNodeRemoveSubgroup)
	EVT_MENU(MENUID_SEGMENTATION_SPLITNODE_SETNODE, SegmentationWindow::OnMenuSegmentationSplitNodeSetNode)
	EVT_MENU(MENUID_SEGMENTATION_SPLITNODE_STARTAGAIN, SegmentationWindow::OnMenuSegmentationSplitNodeStartAgain)
	EVT_MENU(MENUID_SEGMENTATION_SWITCHPARENT_SETCHILD, SegmentationWindow::OnMenuSegmentationSwitchParentSetChild)
	EVT_MENU(MENUID_SEGMENTATION_SWITCHPARENT_SETNEWPARENT, SegmentationWindow::OnMenuSegmentationSwitchParentSetNewParent)
	EVT_MENU(MENUID_SEGMENTATION_SWITCHPARENT_STARTAGAIN, SegmentationWindow::OnMenuSegmentationSwitchParentStartAgain)
	EVT_MENU(MENUID_SEGMENTATION_UNZIPSELECTEDNODE, SegmentationWindow::OnMenuSegmentationUnzipSelectedNode)
	EVT_MENU(MENUID_SELECTION_CLEARSELECTION, SegmentationWindow::OnMenuSelectionClearSelection)
	EVT_MENU(MENUID_TOOLS_QUANTIFYFEATUREVOLUMES, SegmentationWindow::OnMenuToolsQuantifyFeatureVolumes)
	EVT_MENU(MENUID_TOOLS_VALIDATEFEATURESELECTION, SegmentationWindow::OnMenuToolsValidateFeatureSelection)
	EVT_MENU(MENUID_TOOLS_VISUALIZEIN3D, SegmentationWindow::OnMenuToolsVisualizeIn3D)

	//~~~~~~~~~~~~~~~~~~~~ UI UPDATES ~~~~~~~~~~~~~~~~~~~~
	EVT_UPDATE_UI(MENUID_ACTIONS_CLEARHISTORY, SegmentationWindow::OnUpdateMenuActionsClearHistory)
	EVT_UPDATE_UI(MENUID_ACTIONS_REDO, SegmentationWindow::OnUpdateMenuActionsRedo)
	EVT_UPDATE_UI(MENUID_ACTIONS_UNDO, SegmentationWindow::OnUpdateMenuActionsUndo)
	EVT_UPDATE_UI(MENUID_FEATURES_AUTOIDENTIFY_MULTIFEATURE3D, SegmentationWindow::OnUpdateForestNeeder)
	EVT_UPDATE_UI(MENUID_FEATURES_AUTOIDENTIFY_SPINE3D, SegmentationWindow::OnUpdateForestNeeder)
	EVT_UPDATE_UI(MENUID_FEATURES_CLEAR_ALL, SegmentationWindow::OnUpdateMenuFeaturesClearAll)
	EVT_UPDATE_UI(MENUID_FEATURES_MANAGEFEATURESELECTIONS, SegmentationWindow::OnUpdateForestNeeder)
	EVT_UPDATE_UI(MENUID_NAVIGATION_NEXTLAYER, SegmentationWindow::OnUpdateMenuNavigationNextLayer)
	EVT_UPDATE_UI(MENUID_NAVIGATION_NEXTSLICE, SegmentationWindow::OnUpdateMenuNavigationNextSlice)
	EVT_UPDATE_UI(MENUID_NAVIGATION_PREVIOUSLAYER, SegmentationWindow::OnUpdateMenuNavigationPreviousLayer)
	EVT_UPDATE_UI(MENUID_NAVIGATION_PREVIOUSSLICE, SegmentationWindow::OnUpdateMenuNavigationPreviousSlice)
	EVT_UPDATE_UI(MENUID_NAVIGATION_ZOOMIN, SegmentationWindow::OnUpdateMenuNavigationZoomIn)
	EVT_UPDATE_UI(MENUID_NAVIGATION_ZOOMOUT, SegmentationWindow::OnUpdateMenuNavigationZoomOut)
	EVT_UPDATE_UI(MENUID_SEGMENTATION_CLONECURRENTLAYER, SegmentationWindow::OnUpdateForestNeeder)
	EVT_UPDATE_UI(MENUID_SEGMENTATION_DELETECURRENTLAYER, SegmentationWindow::OnUpdateMenuSegmentationDeleteCurrentLayer)
	EVT_UPDATE_UI(MENUID_SEGMENTATION_MERGESELECTEDNODES, SegmentationWindow::OnUpdateMenuSegmentationMergeSelectedNodes)
	EVT_UPDATE_UI(MENUID_SEGMENTATION_SPLITNODE_ADDSUBGROUP, SegmentationWindow::OnUpdateMenuSegmentationSplitNodeAddSubgroup)
	EVT_UPDATE_UI(MENUID_SEGMENTATION_SPLITNODE_FINALIZESPLIT, SegmentationWindow::OnUpdateMenuSegmentationSplitNodeFinalizeSplit)
	EVT_UPDATE_UI(MENUID_SEGMENTATION_SPLITNODE_REMOVESUBGROUP, SegmentationWindow::OnUpdateMenuSegmentationSplitNodeRemoveSubgroup)
	EVT_UPDATE_UI(MENUID_SEGMENTATION_SPLITNODE_SETNODE, SegmentationWindow::OnUpdateSingleNonLowestNodeSelectionNeeder)
	EVT_UPDATE_UI(MENUID_SEGMENTATION_SPLITNODE_STARTAGAIN, SegmentationWindow::OnUpdateMenuSegmentationSplitNodeStartAgain)
	EVT_UPDATE_UI(MENUID_SEGMENTATION_SWITCHPARENT_SETCHILD, SegmentationWindow::OnUpdateSingleNonHighestNodeSelectionNeeder)
	EVT_UPDATE_UI(MENUID_SEGMENTATION_SWITCHPARENT_SETNEWPARENT, SegmentationWindow::OnUpdateMenuSegmentationSwitchParentSetNewParent)
	EVT_UPDATE_UI(MENUID_SEGMENTATION_SWITCHPARENT_STARTAGAIN, SegmentationWindow::OnUpdateMenuSegmentationSwitchParentStartAgain)
	EVT_UPDATE_UI(MENUID_SEGMENTATION_UNZIPSELECTEDNODE, SegmentationWindow::OnUpdateSingleNonHighestNodeSelectionNeeder)
	EVT_UPDATE_UI(MENUID_SELECTION_CLEARSELECTION, SegmentationWindow::OnUpdateNonEmptySelectionNeeder)
	EVT_UPDATE_UI(MENUID_TOOLS_QUANTIFYFEATUREVOLUMES, SegmentationWindow::OnUpdateForestNeeder)
	EVT_UPDATE_UI(MENUID_TOOLS_VALIDATEFEATURESELECTION, SegmentationWindow::OnUpdateMenuToolsValidateFeatureSelection)
	EVT_UPDATE_UI(MENUID_TOOLS_VISUALIZEIN3D, SegmentationWindow::OnUpdateForestNeeder)
END_EVENT_TABLE()

}

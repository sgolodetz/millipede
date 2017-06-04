/***
 * millipede: SegmentationWindow.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SEGMENTATIONWINDOW
#define H_MILLIPEDE_SEGMENTATIONWINDOW

#include <millipede/ogl/WrappedGL.h>

#include <wx/frame.h>
#include <wx/glcanvas.h>

#include <millipede/partitionforests/images/AbdominalFeature.h>
#include <millipede/partitionforests/images/DICOMImageBranchLayer.h>
#include <millipede/partitionforests/images/DICOMImageLeafLayer.h>
#include <mast/models/PartitionModel.h>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef boost::shared_ptr<class ICommandManager> ICommandManager_Ptr;
class PartitionView;

class SegmentationWindow : public wxFrame
{
	//#################### TYPEDEFS ####################
private:
	typedef DICOMImageBranchLayer BranchLayer;
	typedef AbdominalFeature::Enum Feature;
	typedef DICOMImageLeafLayer LeafLayer;
	typedef PartitionModel<LeafLayer,BranchLayer,Feature> PartitionModelT;
	typedef boost::shared_ptr<PartitionModelT> PartitionModel_Ptr;
	typedef boost::shared_ptr<const PartitionModelT> PartitionModel_CPtr;

	//#################### PRIVATE VARIABLES ####################
private:
	ICommandManager_Ptr m_commandManager;
	wxMenuBar *m_menuBar;
	PartitionModel_Ptr m_model;
	PartitionView *m_view;

	//#################### CONSTRUCTORS ####################
public:
	SegmentationWindow(wxWindow *parent, const std::string& title, const PartitionModel_Ptr& model, wxGLContext *context = NULL);

	//#################### PUBLIC METHODS ####################
public:
	wxGLContext *get_context() const;

	//#################### PRIVATE METHODS ####################
private:
	void connect_special_menu_items();
	static wxString make_feature_menu_item(const std::vector<Feature>& featureTypes, size_t i, bool useShortcut);
	void setup_gui(wxGLContext *context);
	void setup_menus();

	//#################### EVENT HANDLERS ####################
public:
	//~~~~~~~~~~~~~~~~~~~~ MENUS ~~~~~~~~~~~~~~~~~~~~
	void OnMenuActionsClearHistory(wxCommandEvent&);
	void OnMenuActionsRedo(wxCommandEvent&);
	void OnMenuActionsUndo(wxCommandEvent&);
	void OnMenuFeaturesAutoIdentifyMultiFeature(wxCommandEvent&);
	void OnMenuFeaturesAutoIdentifySpine(wxCommandEvent&);
	void OnMenuFeaturesClearAll(wxCommandEvent&);
	void OnMenuFeaturesClearFeature(wxCommandEvent& e);
	void OnMenuFeaturesIdentify(wxCommandEvent& e);
	void OnMenuFeaturesManageFeatureSelections(wxCommandEvent&);
	void OnMenuFeaturesToggle(wxCommandEvent& e);
	void OnMenuFeaturesUnidentify(wxCommandEvent& e);
	void OnMenuFileExit(wxCommandEvent&);
	void OnMenuHelpContents(wxCommandEvent&);
	void OnMenuNavigationCentreCamera(wxCommandEvent&);
	void OnMenuNavigationFitToView(wxCommandEvent&);
	void OnMenuNavigationGotoSlice(wxCommandEvent&);
	void OnMenuNavigationNextLayer(wxCommandEvent&);
	void OnMenuNavigationNextSlice(wxCommandEvent&);
	void OnMenuNavigationPanDown(wxCommandEvent&);
	void OnMenuNavigationPanLeft(wxCommandEvent&);
	void OnMenuNavigationPanRight(wxCommandEvent&);
	void OnMenuNavigationPanUp(wxCommandEvent&);
	void OnMenuNavigationPreviousLayer(wxCommandEvent&);
	void OnMenuNavigationPreviousSlice(wxCommandEvent&);
	void OnMenuNavigationZoomIn(wxCommandEvent&);
	void OnMenuNavigationZoomOut(wxCommandEvent&);
	void OnMenuSegmentationCloneCurrentLayer(wxCommandEvent&);
	void OnMenuSegmentationDeleteCurrentLayer(wxCommandEvent&);
	void OnMenuSegmentationMergeSelectedNodes(wxCommandEvent&);
	void OnMenuSegmentationSegmentVolume(wxCommandEvent&);
	void OnMenuSegmentationSplitNodeAddSubgroup(wxCommandEvent&);
	void OnMenuSegmentationSplitNodeFinalizeSplit(wxCommandEvent&);
	void OnMenuSegmentationSplitNodeRemoveSubgroup(wxCommandEvent&);
	void OnMenuSegmentationSplitNodeSetNode(wxCommandEvent&);
	void OnMenuSegmentationSplitNodeStartAgain(wxCommandEvent&);
	void OnMenuSegmentationSwitchParentSetChild(wxCommandEvent&);
	void OnMenuSegmentationSwitchParentSetNewParent(wxCommandEvent&);
	void OnMenuSegmentationSwitchParentStartAgain(wxCommandEvent&);
	void OnMenuSegmentationUnzipSelectedNode(wxCommandEvent&);
	void OnMenuSegmentationUnzipSelection(wxCommandEvent&);
	void OnMenuSelectionClearSelection(wxCommandEvent&);
	void OnMenuSelectionSelectMarked(wxCommandEvent& e);
	void OnMenuToolsQuantifyFeatureVolumes(wxCommandEvent&);
	void OnMenuToolsValidateFeatureSelection(wxCommandEvent&);
	void OnMenuToolsVisualizeIn3D(wxCommandEvent&);

	//~~~~~~~~~~~~~~~~~~~~ UI UPDATES ~~~~~~~~~~~~~~~~~~~~
	void OnUpdateMenuActionsClearHistory(wxUpdateUIEvent& e);
	void OnUpdateMenuActionsRedo(wxUpdateUIEvent& e);
	void OnUpdateMenuActionsUndo(wxUpdateUIEvent& e);
	void OnUpdateMenuFeaturesClearAll(wxUpdateUIEvent& e);
	void OnUpdateMenuFeaturesClearFeature(wxUpdateUIEvent& e);
	void OnUpdateMenuNavigationNextLayer(wxUpdateUIEvent& e);
	void OnUpdateMenuNavigationNextSlice(wxUpdateUIEvent& e);
	void OnUpdateMenuNavigationPreviousLayer(wxUpdateUIEvent& e);
	void OnUpdateMenuNavigationPreviousSlice(wxUpdateUIEvent& e);
	void OnUpdateMenuNavigationZoomIn(wxUpdateUIEvent& e);
	void OnUpdateMenuNavigationZoomOut(wxUpdateUIEvent& e);
	void OnUpdateMenuSegmentationDeleteCurrentLayer(wxUpdateUIEvent& e);
	void OnUpdateMenuSegmentationMergeSelectedNodes(wxUpdateUIEvent& e);
	void OnUpdateMenuSegmentationSplitNodeAddSubgroup(wxUpdateUIEvent& e);
	void OnUpdateMenuSegmentationSplitNodeFinalizeSplit(wxUpdateUIEvent& e);
	void OnUpdateMenuSegmentationSplitNodeRemoveSubgroup(wxUpdateUIEvent& e);
	void OnUpdateMenuSegmentationSplitNodeStartAgain(wxUpdateUIEvent& e);
	void OnUpdateMenuSegmentationSwitchParentSetNewParent(wxUpdateUIEvent& e);
	void OnUpdateMenuSegmentationSwitchParentStartAgain(wxUpdateUIEvent& e);
	void OnUpdateMenuSelectionSelectMarked(wxUpdateUIEvent& e);
	void OnUpdateMenuToolsValidateFeatureSelection(wxUpdateUIEvent& e);
	void OnUpdateForestNeeder(wxUpdateUIEvent& e);
	void OnUpdateNonEmptySelectionNeeder(wxUpdateUIEvent& e);
	void OnUpdateSingleNonHighestNodeSelectionNeeder(wxUpdateUIEvent& e);
	void OnUpdateSingleNonLowestNodeSelectionNeeder(wxUpdateUIEvent& e);

	//#################### EVENT TABLE ####################
	DECLARE_EVENT_TABLE()
};

}

#endif

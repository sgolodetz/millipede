/***
 * millipede: PartitionView.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PARTITIONVIEW
#define H_MILLIPEDE_PARTITIONVIEW

#include <boost/shared_ptr.hpp>

#include <common/ogl/WrappedGL.h>

#include <wx/glcanvas.h>
#include <wx/panel.h>

#include <common/partitionforests/images/AbdominalFeature.h>
#include <common/partitionforests/images/DICOMImageBranchLayer.h>
#include <common/partitionforests/images/DICOMImageLeafLayer.h>
#include <common/slices/SliceLocation.h>
#include <common/slices/SliceTextureSet.h>
#include <mast/models/PartitionModel.h>
#include "NodeSplitManager.h"
#include "ParentSwitchManager.h"

//#################### FORWARD DECLARATIONS ####################
class wxButton;
class wxChoice;
class wxSlider;

namespace mp {

class DICOMCanvas;
typedef boost::shared_ptr<class DrawingTool> DrawingTool_Ptr;
typedef boost::shared_ptr<class Job> Job_Ptr;
typedef boost::shared_ptr<class PartitionCamera> PartitionCamera_Ptr;
typedef boost::shared_ptr<const class PartitionCamera> PartitionCamera_CPtr;
class PartitionCanvas;
class PartitionOverlay;
template <typename LeafLayer, typename BranchLayer, typename Feature> class PartitionModel;
typedef boost::shared_ptr<class PartitionOverlayManager> PartitionOverlayManager_Ptr;
typedef boost::shared_ptr<const class PartitionOverlayManager> PartitionOverlayManager_CPtr;

class PartitionView : public wxPanel
{
	//#################### FRIENDS ####################
	friend class BaseCanvas;

	//#################### ENUMERATIONS ####################
private:
	enum DrawingToolType
	{
		DRAWINGTOOL_BOX,
		DRAWINGTOOL_LASSO,
		DRAWINGTOOL_LINELOOP,
		DRAWINGTOOL_COUNT,
	};

	//#################### TYPEDEFS ####################
private:
	typedef DICOMImageBranchLayer BranchLayer;
	typedef AbdominalFeature::Enum Feature;
	typedef DICOMImageLeafLayer LeafLayer;
	typedef NodeSplitManager<LeafLayer,BranchLayer> NodeSplitManagerT;
	typedef ParentSwitchManager<LeafLayer,BranchLayer> ParentSwitchManagerT;
	typedef PartitionModel<LeafLayer,BranchLayer,Feature> PartitionModelT;
	typedef boost::shared_ptr<PartitionModelT> PartitionModel_Ptr;
	typedef boost::shared_ptr<const PartitionModelT> PartitionModel_CPtr;
	typedef VolumeIPF<LeafLayer,BranchLayer> VolumeIPFT;
	typedef boost::shared_ptr<const VolumeIPFT> VolumeIPF_CPtr;
public:
	typedef boost::shared_ptr<NodeSplitManagerT> NodeSplitManager_Ptr;
	typedef boost::shared_ptr<ParentSwitchManagerT> ParentSwitchManager_Ptr;

	//#################### LISTENERS ####################
private:
	struct CameraListener;
	struct ForestTouchListener;
	struct MFSManagerListener;
	struct ModelListener;
	struct MultiFeatureSelectionListener;
	struct NodeSplitManagerListener;
	struct ParentSwitchManagerListener;
	struct SelectionListener;
	struct IPFListListener;

	//#################### PRIVATE VARIABLES ####################
private:
	PartitionCamera_Ptr m_camera;
	int m_canvasWidth, m_canvasHeight;
	ICommandManager_Ptr m_commandManager;
	wxGLContext *m_context;
	std::pair<DrawingToolType,DrawingTool_Ptr> m_currentDrawingTool;
	Greyscale8SliceTextureSet_Ptr m_dicomTextureSet;
	std::pair<DrawingToolType,DrawingTool_Ptr> m_drawingTools[DRAWINGTOOL_COUNT];
	PartitionModel_Ptr m_model;
	boost::shared_ptr<MultiFeatureSelectionListener> m_multiFeatureSelectionListener;
	NodeSplitManager_Ptr m_nodeSplitManager;
	PartitionOverlayManager_Ptr m_overlayManager;
	ParentSwitchManager_Ptr m_parentSwitchManager;
	std::vector<Greyscale8SliceTextureSet_Ptr> m_partitionTextureSets;
	typedef std::vector<Greyscale8SliceTextureSet_Ptr> TexSet;
	std::vector<TexSet> *m_partitionTextureSetCache;
	wxRadioBox * m_ipfchoice;

	// Top left
	wxButton *m_segmentVolumeButton;
	wxPanel *m_topLeftPanel;

	// Top right
	wxChoice *m_multiFeatureSelectionChoice;

	// Middle left
	DICOMCanvas *m_dicomCanvas;

	// Middle
	wxSlider *m_xSlider, *m_ySlider, *m_zSlider, *m_zoomSlider;

	// Middle right
	PartitionCanvas *m_partitionCanvas;
	wxPanel *m_ipfoptions;

	// Bottom left
	wxChoice *m_drawingToolChoice;

	// Bottom right
	wxSlider *m_layerSlider;
	wxPanel *m_segOptions;

	//#################### CONSTRUCTORS ####################
public:
	PartitionView(wxWindow *parent, const PartitionModel_Ptr& model, const ICommandManager_Ptr& commandManager, wxGLContext *context = NULL);

	//#################### PUBLIC METHODS ####################
public:
	const PartitionCamera_Ptr& camera();
	PartitionCamera_CPtr camera() const;
	void clone_current_layer();
	void delete_current_layer();
	void fit_image_to_view();
	wxGLContext *get_context() const;
	void goto_slice();
	void merge_selected_nodes();
	const PartitionModel_Ptr& model();
	PartitionModel_CPtr model() const;
	const NodeSplitManager_Ptr& node_split_manager();
	const ParentSwitchManager_Ptr& parent_switch_manager();
	void unzip_selected_node();

	//#################### PRIVATE METHODS ####################
private:
	void add_listeners();
	void add_mfs_listener();
	void add_ipf_options(unsigned n);
	void calculate_canvas_size();
	void create_dicom_textures();
	void create_overlays();
	void create_partition_textures();
	DrawingTool_Ptr current_drawing_tool();
	Greyscale8SliceTextureSet_CPtr dicom_texture_set() const;
	Job_Ptr fill_dicom_textures_job(SliceOrientation ori, const itk::Image<unsigned char,3>::Pointer& windowedImage) const;
	Job_Ptr fill_partition_textures_job(SliceOrientation ori) const;
	void fill_textures(SliceOrientation ori);
	static SliceLocation initial_slice_location(const DICOMVolumeChoice& volumeChoice);
	PartitionOverlay *multi_feature_selection_overlay() const;
	std::pair<wxArrayString,int> multi_feature_selection_strings() const;
	PartitionOverlay *node_split_overlay() const;
	PartitionOverlayManager_CPtr overlay_manager() const;
	PartitionOverlay *parent_switch_overlay() const;
	Greyscale8SliceTextureSet_CPtr partition_texture_set(int layer) const;
	void recreate_multi_feature_selection_choice();
	void recreate_multi_feature_selection_overlay();
	void recreate_node_split_overlay();
	void recreate_overlays();
	void recreate_parent_switch_overlay();
	void recreate_selection_overlay();
	void refresh_canvases();
	PartitionOverlay *selection_overlay() const;
	void setup_drawing_tools();
	void cache_partition_textures(unsigned i);
	Job_Ptr cache_partition_textures_job(SliceOrientation ori, unsigned i, VolumeIPF_CPtr volumeIPF) const;
	Job_Ptr reload_texture_job(SliceOrientation ori) const;
	void use_partiton_textures(unsigned i);
	void setup_gui(wxGLContext *context);
	void update_sliders();
	const DICOMVolumeChoice& volume_choice() const;
	void zoom_to_fit();

	//#################### EVENT HANDLERS ####################
public:
	//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
	void OnButtonSegmentVolume(wxCommandEvent&);
	void OnButtonViewXY(wxCommandEvent&);
	void OnButtonViewXZ(wxCommandEvent&);
	void OnButtonViewYZ(wxCommandEvent&);
	void OnButtonVisualizeIn3D(wxCommandEvent&);
	void OnButtonDiffUnion(wxCommandEvent&);
	void OnButtonDiffAB(wxCommandEvent&);
	void OnButtonDiffBA(wxCommandEvent&);
	void OnButtonDiffDialog(wxCommandEvent&);
	void OnButtonDiffIntersection(wxCommandEvent&);
	void OnButtonSegmentMultiple(wxCommandEvent&);

	//~~~~~~~~~~~~~~~~~~~~ CHOICES ~~~~~~~~~~~~~~~~~~~~
	void OnChoiceDrawingTool(wxCommandEvent&);
	void OnChoiceMultiFeatureSelection(wxCommandEvent&);
	void OnRadioChooseIPF(wxCommandEvent&);

	//~~~~~~~~~~~~~~~~~~~~ SLIDERS ~~~~~~~~~~~~~~~~~~~~
	void OnSliderX(wxScrollEvent&);
	void OnSliderY(wxScrollEvent&);
	void OnSliderZ(wxScrollEvent&);
	void OnSliderLayer(wxScrollEvent&);
	void OnSliderZoom(wxScrollEvent&);

	//~~~~~~~~~~~~~~~~~~~~ UI UPDATES ~~~~~~~~~~~~~~~~~~~~
	void OnUpdateForestNeeder(wxUpdateUIEvent& e);
	void OnUpdateSliderLayer(wxUpdateUIEvent& e);

	//#################### EVENT TABLE ####################
	DECLARE_EVENT_TABLE()
};

}

#endif

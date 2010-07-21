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

//#################### FORWARD DECLARATIONS ####################
class wxButton;
class wxSlider;

namespace mp {

class DICOMCanvas;
typedef boost::shared_ptr<class Job> Job_Ptr;
typedef boost::shared_ptr<class PartitionCamera> PartitionCamera_Ptr;
typedef boost::shared_ptr<const class PartitionCamera> PartitionCamera_CPtr;
class PartitionCanvas;
class PartitionOverlay;
typedef boost::shared_ptr<class PartitionOverlayManager> PartitionOverlayManager_Ptr;
typedef boost::shared_ptr<const class PartitionOverlayManager> PartitionOverlayManager_CPtr;

class PartitionView : public wxPanel
{
	//#################### FRIENDS ####################
	friend class BaseCanvas;

	//#################### TYPEDEFS ####################
private:
	typedef PartitionModel<DICOMImageLeafLayer,DICOMImageBranchLayer,AbdominalFeature::Enum> PartitionModelT;
	typedef boost::shared_ptr<PartitionModelT> PartitionModel_Ptr;
	typedef boost::shared_ptr<const PartitionModelT> PartitionModel_CPtr;

	//#################### LISTENERS ####################
private:
	struct CameraListener;
	struct ModelListener;
	struct MultiFeatureSelectionListener;
	struct SelectionListener;

	//#################### PRIVATE VARIABLES ####################
private:
	PartitionCamera_Ptr m_camera;
	int m_canvasWidth, m_canvasHeight;
	ICommandManager_Ptr m_commandManager;
	wxGLContext *m_context;
	Greyscale8SliceTextureSet_Ptr m_dicomTextureSet;
	PartitionModel_Ptr m_model;
	PartitionOverlayManager_Ptr m_overlayManager;
	std::vector<Greyscale8SliceTextureSet_Ptr> m_partitionTextureSets;

	// Top right
	wxButton *m_segmentVolumeButton;

	// Middle left
	DICOMCanvas *m_dicomCanvas;

	// Middle
	wxSlider *m_xSlider, *m_ySlider, *m_zSlider, *m_zoomSlider;

	// Middle right
	PartitionCanvas *m_partitionCanvas;

	// Bottom right
	wxSlider *m_layerSlider;

	//#################### CONSTRUCTORS ####################
public:
	PartitionView(wxWindow *parent, const PartitionModel_Ptr& model, const ICommandManager_Ptr& commandManager, wxGLContext *context = NULL);

	//#################### PUBLIC METHODS ####################
public:
	const PartitionCamera_Ptr& camera();
	PartitionCamera_CPtr camera() const;
	void fit_image_to_view();
	wxGLContext *get_context() const;
	void goto_slice();
	const PartitionModel_Ptr& model();
	PartitionModel_CPtr model() const;

	//#################### PRIVATE METHODS ####################
private:
	void add_selection_listeners();
	void calculate_canvas_size();
	void create_dicom_textures();
	void create_overlays();
	void create_partition_textures();
	Greyscale8SliceTextureSet_CPtr dicom_texture_set() const;
	Job_Ptr fill_dicom_textures_job(SliceOrientation ori, const itk::Image<unsigned char,3>::Pointer& windowedImage) const;
	Job_Ptr fill_partition_textures_job(SliceOrientation ori) const;
	void fill_textures(SliceOrientation ori);
	static SliceLocation initial_slice_location(const DICOMVolumeChoice& volumeChoice);
	PartitionOverlay *multi_feature_selection_overlay() const;
	PartitionOverlayManager_CPtr overlay_manager() const;
	Greyscale8SliceTextureSet_CPtr partition_texture_set(int layer) const;
	void recreate_multi_feature_selection_overlay();
	void recreate_overlays();
	void recreate_selection_overlay();
	void refresh_canvases();
	PartitionOverlay *selection_overlay() const;
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

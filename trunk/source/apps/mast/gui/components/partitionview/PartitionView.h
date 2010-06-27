/***
 * millipede: PartitionView.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PARTITIONVIEW
#define H_MILLIPEDE_PARTITIONVIEW

#include <boost/optional.hpp>
#include <boost/shared_ptr.hpp>

#include <wx/glcanvas.h>
#include <wx/listctrl.h>
#include <wx/panel.h>
#include <wx/slider.h>

#include <common/dicom/volumes/DICOMVolumeChoice.h>
#include <common/partitionforests/images/AbdominalFeature.h>
#include <common/partitionforests/images/CTImageBranchLayer.h>
#include <common/partitionforests/images/CTImageLeafLayer.h>
#include <common/slices/SliceLocation.h>
#include "PartitionCamera.h"
#include "PartitionModel.h"

//#################### FORWARD DECLARATIONS ####################
class wxButton;

namespace mp {

class DICOMCanvas;
typedef boost::shared_ptr<class Job> Job_Ptr;
class PartitionCanvas;
class PartitionOverlay;
typedef boost::shared_ptr<class PartitionOverlayManager> PartitionOverlayManager_Ptr;
typedef boost::shared_ptr<const class PartitionOverlayManager> PartitionOverlayManager_CPtr;
typedef boost::shared_ptr<class SliceTextureSet> SliceTextureSet_Ptr;
typedef boost::shared_ptr<const class SliceTextureSet> SliceTextureSet_CPtr;

class PartitionView : public wxPanel
{
	//#################### FRIENDS ####################
	friend class BaseCanvas;

	//#################### TYPEDEFS ####################
private:
	typedef PartitionModel<CTImageLeafLayer,CTImageBranchLayer,AbdominalFeature::Enum> PartitionModelT;
	typedef boost::shared_ptr<PartitionModelT> PartitionModel_Ptr;
	typedef boost::shared_ptr<const PartitionModelT> PartitionModel_CPtr;
	typedef PartitionModelT::VolumeIPFSelectionT VolumeIPFSelectionT;

	//#################### LISTENERS ####################
private:
	struct CameraListener;
	struct MultiFeatureSelectionListener;
	struct SelectionListener;

	//#################### PRIVATE VARIABLES ####################
private:
	PartitionCamera_Ptr m_camera;
	int m_canvasWidth, m_canvasHeight;
	ICommandManager_Ptr m_commandManager;
	wxGLContext *m_context;
	SliceTextureSet_Ptr m_dicomTextureSet;
	PartitionModel_Ptr m_model;
	PartitionOverlayManager_Ptr m_overlayManager;
	std::vector<SliceTextureSet_Ptr> m_partitionTextureSets;
	DICOMVolumeChoice m_volumeChoice;

	// Top right
	wxButton *m_segmentVolumeButton;

	// Middle left
	DICOMCanvas *m_dicomCanvas;
	wxSlider *m_xSlider, *m_ySlider, *m_zSlider, *m_zoomSlider;

	// Middle right
	PartitionCanvas *m_partitionCanvas;
	wxSlider *m_layerSlider;

	// Bottom
	wxListCtrl *m_regionInfo;

	//#################### CONSTRUCTORS ####################
public:
	PartitionView(wxWindow *parent, const DICOMVolume_Ptr& volume, const DICOMVolumeChoice& volumeChoice, const ICommandManager_Ptr& commandManager, wxGLContext *context = NULL);

	//#################### PUBLIC METHODS ####################
public:
	const PartitionCamera_Ptr& camera();
	PartitionCamera_CPtr camera() const;
	void fit_image_to_view();
	wxGLContext *get_context() const;
	void goto_slice();
	const PartitionModel_Ptr& model();
	PartitionModel_CPtr model() const;
	void segment_volume();

	//#################### PRIVATE METHODS ####################
private:
	void calculate_canvas_size();
	void create_dicom_textures();
	void create_overlays();
	void create_partition_textures();
	SliceTextureSet_CPtr dicom_texture_set() const;
	Job_Ptr fill_dicom_textures_job(SliceOrientation ori, const itk::Image<unsigned char,3>::Pointer& windowedImage) const;
	Job_Ptr fill_partition_textures_job(SliceOrientation ori) const;
	void fill_textures(SliceOrientation ori);
	PartitionOverlay *multi_feature_selection_overlay() const;
	PartitionOverlayManager_CPtr overlay_manager() const;
	SliceTextureSet_CPtr partition_texture_set(int layer) const;
	void recreate_multi_feature_selection_overlay();
	void recreate_overlays();
	void recreate_selection_overlay();
	void refresh_canvases();
	PartitionOverlay *selection_overlay() const;
	void setup_gui(wxGLContext *context);
	void update_sliders();
	void zoom_to_fit();

	//#################### EVENT HANDLERS ####################
public:
	//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
	void OnButtonSegmentCTVolume(wxCommandEvent&);
	void OnButtonViewXY(wxCommandEvent&);
	void OnButtonViewXZ(wxCommandEvent&);
	void OnButtonViewYZ(wxCommandEvent&);

	//~~~~~~~~~~~~~~~~~~~~ SLIDERS ~~~~~~~~~~~~~~~~~~~~
	void OnSliderX(wxScrollEvent&);
	void OnSliderY(wxScrollEvent&);
	void OnSliderZ(wxScrollEvent&);
	void OnSliderLayer(wxScrollEvent&);
	void OnSliderZoom(wxScrollEvent&);

	//~~~~~~~~~~~~~~~~~~~~ UI UPDATES ~~~~~~~~~~~~~~~~~~~~
	void OnUpdateSliderLayer(wxUpdateUIEvent& e);

	//#################### EVENT TABLE ####################
	DECLARE_EVENT_TABLE()
};

}

#endif

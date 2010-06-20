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
class PartitionCanvas;
typedef boost::shared_ptr<class PartitionOverlayManager> PartitionOverlayManager_Ptr;
typedef boost::shared_ptr<const class PartitionOverlayManager> PartitionOverlayManager_CPtr;

class PartitionView
:	public wxPanel,
	public PartitionCamera::Listener,
	public PartitionModel<CTImageLeafLayer,CTImageBranchLayer,AbdominalFeature>::Listener
{
	//#################### FRIENDS ####################
	friend class BaseCanvas;

	//#################### TYPEDEFS ####################
private:
	typedef PartitionModel<CTImageLeafLayer,CTImageBranchLayer,AbdominalFeature> PartitionModelT;
	typedef boost::shared_ptr<PartitionModelT> PartitionModel_Ptr;
	typedef boost::shared_ptr<const PartitionModelT> PartitionModel_CPtr;

	//#################### PRIVATE VARIABLES ####################
private:
	PartitionCamera_Ptr m_camera;
	int m_canvasWidth, m_canvasHeight;
	ICommandManager_Ptr m_commandManager;
	wxGLContext *m_context;
	PartitionModel_Ptr m_model;
	boost::optional<SliceLocation> m_oldSliceLocation;	// the location which was being viewed before the user started scrolling a slider (empty when not scrolling)
	PartitionOverlayManager_Ptr m_overlayManager;
	DICOMVolumeChoice m_volumeChoice;

	// Top right
	wxButton *m_segmentVolumeButton;

	// Middle left
	DICOMCanvas *m_dicomCanvas;
	wxSlider *m_xSlider, *m_ySlider, *m_zSlider;

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
	void camera_changed();
	wxGLContext *get_context() const;
	PartitionModel_CPtr model() const;
	void model_changed();
	void segment_volume();

	//#################### PRIVATE METHODS ####################
private:
	void calculate_canvas_size();
	bool create_dicom_textures(SliceOrientation ori);
	void create_partition_textures(SliceOrientation ori);
	bool create_textures(SliceOrientation ori);
	PartitionOverlayManager_CPtr overlay_manager() const;
	void recreate_overlays();
	void refresh_canvases();
	void set_slice_location(const SliceLocation& loc);
	void setup_gui(wxGLContext *context);

	//#################### EVENT HANDLERS ####################
public:
	//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
	void OnButtonSegmentCTVolume(wxCommandEvent&);
	void OnButtonViewXY(wxCommandEvent&);
	void OnButtonViewXZ(wxCommandEvent&);
	void OnButtonViewYZ(wxCommandEvent&);

	//~~~~~~~~~~~~~~~~~~~~ SLIDERS ~~~~~~~~~~~~~~~~~~~~
	void OnReleaseSlider(wxScrollEvent&);
	void OnTrackSliderX(wxScrollEvent&);
	void OnTrackSliderY(wxScrollEvent&);
	void OnTrackSliderZ(wxScrollEvent&);
	void OnTrackSliderLayer(wxScrollEvent&);

	//~~~~~~~~~~~~~~~~~~~~ UI UPDATES ~~~~~~~~~~~~~~~~~~~~
	void OnUpdateSliderLayer(wxUpdateUIEvent& e);

	//#################### EVENT TABLE ####################
	DECLARE_EVENT_TABLE()
};

}

#endif

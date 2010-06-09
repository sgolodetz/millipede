/***
 * millipede: PartitionView.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PARTITIONVIEW
#define H_MILLIPEDE_PARTITIONVIEW

#include <boost/optional.hpp>
#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

#include <wx/glcanvas.h>
#include <wx/listctrl.h>
#include <wx/panel.h>
#include <wx/slider.h>

#include <common/dicom/volumes/DICOMVolumeChoice.h>
#include <common/slices/SliceLocation.h>
#include <common/slices/SliceOrientation.h>
#include <mast/models/PartitionModel.h>

//#################### FORWARD DECLARATIONS ####################
class wxButton;

namespace mp {

class DICOMCanvas;
class PartitionCanvas;
typedef shared_ptr<class PartitionOverlayManager> PartitionOverlayManager_Ptr;
typedef shared_ptr<const class PartitionOverlayManager> PartitionOverlayManager_CPtr;

class PartitionView : public wxPanel, public PartitionModel::Listener
{
	//#################### FRIENDS ####################
	friend class BaseCanvas;

	//#################### TYPEDEFS ####################
private:
	typedef shared_ptr<SliceLocation> SliceLocation_Ptr;

	//#################### PRIVATE VARIABLES ####################
private:
	int m_canvasWidth, m_canvasHeight;
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
	PartitionView(wxWindow *parent, const DICOMVolume_Ptr& volume, const DICOMVolumeChoice& volumeChoice, wxGLContext *context = NULL);

	//#################### PUBLIC METHODS ####################
public:
	wxGLContext *get_context() const;
	void model_changed();

	//#################### PRIVATE METHODS ####################
private:
	void calculate_canvas_size();
	bool create_dicom_textures(SliceOrientation ori);
	void create_partition_textures(SliceOrientation ori);
	bool create_textures(SliceOrientation ori);
	PartitionModel_CPtr model() const;
	PartitionOverlayManager_CPtr overlay_manager() const;
	void recreate_overlays();
	void refresh_canvases();
	void setup_gui(wxGLContext *context);

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

	//~~~~~~~~~~~~~~~~~~~~ UI UPDATES ~~~~~~~~~~~~~~~~~~~~
	void OnUpdateSliderLayer(wxUpdateUIEvent& e);

	//#################### EVENT TABLE ####################
	DECLARE_EVENT_TABLE()
};

}

#endif

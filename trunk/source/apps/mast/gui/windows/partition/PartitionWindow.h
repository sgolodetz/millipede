/***
 * millipede: PartitionWindow.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PARTITIONWINDOW
#define H_MILLIPEDE_PARTITIONWINDOW

#include <boost/optional.hpp>
#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

#include <wx/frame.h>
#include <wx/glcanvas.h>
#include <wx/listctrl.h>
#include <wx/slider.h>

#include <common/dicom/volumes/SliceOrientation.h>
#include <common/io/util/VolumeChoice.h>
#include "ViewedVolume.h"
#include "ViewedVolumeListener.h"

namespace mp {

//#################### FORWARD DECLARATIONS ####################
class PartitionCanvas;
class StratumCanvas;
typedef shared_ptr<class Volume> Volume_Ptr;
typedef shared_ptr<class VolumeTextureSet> VolumeTextureSet_Ptr;

class PartitionWindow : public wxFrame, public ViewedVolumeListener
{
	//#################### TYPEDEFS ####################
private:
	typedef ViewedVolume::ViewLocation ViewLocation;
	typedef shared_ptr<ViewLocation> ViewLocation_Ptr;

	//#################### PRIVATE VARIABLES ####################
private:
	int m_canvasWidth, m_canvasHeight;
	wxGLContext *m_context;
	boost::optional<ViewLocation> m_oldViewLocation;	// the location which was being viewed before the user started scrolling a slider (empty when not scrolling)
	ViewedVolume_Ptr m_viewedVolume;
	VolumeChoice m_volumeChoice;

	// Middle left
	StratumCanvas *m_stratumCanvas;
	wxSlider *m_xSlider, *m_ySlider, *m_zSlider;

	// Middle right
	PartitionCanvas *m_partitionCanvas;
	wxSlider *m_layerSlider;

	// Bottom
	wxListCtrl *m_regionInfo;

	//#################### CONSTRUCTORS ####################
public:
	PartitionWindow(wxWindow *parent, const std::string& title, const Volume_Ptr& volume, const VolumeChoice& volumeChoice, wxGLContext *context = NULL);

	//#################### PUBLIC METHODS ####################
public:
	wxGLContext *get_context() const;
	void viewed_volume_changed();

	//#################### PRIVATE METHODS ####################
private:
	void calculate_canvas_size();
	bool create_textures(SliceOrientation ori);
	void refresh_canvases();
	void setup_gui(wxGLContext *context);

	//#################### EVENT HANDLERS ####################
public:
	//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
	void OnButtonViewXY(wxCommandEvent&);
	void OnButtonViewXZ(wxCommandEvent&);
	void OnButtonViewYZ(wxCommandEvent&);

	//~~~~~~~~~~~~~~~~~~~~ SLIDERS ~~~~~~~~~~~~~~~~~~~~
	void OnSliderXTrack(wxScrollEvent&);
	void OnSliderYTrack(wxScrollEvent&);
	void OnSliderZTrack(wxScrollEvent&);
	void OnSliderLayerTrack(wxScrollEvent&);

	//#################### EVENT TABLE ####################
	DECLARE_EVENT_TABLE()
};

}

#endif

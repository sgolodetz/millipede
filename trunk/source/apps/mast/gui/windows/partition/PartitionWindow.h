/***
 * millipede: PartitionWindow.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PARTITIONWINDOW
#define H_MILLIPEDE_PARTITIONWINDOW

#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

#include <wx/frame.h>
#include <wx/glcanvas.h>
#include <wx/listctrl.h>
#include <wx/slider.h>

#include <common/io/util/VolumeChoice.h>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
class PartitionCanvas;
class StratumCanvas;
typedef shared_ptr<struct ViewedVolumeModel> ViewedVolumeModel_Ptr;
typedef shared_ptr<class Volume> Volume_Ptr;
typedef shared_ptr<class VolumeTextureSet> VolumeTextureSet_Ptr;

class PartitionWindow : public wxFrame
{
	//#################### NESTED CLASSES ####################
private:
	struct ViewLocation
	{
		int stratum;
		int layer;

		ViewLocation(int stratum_, int layer_) : stratum(stratum_), layer(layer_) {}
	};

	//#################### TYPEDEFS ####################
private:
	typedef shared_ptr<ViewLocation> ViewLocation_Ptr;

	//#################### PRIVATE VARIABLES ####################
private:
	int m_canvasWidth, m_canvasHeight;
	wxGLContext *m_context;
	ViewedVolumeModel_Ptr m_model;
	ViewLocation m_oldViewLocation;		// the stratum and layer which were being viewed before the user started scrolling the slider (-1 when not scrolling)
	VolumeChoice m_volumeChoice;

	// Middle left
	StratumCanvas *m_stratumCanvas;
	wxSlider *m_xSlider, *m_ySlider, *m_zSlider;

	// Middle right
	PartitionCanvas *m_partitionCanvas;

	// Bottom
	wxListCtrl *m_regionInfo;

	//#################### CONSTRUCTORS ####################
public:
	PartitionWindow(wxWindow *parent, const std::string& title, const Volume_Ptr& volume, const VolumeChoice& volumeChoice, wxGLContext *context = NULL);

	//#################### PUBLIC METHODS ####################
public:
	wxGLContext *get_context() const;

	//#################### PRIVATE METHODS ####################
private:
	void calculate_canvas_size();
	void refresh_canvases();
	void setup_gui(wxGLContext *context);
	void texture_creator_thread();

	//#################### EVENT HANDLERS ####################
public:
	//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
	void OnButtonCreateTextures(wxCommandEvent&);

	//#################### EVENT TABLE ####################
	DECLARE_EVENT_TABLE()
};

}

#endif

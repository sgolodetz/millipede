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

namespace mp {

//#################### FORWARD DECLARATIONS ####################
class PartitionCanvas;
class StratumCanvas;
typedef shared_ptr<class Volume> Volume_Ptr;

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
	ViewLocation m_oldViewLocation;		// the stratum and layer which were being viewed before the user started scrolling the slider (-1 when not scrolling)
	ViewLocation_Ptr m_viewLocation;
	Volume_Ptr m_volume;

	// Top Left
	StratumCanvas *m_stratumCanvas;

	// Top Middle
	wxSlider *m_stratumSlider;

	// Top Right
	PartitionCanvas *m_partitionCanvas;
	wxSlider *m_layerSlider;

	// Bottom
	wxListCtrl *m_regionInfo;

	//#################### CONSTRUCTORS ####################
public:
	PartitionWindow(wxWindow *parent, const std::string& title, const Volume_Ptr& volume, wxGLContext *context = NULL);

	//#################### PUBLIC METHODS ####################
public:
	wxGLContext *get_context() const;

	//#################### PRIVATE METHODS ####################
private:
	void calculate_canvas_size();

	//#################### EVENT HANDLERS ####################
public:
	// TODO

	//#################### EVENT TABLE ####################
	DECLARE_EVENT_TABLE()
};

}

#endif

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

namespace mp {

//#################### FORWARD DECLARATIONS ####################
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

	//#################### PRIVATE VARIABLES ####################
private:
	ViewLocation m_oldViewLocation;		// the stratum and layer which were being viewed before the user started scrolling the slider (-1 when not scrolling)
	ViewLocation m_viewLocation;
	Volume_Ptr m_volume;

	// Top Left
	wxGLCanvas *m_stratumCanvas;

	// Top Middle
	wxSlider *m_stratumSlider;

	// Top Right
	wxGLCanvas *m_partitionCanvas;
	wxSlider *m_layerSlider;

	// Bottom
	wxListCtrl *m_regionInfo;

	//#################### CONSTRUCTORS ####################
public:
	PartitionWindow(wxWindow *parent, const std::string& title, const Volume_Ptr& volume);
};

}

#endif

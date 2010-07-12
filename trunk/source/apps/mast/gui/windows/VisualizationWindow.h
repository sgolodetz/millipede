/***
 * millipede: VisualizationWindow.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VISUALIZATIONWINDOW
#define H_MILLIPEDE_VISUALIZATIONWINDOW

#include <string>

#include <wx/frame.h>

namespace mp {

/**
@brief	A VisualizationWindow is a window in which the user can view a 3D representation
		of the segmentation result.
*/
class VisualizationWindow : public wxFrame
{
	//#################### CONSTRUCTORS ####################
public:
	VisualizationWindow(wxWindow *parent, const std::string& title);

	// TODO
};

}

#endif

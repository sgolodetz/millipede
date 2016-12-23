/***
 * millipede: PartitionCanvas.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PARTITIONCANVAS
#define H_MILLIPEDE_PARTITIONCANVAS

#include "BaseCanvas.h"

namespace mp {

class PartitionCanvas : public BaseCanvas
{
	//#################### CONSTRUCTORS ####################
public:
	PartitionCanvas(wxWindow *parent, wxGLContext *context, int *attribList, wxWindowID id = -1, const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = wxFULL_REPAINT_ON_RESIZE|wxWANTS_CHARS);

	//#################### PRIVATE METHODS ####################
private:
	void render_overlays(double left, double top, double right, double bottom) const;
	SliceTextureSet_CPtr texture_set_to_display() const;

	//#################### EVENT HANDLERS ####################
public:
	//~~~~~~~~~~~~~~~~~~~~ MOUSE ~~~~~~~~~~~~~~~~~~~~
	void OnLeftDown(wxMouseEvent& e);

	//#################### EVENT TABLE ####################
	DECLARE_EVENT_TABLE()
};

}

#endif

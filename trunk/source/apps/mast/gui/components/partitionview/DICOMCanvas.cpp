/***
 * millipede: DICOMCanvas.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "DICOMCanvas.h"

#include <mast/gui/components/partitionview/drawingtools/DrawingTool.h>
#include <mast/gui/overlays/PartitionOverlayManager.h>

namespace mp {

//#################### CONSTRUCTORS ####################
DICOMCanvas::DICOMCanvas(wxWindow *parent, wxGLContext *context, int *attribList, wxWindowID id, const wxPoint& pos, const wxSize& size, long style)
:	BaseCanvas(parent, context, attribList, id, pos, size, style)
{}

//#################### PUBLIC METHODS ####################
void DICOMCanvas::render(wxPaintDC& dc) const
{
	BaseCanvas::render(dc);
	if(current_drawing_tool())
	{
		current_drawing_tool()->render();
	}
}

//#################### PRIVATE METHODS ####################
void DICOMCanvas::render_overlays(double left, double top, double right, double bottom) const
{
	if(overlay_manager())
	{
		overlay_manager()->render_dicom_overlays(left, top, right, bottom);
	}
}

Greyscale8SliceTextureSet_CPtr DICOMCanvas::texture_set_to_display() const
{
	return dicom_texture_set();
}

//#################### EVENT HANDLERS ####################

//~~~~~~~~~~~~~~~~~~~~ MOUSE ~~~~~~~~~~~~~~~~~~~~
void DICOMCanvas::OnLeftDown(wxMouseEvent& e)
{
	if(!model()->volume_ipf() || !current_drawing_tool()) return;

	// TODO
}

//#################### EVENT TABLE ####################
BEGIN_EVENT_TABLE(DICOMCanvas, BaseCanvas)
	//~~~~~~~~~~~~~~~~~~~~ MOUSE ~~~~~~~~~~~~~~~~~~~~
	EVT_LEFT_DOWN(DICOMCanvas::OnLeftDown)
END_EVENT_TABLE()

}

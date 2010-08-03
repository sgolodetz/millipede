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
void DICOMCanvas::OnLeaveWindow(wxMouseEvent& e)
{
	if(!model()->volume_ipf() || !current_drawing_tool()) return;

	current_drawing_tool()->reset();
	Refresh();
}

void DICOMCanvas::OnLeftDown(wxMouseEvent& e)
{
	if(!model()->volume_ipf() || !current_drawing_tool()) return;

	Vector2i p_Pixels(e.GetX(), e.GetY());
	if(within_image_bounds(p_Pixels))
	{
		current_drawing_tool()->mouse_pressed(p_Pixels);
		Refresh();
	}
}

void DICOMCanvas::OnLeftUp(wxMouseEvent& e)
{
	if(!model()->volume_ipf() || !current_drawing_tool()) return;

	if(current_drawing_tool()->has_started())
	{
		std::vector<Vector2i> selectedPixels = current_drawing_tool()->selected_pixels();
		// TODO: Convert the selected pixels to leaf indices.

		if(e.ShiftDown())
		{
			// TODO
		}
		else if(e.ControlDown())
		{
			// TODO
		}
		else
		{
			// TODO
		}

		current_drawing_tool()->reset();
		Refresh();
	}
}

void DICOMCanvas::OnMouseMotion(wxMouseEvent& e)
{
	if(!model()->volume_ipf() || !current_drawing_tool()) return;

	if(e.LeftIsDown())
	{
		if(current_drawing_tool()->has_started())
		{
			Vector2i p_Pixels(e.GetX(), e.GetY());
			p_Pixels = clamp_to_image_bounds(p_Pixels);
			current_drawing_tool()->mouse_dragged(p_Pixels);
			Refresh();
		}
		else OnLeftDown(e);
	}
}

//#################### EVENT TABLE ####################
BEGIN_EVENT_TABLE(DICOMCanvas, BaseCanvas)
	//~~~~~~~~~~~~~~~~~~~~ MOUSE ~~~~~~~~~~~~~~~~~~~~
	EVT_LEAVE_WINDOW(DICOMCanvas::OnLeaveWindow)
	EVT_LEFT_DOWN(DICOMCanvas::OnLeftDown)
	EVT_LEFT_UP(DICOMCanvas::OnLeftUp)
	EVT_MOTION(DICOMCanvas::OnMouseMotion)
END_EVENT_TABLE()

}

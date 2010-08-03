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
		typedef PartitionModelT::VolumeIPFSelectionT VolumeIPFSelectionT;
		typedef PartitionModelT::VolumeIPFSelection_Ptr VolumeIPFSelection_Ptr;
		VolumeIPFSelection_Ptr selectionDiff(new VolumeIPFSelectionT(model()->volume_ipf()));

		std::vector<Vector2i> selectedPixels = current_drawing_tool()->selected_pixels();
		for(size_t i=0, size=selectedPixels.size(); i<size; ++i)
		{
			const Vector2i& p_Pixels = selectedPixels[i];
			Vector3d p_Coords = pixels_to_3d_coords(Vector2d(p_Pixels));
			itk::Index<3> position;
			for(int i=0; i<3; ++i) position[i] = NumericUtil::round_to_nearest<int>(p_Coords[i]);
			selectionDiff->select_node(PFNodeID(0, model()->volume_ipf()->leaf_of_position(position)));
		}

		VolumeIPFSelection_Ptr newSelection;

		if(e.ShiftDown())
		{
			newSelection.reset(new VolumeIPFSelectionT(model()->volume_ipf()));
			newSelection->combine(model()->selection(), selectionDiff);
		}
		else if(e.ControlDown())
		{
			newSelection.reset(new VolumeIPFSelectionT(model()->volume_ipf()));
			newSelection->subtract(model()->selection(), selectionDiff);
		}
		else
		{
			newSelection = selectionDiff;
		}

		model()->selection()->replace_with_selection(newSelection);
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

/***
 * millipede: PartitionCanvas.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "PartitionCanvas.h"

#include <mast/gui/overlays/PartitionOverlayManager.h>
#include "PartitionCamera.h"

namespace mp {

//#################### CONSTRUCTORS ####################
PartitionCanvas::PartitionCanvas(wxWindow *parent, wxGLContext *context, int *attribList, wxWindowID id, const wxPoint& pos, const wxSize& size, long style)
:	BaseCanvas(parent, context, attribList, id, pos, size, style)
{}

//#################### PRIVATE METHODS ####################
void PartitionCanvas::render_overlays(double left, double top, double right, double bottom) const
{
	if(overlay_manager()) overlay_manager()->render_partition_overlays(left, top, right, bottom);
}

SliceTextureSet_CPtr PartitionCanvas::texture_set_to_display() const
{
	if(camera())
	{
		int layer = camera()->slice_location().layer;
		return camera()->partition_texture_set(layer);
	}
	else return SliceTextureSet_CPtr();
}

}

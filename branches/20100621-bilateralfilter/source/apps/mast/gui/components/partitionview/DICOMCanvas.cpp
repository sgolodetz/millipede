/***
 * millipede: DICOMCanvas.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "DICOMCanvas.h"

#include <mast/gui/overlays/PartitionOverlayManager.h>
#include "PartitionCamera.h"

namespace mp {

//#################### CONSTRUCTORS ####################
DICOMCanvas::DICOMCanvas(wxWindow *parent, wxGLContext *context, int *attribList, wxWindowID id, const wxPoint& pos, const wxSize& size, long style)
:	BaseCanvas(parent, context, attribList, id, pos, size, style)
{}

//#################### PRIVATE METHODS ####################
void DICOMCanvas::render_overlays(double left, double top, double right, double bottom) const
{
	if(overlay_manager()) overlay_manager()->render_dicom_overlays(left, top, right, bottom);
}

SliceTextureSet_CPtr DICOMCanvas::texture_set_to_display() const
{
	if(camera()) return camera()->dicom_texture_set();
	else return SliceTextureSet_CPtr();
}

}

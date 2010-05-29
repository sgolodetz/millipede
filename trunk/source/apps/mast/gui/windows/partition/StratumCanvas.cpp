/***
 * millipede: StratumCanvas.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "StratumCanvas.h"

#include <mast/models/PartitionModel.h>

namespace mp {

//#################### CONSTRUCTORS ####################
StratumCanvas::StratumCanvas(wxWindow *parent, wxGLContext *context, int *attribList, wxWindowID id, const wxPoint& pos, const wxSize& size, long style)
:	BaseCanvas(parent, context, attribList, id, pos, size, style)
{}

//#################### PRIVATE METHODS ####################
SliceTextureSet_CPtr StratumCanvas::texture_set_to_display() const
{
	if(m_model) return m_model->dicom_texture_set();
	else return SliceTextureSet_CPtr();
}

}

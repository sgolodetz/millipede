/***
 * millipede: PartitionCanvas.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "PartitionCanvas.h"

#include <mast/models/PartitionModel.h>

namespace mp {

//#################### CONSTRUCTORS ####################
PartitionCanvas::PartitionCanvas(wxWindow *parent, wxGLContext *context, int *attribList, wxWindowID id, const wxPoint& pos, const wxSize& size, long style)
:	BaseCanvas(parent, context, attribList, id, pos, size, style)
{}

//#################### PRIVATE METHODS ####################
SliceTextureSet_CPtr PartitionCanvas::texture_set_to_display() const
{
	if(m_model)
	{
		int layer = m_model->view_location().layer;
		return m_model->partition_texture_set(layer);
	}
	else return SliceTextureSet_CPtr();
}

}

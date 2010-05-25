/***
 * millipede: StratumCanvas.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "StratumCanvas.h"

#include <common/dicom/volumes/VolumeTextureSet.h>
#include <common/exceptions/Exception.h>
#include <mast/models/PartitionModel.h>

namespace mp {

//#################### CONSTRUCTORS ####################
StratumCanvas::StratumCanvas(wxWindow *parent, wxGLContext *context, int *attribList, wxWindowID id, const wxPoint& pos, const wxSize& size, long style)
:	BaseCanvas(parent, context, attribList, id, pos, size, style)
{}

//#################### PRIVATE METHODS ####################
Texture_CPtr StratumCanvas::texture_to_display() const
{
	if(m_model && m_model->volume_texture_set())
	{
		switch(m_model->slice_orientation())
		{
			case ORIENT_XY:		return m_model->volume_texture_set()->texture(ORIENT_XY, m_model->view_location().z);
			case ORIENT_XZ:		return m_model->volume_texture_set()->texture(ORIENT_XZ, m_model->view_location().y);
			case ORIENT_YZ:		return m_model->volume_texture_set()->texture(ORIENT_YZ, m_model->view_location().x);
			default:			throw Exception("Unexpected slice orientation");
		}
	}
	else return Texture_CPtr();
}

}

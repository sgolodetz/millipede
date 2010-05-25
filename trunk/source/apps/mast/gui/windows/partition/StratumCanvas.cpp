/***
 * millipede: StratumCanvas.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "StratumCanvas.h"

#include <common/dicom/volumes/VolumeTextureSet.h>
#include <common/exceptions/Exception.h>
#include "ViewedVolume.h"

namespace mp {

//#################### CONSTRUCTORS ####################
StratumCanvas::StratumCanvas(wxWindow *parent, wxGLContext *context, int *attribList, wxWindowID id, const wxPoint& pos, const wxSize& size, long style)
:	BaseCanvas(parent, context, attribList, id, pos, size, style)
{}

//#################### PRIVATE METHODS ####################
Texture_CPtr StratumCanvas::texture_to_display() const
{
	if(m_viewedVolume && m_viewedVolume->volume_texture_set())
	{
		switch(m_viewedVolume->slice_orientation())
		{
			case ORIENT_XY:		return m_viewedVolume->volume_texture_set()->texture(ORIENT_XY, m_viewedVolume->view_location().z);
			case ORIENT_XZ:		return m_viewedVolume->volume_texture_set()->texture(ORIENT_XZ, m_viewedVolume->view_location().y);
			case ORIENT_YZ:		return m_viewedVolume->volume_texture_set()->texture(ORIENT_YZ, m_viewedVolume->view_location().x);
			default:			throw Exception("Unexpected slice orientation");
		}
	}
	else return Texture_CPtr();
}

}

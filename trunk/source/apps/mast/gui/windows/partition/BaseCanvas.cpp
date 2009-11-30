/***
 * millipede: BaseCanvas.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "BaseCanvas.h"

#include <common/dicom/volumes/VolumeTextureSet.h>
#include <common/exceptions/Exception.h>
#include <common/textures/Texture.h>
#include "ViewedVolume.h"

namespace mp {

//#################### CONSTRUCTORS ####################
BaseCanvas::BaseCanvas(wxWindow *parent, wxGLContext *context, int *attribList, wxWindowID id, const wxPoint& pos, const wxSize& size, long style)
:	Canvas(parent, context, attribList, id, pos, size, style)
{}

//#################### PUBLIC METHODS ####################
void BaseCanvas::render(wxPaintDC& dc) const
{
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();

	glTranslated(0, 0, -256);

	glPushAttrib(GL_ENABLE_BIT);

	if(m_viewedVolume && m_viewedVolume->volume_texture_set())
	{
		// TEMPORARY
		Texture_CPtr texture;
		switch(m_viewedVolume->view_orientation())
		{
		case ViewedVolume::ORIENT_XY:
			texture = m_viewedVolume->volume_texture_set()->xy_texture(m_viewedVolume->view_location().z);
			break;
		case ViewedVolume::ORIENT_XZ:
			texture = m_viewedVolume->volume_texture_set()->xz_texture(m_viewedVolume->view_location().y);
			break;
		case ViewedVolume::ORIENT_YZ:
			texture = m_viewedVolume->volume_texture_set()->yz_texture(m_viewedVolume->view_location().x);
			break;
		default:
			throw Exception("Unexpected view orientation");
		}

		glEnable(GL_TEXTURE_2D);
		texture->bind();
		glColor3d(1,1,1);
		glBegin(GL_QUADS);
			glTexCoord2d(0,0);	glVertex2d(0,0);
			glTexCoord2d(1,0);	glVertex2d(511,0);
			glTexCoord2d(1,1);	glVertex2d(511,511);
			glTexCoord2d(0,1);	glVertex2d(0,511);
		glEnd();
	}
	else
	{
		// Draw a cross to indicate that it's deliberate that no image is being displayed.
		glColor3d(1,1,1);
		glBegin(GL_LINES);
			glVertex2d(0,0);
			glVertex2d(511,511);
			glVertex2d(511,0);
			glVertex2d(0,511);
		glEnd();
	}

	glPopAttrib();
}

void BaseCanvas::setup(const ViewedVolume_Ptr& viewedVolume)
{
	m_viewedVolume = viewedVolume;

	SetCurrent();

	int width, height;
	GetSize(&width, &height);

	// Enable back-face culling.
	glCullFace(GL_BACK);
	glFrontFace(GL_CW);
	glEnable(GL_CULL_FACE);

	// Set up the z-buffer.
	glDepthFunc(GL_LEQUAL);
	glEnable(GL_DEPTH_TEST);

	// Set up alpha testing.
	glAlphaFunc(GL_NOTEQUAL, 0);
	glEnable(GL_ALPHA_TEST);

	glClearColor(0, 0, 0, 0);

	glViewport(0, 0, width, height);

	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();

	glOrtho(0, width, height, 0, 0.0, 2048.0);
}

}

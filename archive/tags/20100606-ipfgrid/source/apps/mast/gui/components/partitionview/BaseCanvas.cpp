/***
 * millipede: BaseCanvas.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "BaseCanvas.h"

#include <common/exceptions/Exception.h>
#include <common/slices/SliceTextureSet.h>
#include <common/textures/Texture.h>
#include <mast/models/PartitionModel.h>
#include "PartitionView.h"

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

	// Choose an image to render (if available).
	Texture_CPtr texture;
	SliceTextureSet_CPtr textureSet = texture_set_to_display();
	if(textureSet)
	{
		assert(model() != NULL);	// the texture set will have come from the model, so it should be non-null
		switch(model()->slice_orientation())
		{
			case ORIENT_XY:		texture = textureSet->texture(ORIENT_XY, model()->slice_location().z); break;
			case ORIENT_XZ:		texture = textureSet->texture(ORIENT_XZ, model()->slice_location().y); break;
			case ORIENT_YZ:		texture = textureSet->texture(ORIENT_YZ, model()->slice_location().x); break;
			default:			throw Exception("Unexpected slice orientation");
		}
	}

	if(texture)
	{
		// Render the image.
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

	// Render any overlays for this canvas.
	render_overlays(0, 0, 511, 511);
}

void BaseCanvas::setup(const PartitionView *partitionView)
{
	m_partitionView = partitionView;

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

//#################### PROTECTED METHODS ####################
PartitionModel_CPtr BaseCanvas::model() const
{
	return m_partitionView ? m_partitionView->model() : PartitionModel_CPtr();
}

PartitionOverlayManager_CPtr BaseCanvas::overlay_manager() const
{
	return m_partitionView ? m_partitionView->overlay_manager() : PartitionOverlayManager_CPtr();
}

}

/***
 * millipede: MeshCanvas.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "MeshCanvas.h"

#include <GL/glu.h>

#include <common/visualization/MeshRenderer.h>

namespace mp {

//#################### CONSTRUCTORS ####################
MeshCanvas::MeshCanvas(const MeshRenderer_Ptr& meshRenderer, wxWindow *parent, wxGLContext *context, int *attribList, wxWindowID id, const wxPoint& pos,
					   const wxSize& size, long style)
:	Canvas(parent, context, attribList, id, pos, size, style), m_meshRenderer(meshRenderer)
{}

//#################### PUBLIC METHODS ####################
void MeshCanvas::render(wxPaintDC&) const
{
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	// TEMPORARY: Just to demonstrate that the canvas is working.
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
#if 0
	gluLookAt(5,-10,5, 0,0,0, 0,0,1);
#else
	gluLookAt(16,32,-50, 16,16,0, 0,0,-1);
#endif

	glBegin(GL_LINES);
		glColor3d(1,0,0);
		glVertex3d(0,0,0);
		glVertex3d(1,0,0);

		glColor3d(0,1,0);
		glVertex3d(0,0,0);
		glVertex3d(0,1,0);

		glColor3d(0,0,1);
		glVertex3d(0,0,0);
		glVertex3d(0,0,1);
	glEnd();

	m_meshRenderer->render();
}

void MeshCanvas::setup()
{
	SetCurrent();

	// Set up the viewport.
	int width, height;
	GetSize(&width, &height);
	glViewport(0, 0, width, height);

	// Enable back-face culling.
	glCullFace(GL_BACK);
	glFrontFace(GL_CCW);
	glEnable(GL_CULL_FACE);

	// Set up the z-buffer.
	glDepthFunc(GL_LEQUAL);
	glEnable(GL_DEPTH_TEST);

	// Set up the clear colour.
	glClearColor(0.0f, 0.0f, 0.0f, 0.0f);

	// Set up the projection matrix.
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluPerspective(45.0, static_cast<double>(width) / height, 1.0, 4096.0);
}

}

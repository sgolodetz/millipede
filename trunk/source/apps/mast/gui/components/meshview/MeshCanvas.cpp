/***
 * millipede: MeshCanvas.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "MeshCanvas.h"

#include <GL/glu.h>

namespace mp {

//#################### CONSTRUCTORS ####################
MeshCanvas::MeshCanvas(wxWindow *parent, wxGLContext *context, int *attribList, wxWindowID id, const wxPoint& pos, const wxSize& size, long style)
:	Canvas(parent, context, attribList, id, pos, size, style)
{}

//#################### PUBLIC METHODS ####################
void MeshCanvas::render(wxPaintDC&) const
{
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	// TODO
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

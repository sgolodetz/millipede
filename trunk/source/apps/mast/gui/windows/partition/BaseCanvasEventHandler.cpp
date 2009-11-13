/***
 * millipede: BaseCanvasEventHandler.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "BaseCanvasEventHandler.h"

#include <wx/glcanvas.h>

namespace mp {

//#################### PUBLIC METHODS ####################
void BaseCanvasEventHandler::render(wxPaintDC& dc) const
{
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();

	glTranslated(0, 0, -256);

	// Draw a cross to indicate that it's deliberate that no image is being displayed.
	glColor3d(1,1,1);
	glBegin(GL_LINES);
		glVertex2d(0,0);
		glVertex2d(511,511);
		glVertex2d(511,0);
		glVertex2d(0,511);
	glEnd();
}

}

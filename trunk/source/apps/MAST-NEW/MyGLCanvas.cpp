#include "MyGLCanvas.h"

#include <wx/wx.h>

//#################### CONSTRUCTORS ####################
MyGLCanvas::MyGLCanvas(wxWindow *parent, int width, int height, int *attribList)
:	wxGLCanvas(parent, (wxGLCanvas*)NULL, wxID_ANY, wxDefaultPosition, wxSize(width, height), wxFULL_REPAINT_ON_RESIZE, wxGLCanvasName, attribList),
	m_width(width), m_height(height)
{}

//#################### EVENT HANDLERS ####################
void MyGLCanvas::OnPaint(wxPaintEvent& event)
{
	wxPaintDC dc(this);

	SetCurrent();

	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();

	glTranslated(0, 0, -256);

	glColor3d(1.0, 0.0, 0.0);
	glBegin(GL_LINES);
		glVertex2d(10, 10);
		glVertex2d(100, 100);
	glEnd();

	glFlush();
	SwapBuffers();
}

//#################### PUBLIC METHODS ####################
void MyGLCanvas::setup()
{
	SetCurrent();

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

	glViewport(0, 0, m_width, m_height);

	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();

	glOrtho(0, m_width, m_height, 0, 0.0, 2048.0);
}

//#################### EVENT TABLE ####################
BEGIN_EVENT_TABLE(MyGLCanvas, wxGLCanvas)
    EVT_PAINT(MyGLCanvas::OnPaint)
END_EVENT_TABLE()

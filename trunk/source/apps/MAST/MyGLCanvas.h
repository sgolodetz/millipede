#ifndef H_MYGLCANVAS
#define H_MYGLCANVAS

#include <wx/glcanvas.h>

class MyGLCanvas : public wxGLCanvas
{
	//#################### PRIVATE VARIABLES ####################
private:
	int m_height;
	int m_width;

	//#################### CONSTRUCTORS ####################
public:
	MyGLCanvas(wxWindow *parent, int width, int height, int *attribList);

	//#################### EVENT HANDLERS ####################
public:
	void OnPaint(wxPaintEvent& event);

	//#################### PUBLIC METHODS ####################
public:
	void setup();

	//#################### EVENT TABLE ####################
	DECLARE_EVENT_TABLE()
};

#endif

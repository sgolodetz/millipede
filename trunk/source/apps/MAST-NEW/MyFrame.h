#ifndef H_MYFRAME
#define H_MYFRAME

#include <wx/wx.h>

#include "MyGLCanvas.h"

class MyFrame : public wxFrame
{
private:
	MyGLCanvas *m_canvas;
public:
	MyFrame(const wxString& title, int width, int height);

	void setup();
};

#endif

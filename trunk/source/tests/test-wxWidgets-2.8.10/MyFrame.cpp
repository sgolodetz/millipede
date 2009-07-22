#include "MyFrame.h"

#include <wx/spinctrl.h>

MyFrame::MyFrame(const wxString& title, int width, int height)
:	wxFrame(NULL, wxID_ANY, title, wxDefaultPosition, wxDefaultSize)//wxSize(width, height + 35))
{
	int attribList[] =
	{
		WX_GL_RGBA,
		WX_GL_DEPTH_SIZE,
		16,
		WX_GL_DOUBLEBUFFER
	};

	wxBoxSizer *sizer = new wxBoxSizer(wxHORIZONTAL);
	SetSizer(sizer);

	// Setup sidebar.
	wxPanel *sidebar = new wxPanel(this);
	wxBoxSizer *sidebarSizer = new wxBoxSizer(wxVERTICAL);
	sidebar->SetSizer(sidebarSizer);

	wxButton *button1 = new wxButton(sidebar, wxID_OK, wxT("OK"));
	sidebarSizer->Add(button1);
	wxButton *button2 = new wxButton(sidebar, wxID_CANCEL, wxT("Cancel"));
	sidebarSizer->Add(button2);

	// Setup main panel.
	wxPanel *main = new wxPanel(this);
	wxBoxSizer *mainSizer = new wxBoxSizer(wxHORIZONTAL);
	main->SetSizer(mainSizer);

	m_canvas = new MyGLCanvas(main, width, height, attribList);
	mainSizer->Add(m_canvas);
	wxSpinCtrl *spin = new wxSpinCtrl(main, wxID_OK, wxT("1"), wxDefaultPosition, wxDefaultSize, 1, 100, 1);
	mainSizer->Add(spin);

	// Setup menus.
	wxMenu *fileMenu = new wxMenu;
	fileMenu->Append(wxID_EXIT, wxT("E&xit"));

	wxMenuBar *menuBar = new wxMenuBar;
	menuBar->Append(fileMenu, wxT("&File"));

	SetMenuBar(menuBar);

	// Add top-level panels and do size fitting.
	sizer->Add(sidebar);
	sizer->Add(main);
	sizer->Fit(this);
}

void MyFrame::setup()
{
	m_canvas->setup();
}

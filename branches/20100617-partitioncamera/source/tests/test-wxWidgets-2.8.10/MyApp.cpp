#include "MyApp.h"

#include "MyFrame.h"

bool MyApp::OnInit()
{
	MyFrame *frame = new MyFrame(wxT("wxWidgets Test"), 1024, 768);
	frame->Show(true);
	frame->setup();
	return true;
}

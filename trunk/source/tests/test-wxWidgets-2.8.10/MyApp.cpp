#include "MyApp.h"

#ifdef __WXMAC__
	#include <ApplicationServices/ApplicationServices.h>
#endif

#include "MyFrame.h"

bool MyApp::OnInit()
{
#ifdef __WXMAC__
	// Make test-wxWidgets-2.8.10 into a foreground application on Mac OS X.
	ProcessSerialNumber PSN;
	GetCurrentProcess(&PSN);
	TransformProcessType(&PSN, kProcessTransformToForegroundApplication);
#endif

	MyFrame *frame = new MyFrame(wxT("wxWidgets Test"), 1024, 768);
	frame->Show(true);
	frame->setup();
	return true;
}

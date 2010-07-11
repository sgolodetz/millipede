/***
 * mast: Application.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "Application.h"

#ifdef __WXMAC__
	#include <ApplicationServices/ApplicationServices.h>
#endif

#include <mast/gui/windows/MainWindow.h>

namespace mp {

//#################### EVENT HANDLERS ####################
bool Application::OnInit()
{
#ifdef __WXMAC__
	// Make mast into a foreground application on Mac OS X and bring it to the front.
	ProcessSerialNumber PSN;
	GetCurrentProcess(&PSN);
	TransformProcessType(&PSN, kProcessTransformToForegroundApplication);
	SetFrontProcess(&PSN);
#endif

	MainWindow *window = new MainWindow("MAST - Millipede Automatic Segmentation Tool");
	window->Show();
	return true;
}

}

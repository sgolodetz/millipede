/***
 * mast: Application.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "Application.h"

#ifdef __WXMAC__
	#include <ApplicationServices/ApplicationServices.h>
#endif

#include <itkTextOutput.h>

#include <mast/gui/windows/MainWindow.h>
#include <mast/util/HelpController.h>

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

	HelpController::instance().initialize();

	// Send all ITK logging output to the console.
	itk::OutputWindow::SetInstance(itk::TextOutput::New());

	MainWindow *window = new MainWindow("MAST - Millipede Automatic Segmentation Tool");
	window->Show();
	return true;
}

int Application::OnExit()
{
	HelpController::instance().shutdown();
	return 0;
}

}

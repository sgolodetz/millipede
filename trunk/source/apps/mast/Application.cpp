/***
 * mast: Application.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "Application.h"

#include <mast/gui/windows/MainWindow.h>

namespace mp {

//#################### EVENT HANDLERS ####################
bool Application::OnInit()
{
	MainWindow *window = new MainWindow("MAST - Millipede Automatic Segmentation Tool");
	window->Show();
	return true;
}

}

/***
 * mast: MainWindow.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MAST_MAINWINDOW
#define H_MAST_MAINWINDOW

#include <wx/frame.h>

namespace mp {

class MainWindow : public wxFrame
{
	//#################### CONSTRUCTORS ####################
public:
	explicit MainWindow(const std::string& title);

	//#################### PUBLIC METHODS ####################
public:
	void setup();
};

}

#endif

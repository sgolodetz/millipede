/***
 * mast: MainWindow_Menus.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "MainWindow.h"

#include <wx/menu.h>

namespace mp {

//#################### PRIVATE METHODS ####################
void MainWindow::setup_menus()
{
	wxMenu *fileMenu = new wxMenu;
	wxMenu *fileOpenMenu = new wxMenu;
	fileMenu->AppendSubMenu(fileOpenMenu, wxT("&Open"));
		fileOpenMenu->Append(MENUID_FILE_OPEN_DICOMDIR, wxT("&DICOMDIR...\tCtrl+Shift+O"));
		fileOpenMenu->Append(MENUID_FILE_OPEN_MODEL, wxT("&Model...\tCtrl+O"));
		fileOpenMenu->Append(MENUID_FILE_OPEN_VOLUMECHOICE, wxT("&Volume Choice...\tCtrl+Alt+O"));
	wxMenu *fileSaveMenu = new wxMenu;
	fileMenu->AppendSubMenu(fileSaveMenu, wxT("&Save"));
		fileSaveMenu->Append(MENUID_FILE_SAVE_MODEL, wxT("&Model...\tCtrl+S"));
		fileSaveMenu->Append(MENUID_FILE_SAVE_VOLUMECHOICE, wxT("&Volume Choice...\tCtrl+Alt+S"));
	fileMenu->AppendSeparator();
	fileMenu->Append(MENUID_FILE_REPLACE_VOLUMECHOICESECTION, wxT("&Replace Volume Choice Section..."));
	fileMenu->AppendSeparator();
	fileMenu->Append(MENUID_FILE_EXIT, wxT("E&xit\tAlt+F4"));

	wxMenu *editMenu = new wxMenu;
	editMenu->Append(MENUID_EDIT_UNDO, wxT("&Undo\tCtrl+Z"));
	editMenu->Append(MENUID_EDIT_REDO, wxT("&Redo\tCtrl+Y"));
	editMenu->AppendSeparator();
	editMenu->Append(MENUID_EDIT_CLEARUNDOHISTORY, wxT("&Clear Undo History"));

	m_viewMenu = new wxMenu;

	wxMenu *toolsMenu = new wxMenu;
	toolsMenu->Append(MENUID_TOOLS_FEATURESQUANTIFIER, wxT("Features &Quantifier...\tCtrl+Q"));

	wxMenu *helpMenu = new wxMenu;
	helpMenu->Append(MENUID_HELP_ABOUT, wxT("&About...\tF2"));

	m_menuBar = new wxMenuBar;
	m_menuBar->Append(fileMenu, wxT("&File"));
	m_menuBar->Append(editMenu, wxT("&Edit"));
	m_menuBar->Append(m_viewMenu, wxT("&View"));
	m_menuBar->Append(toolsMenu, wxT("&Tools"));
	m_menuBar->Append(helpMenu, wxT("&Help"));

	SetMenuBar(m_menuBar);
}

}

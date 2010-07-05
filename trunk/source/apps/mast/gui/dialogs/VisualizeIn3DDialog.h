/***
 * millipede: VisualizeIn3DDialog.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VISUALIZEIN3DDIALOG
#define H_MILLIPEDE_VISUALIZEIN3DDIALOG

#include <wx/dialog.h>

namespace mp {

class VisualizeIn3DDialog : public wxDialog
{
	//#################### CONSTRUCTORS ####################
public:
	explicit VisualizeIn3DDialog(wxWindow *parent)
	:	wxDialog(parent, wxID_ANY, wxT("Visualize In 3D"), wxDefaultPosition, wxDefaultSize)
	{
		// TODO
	}
};

}

#endif

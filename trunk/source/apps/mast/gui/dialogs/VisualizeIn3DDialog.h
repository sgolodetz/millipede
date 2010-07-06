/***
 * millipede: VisualizeIn3DDialog.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VISUALIZEIN3DDIALOG
#define H_MILLIPEDE_VISUALIZEIN3DDIALOG

#include <wx/dialog.h>

//#################### FORWARD DECLARATIONS ####################
class wxCheckBox;

namespace mp {

class VisualizeIn3DDialog : public wxDialog
{
	//#################### PRIVATE VARIABLES ####################
private:
	wxCheckBox *m_laplacianSmoothingCheckBox;
	wxCheckBox *m_meshDecimationCheckBox;

	//#################### CONSTRUCTORS ####################
public:
	explicit VisualizeIn3DDialog(wxWindow *parent);

	//#################### EVENT HANDLERS ####################
public:
	//~~~~~~~~~~~~~~~~~~~~ UI UPDATES ~~~~~~~~~~~~~~~~~~~~
	void OnUpdateLaplacianSmoothingOption(wxUpdateUIEvent& e);
	void OnUpdateMeshDecimationOption(wxUpdateUIEvent& e);

	//#################### EVENT TABLE ####################
	DECLARE_EVENT_TABLE()
};

}

#endif

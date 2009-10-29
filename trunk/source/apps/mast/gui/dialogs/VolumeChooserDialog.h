/***
 * millipede: VolumeChooserDialog.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VOLUMECHOOSERDIALOG
#define H_MILLIPEDE_VOLUMECHOOSERDIALOG

#include <string>

#include <wx/checkbox.h>
#include <wx/dialog.h>
#include <wx/spinctrl.h>
#include <wx/stattext.h>
#include <wx/treectrl.h>

namespace mp {

class VolumeChooserDialog : public wxDialog
{
	//#################### PRIVATE VARIABLES ####################
private:
	std::string m_filePrefix;

	wxTreeCtrl *m_tree;

	wxStaticText *m_volumeDimensions;
	wxSpinCtrl *m_bounds[6];

	wxCheckBox *m_autoWindowCheckbox;
	wxTextCtrl *m_windowCentre;
	wxTextCtrl *m_windowWidth;

	//#################### CONSTRUCTORS ####################
public:
	explicit VolumeChooserDialog(const std::string& dicomdirFilename);
};

}

#endif

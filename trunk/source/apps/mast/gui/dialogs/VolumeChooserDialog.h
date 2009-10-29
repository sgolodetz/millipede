/***
 * millipede: VolumeChooserDialog.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VOLUMECHOOSERDIALOG
#define H_MILLIPEDE_VOLUMECHOOSERDIALOG

#include <string>

#include <boost/optional.hpp>

#include <wx/checkbox.h>
#include <wx/dialog.h>
#include <wx/spinctrl.h>
#include <wx/stattext.h>
#include <wx/treectrl.h>

#include <common/io/util/VolumeChoice.h>

namespace mp {

class VolumeChooserDialog : public wxDialog
{
	//#################### PRIVATE VARIABLES ####################
private:
	std::string m_filePrefix;
	boost::optional<VolumeChoice> m_volumeChoice;

	wxTreeCtrl *m_tree;

	wxStaticText *m_volumeDimensions;
	wxSpinCtrl *m_bounds[6];
	wxSpinCtrl *&m_minX, *&m_minY, *&m_minZ, *&m_maxX, *&m_maxY, *&m_maxZ;

	wxCheckBox *m_autoWindowCheckbox;
	wxTextCtrl *m_windowCentre;
	wxTextCtrl *m_windowWidth;

	//#################### CONSTRUCTORS ####################
public:
	explicit VolumeChooserDialog(const std::string& dicomdirFilename);

	//#################### PUBLIC METHODS ####################
public:
	// TODO

	//#################### PRIVATE METHODS ####################
private:
	bool construct_volume_choice();

	//#################### EVENT HANDLERS ####################
public:
	//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
	void OnButtonOK(wxCommandEvent&);
	void OnButtonSave(wxCommandEvent&);

	//~~~~~~~~~~~~~~~~~~~~ SPIN CONTROLS ~~~~~~~~~~~~~~~~~~~~
	void OnSpinMaxVolumeSelector(wxSpinEvent&);
	void OnSpinMinVolumeSelector(wxSpinEvent&);

	//~~~~~~~~~~~~~~~~~~~~ TREE CONTROLS ~~~~~~~~~~~~~~~~~~~~
	void OnTreeSelectionChanged(wxTreeEvent&);

	//~~~~~~~~~~~~~~~~~~~~ UI UPDATES ~~~~~~~~~~~~~~~~~~~~
	void OnUpdate(wxUpdateUIEvent& e);
	void OnUpdateWindowControl(wxUpdateUIEvent& e);

	//#################### EVENT TABLE ####################
	DECLARE_EVENT_TABLE()
};

}

#endif

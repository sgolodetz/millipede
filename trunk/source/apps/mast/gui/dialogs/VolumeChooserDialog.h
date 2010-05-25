/***
 * millipede: VolumeChooserDialog.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VOLUMECHOOSERDIALOG
#define H_MILLIPEDE_VOLUMECHOOSERDIALOG

#include <string>

#include <boost/optional.hpp>
#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

#include <wx/checkbox.h>
#include <wx/propdlg.h>
#include <wx/spinctrl.h>
#include <wx/stattext.h>
#include <wx/treectrl.h>

#include <common/io/util/VolumeChoice.h>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef shared_ptr<class DICOMDirectory> DICOMDirectory_Ptr;
typedef shared_ptr<const class DICOMDirectory> DICOMDirectory_CPtr;

class VolumeChooserDialog : public wxPropertySheetDialog
{
	//#################### PRIVATE VARIABLES ####################
private:
	DICOMDirectory_Ptr m_dicomdir;
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
	DICOMDirectory_CPtr dicomdir() const;
	const boost::optional<VolumeChoice>& volume_choice() const;

	//#################### PRIVATE METHODS ####################
private:
	bool construct_volume_choice();
	wxPanel *create_basic_page(wxWindow *parent);
	wxPanel *create_advanced_page(wxWindow *parent);

	//#################### EVENT HANDLERS ####################
public:
	//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
	void OnButtonAboutWindows(wxCommandEvent&);
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

/***
 * millipede: SegmentDICOMVolumeDialog.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_DIFFERENCEDIALOG
#define H_MILLIPEDE_DIFFERENCEDIALOG

#include <wx/textctrl.h>

#include <wx/button.h>
#include <wx/dialog.h>
#include <wx/sizer.h>
#include <wx/combobox.h>
#include <wx/spinctrl.h>
#include <boost/optional.hpp>

#include <vector>

#include <common/segmentation/DICOMSegmentationOptions.h>
#include <mast/models/DifferenceOptions.h>
#include <common/jobs/CompositeJob.h>



namespace mp {
	
namespace mp_differenceDialog {	
	
//#################### ENUMERATIONS ####################
enum
{
	BUTTONID_OK,
	BUTTONID_CANCEL,
	SPINID_IPFA,
	SPINID_IPFB,
	SPINID_LAYERA,
	SPINID_LAYERB,
	CBBID_TYPE,
};

class DifferenceDialog : public wxDialog
{
	//#################### PRIVATE VARIABLES ####################
private:

	DifferenceOptions * m_options;
	
	wxSpinCtrl* m_ipfASpin;
	wxSpinCtrl* m_ipfBSpin;
	wxSpinCtrl* m_layerASpin;
	wxSpinCtrl* m_layerBSpin;
	
	wxComboBox* m_cmbb;
	
	wxWindow* m_parent;
	
	unsigned m_ipfs, m_layerAMax, m_layerBMax;

	//#################### CONSTRUCTORS ####################
public:
	DifferenceDialog(wxWindow *parent, unsigned n):	wxDialog(parent, wxID_ANY, wxT("Select Difference"), wxDefaultPosition, wxDefaultSize)
	{
		
		m_layerAMax = 100;
		m_layerBMax = 100;
		
		wxBoxSizer * sizer = new wxBoxSizer(wxVERTICAL);
		
		sizer->Add(new wxStaticText(this, wxID_ANY, wxT("IPF A: ")), 0, wxALIGN_CENTRE_HORIZONTAL);
		m_ipfASpin = new wxSpinCtrl(this, SPINID_IPFA, wxString::Format(wxT("%i"), 0), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, m_ipfs - 1, 0);
		sizer->Add(m_ipfASpin);
		
		
		sizer->Add(new wxStaticText(this, wxID_ANY, wxT("Layer A: ")), 0, wxALIGN_CENTRE_HORIZONTAL);
		m_layerASpin = new wxSpinCtrl(this, SPINID_LAYERA, wxString::Format(wxT("%i"), m_layerAMax), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, m_layerAMax, m_layerAMax);
		sizer->Add(m_layerASpin);
		
		sizer->Add(new wxStaticText(this, wxID_ANY, wxT("IPF B: ")), 0, wxALIGN_CENTRE_HORIZONTAL);
		m_ipfBSpin = new wxSpinCtrl(this, SPINID_IPFB, wxString::Format(wxT("%i"), 0), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, m_ipfs - 1, 0);
		sizer->Add(m_ipfBSpin);
		
		sizer->Add(new wxStaticText(this, wxID_ANY, wxT("Layer B: ")), 0, wxALIGN_CENTRE_HORIZONTAL);
		m_layerBSpin = new wxSpinCtrl(this, SPINID_LAYERB, wxString::Format(wxT("%i"), m_layerBMax), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, m_layerBMax, m_layerBMax);
		sizer->Add(m_layerBSpin);
		
		sizer->Add(new wxStaticText(this, wxID_ANY, wxT("Type: ")), 0, wxALIGN_CENTRE_HORIZONTAL);
		
		wxString typestrings[3];
		typestrings[0] = "A - B";
		typestrings[1] = "Union";
		typestrings[2] = "Intersection";
		
		m_cmbb = new wxComboBox(this, CBBID_TYPE,  wxT("Choose a type"), wxDefaultPosition, wxDefaultSize, 3, typestrings);
		sizer->Add(m_cmbb);
		
		sizer->Add(new wxButton(this, BUTTONID_OK, wxT("OK")));
		sizer->Add(new wxButton(this, BUTTONID_CANCEL, wxT("Cancel")));
		m_options = new DifferenceOptions();
	
	}
	
	//#################### PUBLIC METHODS ###################
	DifferenceOptions* get_options() {
		return m_options;
	}
	
	void construct_options() {
		m_options->ipfA = m_ipfASpin->GetValue();
		m_options->ipfB = m_ipfBSpin->GetValue();
		m_options->layerA = m_layerASpin->GetValue();
		m_options->layerB = m_layerBSpin->GetValue();
		m_options->type = DifferenceOptions::DifferenceType(m_cmbb->GetSelection());
	}
	
	
	
	void OnOK(wxCommandEvent&) {
		construct_options();
		Close();
	}
	
	void OnCancel(wxCommandEvent&) {
		
		Close();
	}
	
	
	
	
	//#################### EVENT TABLE ####################
	DECLARE_EVENT_TABLE()
	
};

//#################### EVENT TABLE ####################
BEGIN_EVENT_TABLE(DifferenceDialog, wxDialog)
	//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
	EVT_BUTTON(BUTTONID_OK, DifferenceDialog::OnOK)
	EVT_BUTTON(BUTTONID_CANCEL, DifferenceDialog::OnCancel)
END_EVENT_TABLE()

}

using mp_differenceDialog::DifferenceDialog;
}

#endif

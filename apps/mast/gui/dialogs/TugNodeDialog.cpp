/***
 * millipede: TugNodeDialog.cpp
 * Copyright Stuart Golodetz, 2017. All rights reserved.
 ***/

#include "TugNodeDialog.h"

#include <wx/button.h>
#include <wx/panel.h>
#include <wx/radiobox.h>
#include <wx/sizer.h>
#include <wx/spinctrl.h>
#include <wx/stattext.h>

namespace {

//#################### LOCAL CONSTANTS ####################
enum
{
	RADIOBOXID_TUGMODE,
	SPINID_ATOMICLAYER,
	SPINID_TOLAYEROFFSET,
};

}

namespace mp {

//#################### CONSTRUCTORS ####################
TugNodeDialog::TugNodeDialog(wxWindow *parent, int nodeLayer, int atomicLayer, int toLayerOffset,
                             const boost::function<void(int)>& setAtomicLayerHook,
                             const boost::function<void(int)>& setToLayerOffsetHook,
                             const boost::function<void(TugMode)>& setTugModeHook)
:	wxDialog(parent, wxID_ANY, wxT("Tug Selected Node"), wxDefaultPosition, wxDefaultSize),
  m_setAtomicLayerHook(setAtomicLayerHook),
  m_setToLayerOffsetHook(setToLayerOffsetHook),
  m_setTugModeHook(setTugModeHook)
{
	wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
	SetSizer(sizer);

	// Add the options panel.
	wxPanel *optionsPanel = new wxPanel(this);
	wxGridSizer *optionsSizer = new wxGridSizer(0, 2, 5, 5);
	optionsPanel->SetSizer(optionsSizer);

	optionsSizer->Add(new wxStaticText(optionsPanel, wxID_ANY, wxT("Atomic Layer:")), 0, wxALIGN_CENTRE_VERTICAL);
	m_atomicLayer = new wxSpinCtrl(optionsPanel, SPINID_ATOMICLAYER, wxT(""), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 1, nodeLayer - 1, atomicLayer);
	optionsSizer->Add(m_atomicLayer);
	optionsSizer->Add(new wxStaticText(optionsPanel, wxID_ANY, wxT("To Layer Offset:")), 0, wxALIGN_CENTRE_VERTICAL);
	m_toLayerOffset = new wxSpinCtrl(optionsPanel, SPINID_TOLAYEROFFSET, wxT(""), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, -1, 0, toLayerOffset);
	optionsSizer->Add(m_toLayerOffset);
	sizer->Add(optionsPanel, 0, wxALIGN_CENTRE_HORIZONTAL);

	sizer->AddSpacer(10);

	// Set up the radio box to choose a tug mode.
	wxString strings[TUGMODE_COUNT];
	strings[TUGMODE_FAST] = wxT("Use &FastTug");
	strings[TUGMODE_SIMPLE] = wxT("Use &SimpleTug");
	m_tugMode = new wxRadioBox(this, RADIOBOXID_TUGMODE, wxT("Tug Mode"), wxDefaultPosition, wxDefaultSize, TUGMODE_COUNT, strings, 1, wxRA_SPECIFY_COLS);
	m_tugMode->SetSelection(TUGMODE_SIMPLE);
	sizer->Add(m_tugMode, 0, wxALIGN_CENTRE_HORIZONTAL);

	sizer->AddSpacer(10);

	// Add the buttons panel.
	wxPanel *buttonsPanel = new wxPanel(this);
	wxBoxSizer *buttonsSizer = new wxBoxSizer(wxHORIZONTAL);
	buttonsPanel->SetSizer(buttonsSizer);
		wxButton *okButton = new wxButton(buttonsPanel, wxID_OK, wxT("OK"));
		buttonsSizer->Add(okButton);
		okButton->SetFocus();

		wxButton *cancelButton = new wxButton(buttonsPanel, wxID_CANCEL, wxT("Cancel"));
		buttonsSizer->Add(cancelButton);
	sizer->Add(buttonsPanel, 0, wxALIGN_CENTRE_HORIZONTAL);

	sizer->Fit(this);
	CentreOnParent();
}

//#################### EVENT HANDLERS ####################

//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
void TugNodeDialog::OnButtonOK(wxCommandEvent&)
{
	EndModal(wxID_OK);
}

//~~~~~~~~~~~~~~~~~~~~ RADIO BOXES ~~~~~~~~~~~~~~~~~~~~
void TugNodeDialog::OnRadioBoxTugMode(wxCommandEvent&)
{
	m_setTugModeHook(static_cast<TugMode>(m_tugMode->GetSelection()));
}

//~~~~~~~~~~~~~~~~~~~~ SPIN CONTROLS ~~~~~~~~~~~~~~~~~~~~
void TugNodeDialog::OnSpinAtomicLayer(wxSpinEvent&)
{
	m_setAtomicLayerHook(m_atomicLayer->GetValue());
}

void TugNodeDialog::OnSpinToLayerOffset(wxSpinEvent&)
{
	m_setToLayerOffsetHook(m_toLayerOffset->GetValue());
}

//#################### EVENT TABLE ####################
BEGIN_EVENT_TABLE(TugNodeDialog, wxDialog)
	//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
	EVT_BUTTON(wxID_OK, TugNodeDialog::OnButtonOK)

	//~~~~~~~~~~~~~~~~~~~~ RADIO BOXES ~~~~~~~~~~~~~~~~~~~~
	EVT_RADIOBOX(RADIOBOXID_TUGMODE, TugNodeDialog::OnRadioBoxTugMode)

	//~~~~~~~~~~~~~~~~~~~~ SPIN CONTROLS ~~~~~~~~~~~~~~~~~~~~
	EVT_SPINCTRL(SPINID_ATOMICLAYER, TugNodeDialog::OnSpinAtomicLayer)
	EVT_SPINCTRL(SPINID_TOLAYEROFFSET, TugNodeDialog::OnSpinToLayerOffset)
END_EVENT_TABLE()

}

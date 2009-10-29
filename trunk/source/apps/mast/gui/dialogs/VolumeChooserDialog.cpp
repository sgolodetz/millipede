/***
 * millipede: VolumeChooserDialog.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "VolumeChooserDialog.h"

#include <wx/button.h>
#include <wx/sizer.h>

#include <common/dicom/directories/PatientRecord.h>
#include <common/dicom/directories/SeriesRecord.h>
#include <common/dicom/directories/StudyRecord.h>
#include <common/io/files/DICOMDIRFile.h>
#include <mast/util/StringConversion.h>

namespace mp {

//#################### LOCAL CONSTANTS ####################
enum
{
	BUTTONID_SAVE,
	CHECKBOXID_AUTOWINDOW,
	SPINID_MAXX,
	SPINID_MAXY,
	SPINID_MAXZ,
	SPINID_MINX,
	SPINID_MINY,
	SPINID_MINZ,
	TEXTID_WINDOWCENTRE,
	TEXTID_WINDOWWIDTH,
};

//#################### CONSTRUCTORS ####################
VolumeChooserDialog::VolumeChooserDialog(const std::string& dicomdirFilename)
:	wxDialog(NULL, wxID_ANY, wxT("Volume Chooser"), wxDefaultPosition, wxDefaultSize)
{
	m_filePrefix = dicomdirFilename.substr(0, dicomdirFilename.length()-8);		// the prefix is the DICOMDIR path without 'DICOMDIR' on the end of it

	wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
	SetSizer(sizer);

	// Set up the tree control.
	m_tree = new wxTreeCtrl(this, wxID_ANY, wxDefaultPosition, wxSize(400,400), wxTR_HAS_BUTTONS|wxTR_SINGLE);
	sizer->Add(m_tree, 0, wxALIGN_CENTER_HORIZONTAL);
	wxTreeItemId rootID = m_tree->AddRoot(wxT("DICOMDIR"));

	// Load the DICOMDIR.
	DICOMDirectory dicomdir = DICOMDIRFile::load(dicomdirFilename);

	// Add the DICOMDIR data to the tree control.
	const std::vector<PatientRecord_CPtr> patientRecords = dicomdir.patient_records();
	for(size_t i=0, patientCount=patientRecords.size(); i<patientCount; ++i)
	{
		wxTreeItemId patientID = m_tree->AppendItem(rootID, string_to_wxString(patientRecords[i]->patients_name()));
		const std::map<std::string,StudyRecord_CPtr>& studyRecords = patientRecords[i]->study_records().base();
		for(std::map<std::string,StudyRecord_CPtr>::const_iterator jt=studyRecords.begin(), jend=studyRecords.end(); jt!=jend; ++jt)
		{
			wxTreeItemId studyID = m_tree->AppendItem(patientID, string_to_wxString(jt->second->key()));
			const std::map<std::string,SeriesRecord_CPtr>& seriesRecords = jt->second->series_records().base();
			for(std::map<std::string,SeriesRecord_CPtr>::const_iterator kt=seriesRecords.begin(), kend=seriesRecords.end(); kt!=kend; ++kt)
			{
				m_tree->AppendItem(studyID, string_to_wxString("Series " + kt->second->series_number()), -1, -1/*, new SeriesData(series[k]->slice_count())*/);
			}
		}
	}

	m_tree->Expand(rootID);

	// Set up the volume selectors.
	wxPanel *volumeSelectors = new wxPanel(this);
	wxGridSizer *volumeSelectorsSizer = new wxGridSizer(0, 2, 0, 0);
	volumeSelectors->SetSizer(volumeSelectorsSizer);
		wxStaticText *volumeDimensionsLabel = new wxStaticText(volumeSelectors, wxID_ANY, wxT("Volume Dimensions:"));
		volumeSelectorsSizer->Add(volumeDimensionsLabel);
		m_volumeDimensions = new wxStaticText(volumeSelectors, wxID_ANY, wxT(""));
		volumeSelectorsSizer->Add(m_volumeDimensions);

		wxString captions[] = {"Min X:", "Min Y:", "Min Z:", "Max X:", "Max Y:", "Max Z:"};
		int ids[] = {SPINID_MINX, SPINID_MINY, SPINID_MINZ, SPINID_MAXX, SPINID_MAXY, SPINID_MAXZ};
		for(int i=0; i<6; ++i)
		{
			volumeSelectorsSizer->Add(new wxStaticText(volumeSelectors, wxID_ANY, captions[i]));
			m_bounds[i] = new wxSpinCtrl(volumeSelectors, ids[i], wxT("999"), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 999, 999, 999);
			volumeSelectorsSizer->Add(m_bounds[i]);
		}
	sizer->Add(volumeSelectors);

	// Set up the image window settings controls.
	sizer->AddSpacer(10);

	m_autoWindowCheckbox = new wxCheckBox(this, CHECKBOXID_AUTOWINDOW, wxT("Use Automatic Window Settings"));
	m_autoWindowCheckbox->SetValue(true);
	sizer->Add(m_autoWindowCheckbox);

	sizer->AddSpacer(10);

	wxPanel *windowControls = new wxPanel(this);
	wxGridSizer *windowControlsSizer = new wxGridSizer(0, 2, 0, 0);
	windowControls->SetSizer(windowControlsSizer);
		wxStaticText *windowCentreLabel = new wxStaticText(windowControls, wxID_ANY, wxT("Window Centre:"));
		windowControlsSizer->Add(windowCentreLabel);
		m_windowCentre = new wxTextCtrl(windowControls, TEXTID_WINDOWCENTRE);
		windowControlsSizer->Add(m_windowCentre);

		wxStaticText *windowWidthLabel = new wxStaticText(windowControls, wxID_ANY, wxT("Window Width:"));
		windowControlsSizer->Add(windowWidthLabel);
		m_windowWidth = new wxTextCtrl(windowControls, TEXTID_WINDOWWIDTH);
		windowControlsSizer->Add(m_windowWidth);
	sizer->Add(windowControls);

	// Set up the buttons at the bottom of the dialog.
	wxPanel *buttons = new wxPanel(this);
	wxBoxSizer *buttonsSizer = new wxBoxSizer(wxHORIZONTAL);
	buttons->SetSizer(buttonsSizer);
		wxButton *okButton = new wxButton(buttons, wxID_OK, wxT("OK"));
		buttonsSizer->Add(okButton);

		wxButton *cancelButton = new wxButton(buttons, wxID_CANCEL, wxT("Cancel"));
		buttonsSizer->Add(cancelButton);

		wxButton *saveButton = new wxButton(buttons, BUTTONID_SAVE, wxT("Save Volume Choice..."));
		buttonsSizer->Add(saveButton);
	sizer->Add(buttons, 0, wxALIGN_CENTER_HORIZONTAL);

	sizer->Fit(this);
	Centre();
}

}

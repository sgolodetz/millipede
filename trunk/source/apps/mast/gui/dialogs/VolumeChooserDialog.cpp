/***
 * millipede: VolumeChooserDialog.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "VolumeChooserDialog.h"

#include <sstream>

#include <boost/lexical_cast.hpp>
using boost::bad_lexical_cast;
using boost::lexical_cast;

#include <wx/button.h>
#include <wx/filedlg.h>
#include <wx/msgdlg.h>
#include <wx/sizer.h>

#include <common/dicom/directories/PatientRecord.h>
#include <common/dicom/directories/SeriesRecord.h>
#include <common/dicom/directories/StudyRecord.h>
#include <common/io/files/DICOMDIRFile.h>
#include <common/io/files/VolumeChoiceFile.h>
#include <mast/util/StringConversion.h>

namespace mp {

//#################### LOCAL CONSTANTS ####################
enum
{
	BUTTONID_ABOUTWINDOWS,
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

//#################### LOCAL CLASSES ####################
struct SeriesData : wxTreeItemData
{
	SeriesData(int imageWidth_, int imageHeight_, int imageCount_)
	:	imageHeight(imageHeight_), imageWidth(imageWidth_), imageCount(imageCount_)
	{}

	const int imageHeight;
	const int imageWidth;
	const int imageCount;
};

//#################### CONSTRUCTORS ####################
VolumeChooserDialog::VolumeChooserDialog(const std::string& dicomdirFilename)
:	wxDialog(NULL, wxID_ANY, wxT("Volume Chooser"), wxDefaultPosition, wxDefaultSize),
	m_minX(m_bounds[0]), m_minY(m_bounds[1]), m_minZ(m_bounds[2]),
	m_maxX(m_bounds[3]), m_maxY(m_bounds[4]), m_maxZ(m_bounds[5])
{
	m_filePrefix = dicomdirFilename.substr(0, dicomdirFilename.length()-8);		// the prefix is the DICOMDIR path without 'DICOMDIR' on the end of it

	wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
	SetSizer(sizer);

	// Set up the tree control.
	m_tree = new wxTreeCtrl(this, wxID_ANY, wxDefaultPosition, wxSize(400,300), wxTR_HAS_BUTTONS|wxTR_SINGLE);
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
				const SeriesRecord& seriesRecord = *kt->second;
				SeriesData *seriesData = new SeriesData(seriesRecord.image_width(), seriesRecord.image_height(), seriesRecord.image_count());
				m_tree->AppendItem(studyID, string_to_wxString("Series " + kt->second->series_number()), -1, -1, seriesData);
			}
		}
	}

	m_tree->Expand(rootID);

	// Set up the volume selectors.
	wxPanel *volumeSelectors = new wxPanel(this);
	wxGridSizer *volumeSelectorsSizer = new wxGridSizer(0, 2, 0, 0);
	volumeSelectors->SetSizer(volumeSelectorsSizer);
		wxStaticText *volumeDimensionsLabel = new wxStaticText(volumeSelectors, wxID_ANY, wxT("Volume Dimensions:               "));
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

	wxButton *aboutWindowsButton = new wxButton(this, BUTTONID_ABOUTWINDOWS, wxT("About Windows..."));
	sizer->Add(aboutWindowsButton);

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

//#################### PUBLIC METHODS ####################
const boost::optional<VolumeChoice>& VolumeChooserDialog::volume_choice() const
{
	return m_volumeChoice;
}

//#################### PRIVATE METHODS ####################
bool VolumeChooserDialog::construct_volume_choice()
{
	wxTreeItemId seriesID = m_tree->GetSelection();
	wxTreeItemId studyID = m_tree->GetItemParent(seriesID);
	wxTreeItemId patientID = m_tree->GetItemParent(studyID);

	std::string patientKey = wxString_to_string(m_tree->GetItemText(patientID));
	std::string studyKey = wxString_to_string(m_tree->GetItemText(studyID));
	std::string seriesKey = wxString_to_string(m_tree->GetItemText(seriesID));
	seriesKey = seriesKey.substr(7);	// remove 'Series ' from the beginning of the string

	int minX = m_minX->GetValue(), minY = m_minY->GetValue(), maxX = m_maxX->GetValue(), maxY = m_maxY->GetValue();
	int minZ = m_minZ->GetValue() - 1, maxZ = m_maxZ->GetValue() - 1;

	WindowSettings windowSettings;
	if(!m_autoWindowCheckbox->IsChecked())
	{
		try
		{
			double windowCentre, windowWidth;
			windowCentre = lexical_cast<double>(wxString_to_string(m_windowCentre->GetValue()));
			windowWidth = lexical_cast<double>(wxString_to_string(m_windowWidth->GetValue()));
			windowSettings = WindowSettings(windowCentre, windowWidth);
		}
		catch(bad_lexical_cast&)
		{
			wxMessageBox(wxT("Error: The window values must be decimal numbers."), wxT("Error"), wxOK|wxICON_ERROR|wxCENTRE, this);
			return false;
		}
	}

	m_volumeChoice.reset(VolumeChoice(m_filePrefix, patientKey, studyKey, seriesKey, minX, minY, minZ, maxX, maxY, maxZ, windowSettings));

	return true;
}

//#################### EVENT HANDLERS ####################

//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
void VolumeChooserDialog::OnButtonAboutWindows(wxCommandEvent&)
{
	std::ostringstream oss;
	oss	<< "The window centre and width settings effectively control the brightness and contrast of "
		<< "the images. The number of grey values generated by medical scanners is much larger than "
		<< "can be distinguished by the human eye: for instance, CT scanners generate Hounsfield images, "
		<< "whose pixel values can range from -1024 to 3071. There is a need to map these to the smaller "
		<< "number of grey values in the on-screen image (e.g. 256 for an 8-bit greyscale image). The "
		<< "window settings specify how this transformation is done: in particular, they specify a "
		<< "window from centre-width/2 to centre+width/2. Scan values below centre-width/2 map to 0, "
		<< "whilst those above centre+width/2 map to the maximum on-screen image value (e.g. 255). "
		<< "Values in between are interpolated.\n"
		<< '\n'
		<< "Different windows show certain features more or less clearly, so there are well-defined "
		<< "windows for visualizing different types of tissue. For example, to see soft tissue, the "
		<< "'usual' window is centre=40, width=400. The appropriate window to use is often stored "
		<< "with the images when they are viewed by the radiologist, so it's generally wise to use "
		<< "these so-called 'automatic' window settings. The ability to use different windows is "
		<< "sometimes useful, however, and is provided as a convenience.";
	wxMessageBox(string_to_wxString(oss.str()), wxT("About Windows"), wxOK|wxCENTRE, this);
}

void VolumeChooserDialog::OnButtonOK(wxCommandEvent&)
{
	if(construct_volume_choice())
	{
		Close();
	}
}

void VolumeChooserDialog::OnButtonSave(wxCommandEvent&)
{
	wxFileDialog dialog(this, wxT("Save Volume Choice"), wxT("."), wxEmptyString, wxT("Volume Choice Files (*.vcf)|*.vcf"), wxSAVE | wxOVERWRITE_PROMPT);
	if(construct_volume_choice())
	{
		if(dialog.ShowModal() == wxID_OK)
		{
			std::string path = wxString_to_string(dialog.GetPath());
			VolumeChoiceFile::save(path, *m_volumeChoice);
		}

		// It's important to wipe the volume choice after saving it. This is because the code which invokes the dialog
		// loads a volume iff a volume choice has been constructed. Normally it would only be constructed when pressing
		// the OK button, but we need to construct it here in order to save it. However, if we don't wipe it afterwards,
		// then pressing Cancel will erroneously result in loading a volume.
		m_volumeChoice.reset();
	}
}

//~~~~~~~~~~~~~~~~~~~~ SPIN CONTROLS ~~~~~~~~~~~~~~~~~~~~
void VolumeChooserDialog::OnSpinMaxVolumeSelector(wxSpinEvent&)
{
	if(m_minX->GetValue() > m_maxX->GetValue()) m_minX->SetValue(m_maxX->GetValue());
	if(m_minY->GetValue() > m_maxY->GetValue()) m_minY->SetValue(m_maxY->GetValue());
	if(m_minZ->GetValue() > m_maxZ->GetValue()) m_minZ->SetValue(m_maxZ->GetValue());
}

void VolumeChooserDialog::OnSpinMinVolumeSelector(wxSpinEvent&)
{
	if(m_maxX->GetValue() < m_minX->GetValue()) m_maxX->SetValue(m_minX->GetValue());
	if(m_maxY->GetValue() < m_minY->GetValue()) m_maxY->SetValue(m_minY->GetValue());
	if(m_maxZ->GetValue() < m_minZ->GetValue()) m_maxZ->SetValue(m_minZ->GetValue());
}

//~~~~~~~~~~~~~~~~~~~~ TREE CONTROLS ~~~~~~~~~~~~~~~~~~~~
void VolumeChooserDialog::OnTreeSelectionChanged(wxTreeEvent&)
{
	wxTreeItemId selected = m_tree->GetSelection();
	if(selected.IsOk() && !m_tree->ItemHasChildren(selected))
	{
		SeriesData *data = static_cast<SeriesData*>(m_tree->GetItemData(selected));
		const int& imageWidth = data->imageWidth;
		const int& imageHeight = data->imageHeight;
		const int& imageCount = data->imageCount;

		std::ostringstream oss;
		oss << "(0, 0, 1) - (" << imageWidth-1 << ", " << imageHeight-1 << ", " << imageCount << ')';
		m_volumeDimensions->SetLabel(string_to_wxString(oss.str()));

		for(int i=0; i<6; ++i) m_bounds[i]->Enable(true);

		m_minX->SetRange(0, imageWidth-1);		m_minX->SetValue(0);
		m_minY->SetRange(0, imageHeight-1);		m_minY->SetValue(0);
		m_minZ->SetRange(1, imageCount);		m_minZ->SetValue(1);
		m_maxX->SetRange(0, imageWidth-1);		m_maxX->SetValue(imageWidth-1);
		m_maxY->SetRange(0, imageHeight-1);		m_maxY->SetValue(imageHeight-1);
		m_maxZ->SetRange(1, imageCount);		m_maxZ->SetValue(imageCount);
	}
	else
	{
		m_volumeDimensions->SetLabel(wxT(""));
		for(int i=0; i<6; ++i)
		{
			m_bounds[i]->SetRange(999, 999);
			m_bounds[i]->SetValue(999);
			m_bounds[i]->Enable(false);
		}
	}
}

//~~~~~~~~~~~~~~~~~~~~ UI UPDATES ~~~~~~~~~~~~~~~~~~~~
void VolumeChooserDialog::OnUpdate(wxUpdateUIEvent& e)
{
	wxTreeItemId selected = m_tree->GetSelection();
	e.Enable(selected.IsOk() && !m_tree->ItemHasChildren(selected));
}

void VolumeChooserDialog::OnUpdateWindowControl(wxUpdateUIEvent& e)
{
	wxTreeItemId selected = m_tree->GetSelection();
	e.Enable(selected.IsOk() && !m_tree->ItemHasChildren(selected) && !m_autoWindowCheckbox->IsChecked());
}

//#################### EVENT TABLE ####################
BEGIN_EVENT_TABLE(VolumeChooserDialog, wxDialog)
	//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
	EVT_BUTTON(BUTTONID_ABOUTWINDOWS, VolumeChooserDialog::OnButtonAboutWindows)
	EVT_BUTTON(BUTTONID_SAVE, VolumeChooserDialog::OnButtonSave)
	EVT_BUTTON(wxID_OK, VolumeChooserDialog::OnButtonOK)

	//~~~~~~~~~~~~~~~~~~~~ SPIN CONTROLS ~~~~~~~~~~~~~~~~~~~~
	EVT_SPINCTRL(SPINID_MAXX, VolumeChooserDialog::OnSpinMaxVolumeSelector)
	EVT_SPINCTRL(SPINID_MAXY, VolumeChooserDialog::OnSpinMaxVolumeSelector)
	EVT_SPINCTRL(SPINID_MAXZ, VolumeChooserDialog::OnSpinMaxVolumeSelector)
	EVT_SPINCTRL(SPINID_MINX, VolumeChooserDialog::OnSpinMinVolumeSelector)
	EVT_SPINCTRL(SPINID_MINY, VolumeChooserDialog::OnSpinMinVolumeSelector)
	EVT_SPINCTRL(SPINID_MINZ, VolumeChooserDialog::OnSpinMinVolumeSelector)

	//~~~~~~~~~~~~~~~~~~~~ TREE CONTROLS ~~~~~~~~~~~~~~~~~~~~
	EVT_TREE_SEL_CHANGED(wxID_ANY, VolumeChooserDialog::OnTreeSelectionChanged)

	//~~~~~~~~~~~~~~~~~~~~ UI UPDATES ~~~~~~~~~~~~~~~~~~~~
	EVT_UPDATE_UI(BUTTONID_SAVE, VolumeChooserDialog::OnUpdate)
	EVT_UPDATE_UI(CHECKBOXID_AUTOWINDOW, VolumeChooserDialog::OnUpdate)
	EVT_UPDATE_UI(TEXTID_WINDOWCENTRE, VolumeChooserDialog::OnUpdateWindowControl)
	EVT_UPDATE_UI(TEXTID_WINDOWWIDTH, VolumeChooserDialog::OnUpdateWindowControl)
	EVT_UPDATE_UI(wxID_OK, VolumeChooserDialog::OnUpdate)
END_EVENT_TABLE()

}

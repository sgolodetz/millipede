/***
 * millipede: VolumeChooserDialog.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "VolumeChooserDialog.h"

#include <sstream>

#include <boost/lexical_cast.hpp>
using boost::bad_lexical_cast;
using boost::lexical_cast;

#include <wx/bookctrl.h>
#include <wx/button.h>
#include <wx/msgdlg.h>
#include <wx/sizer.h>

#include <common/dicom/directories/PatientRecord.h>
#include <common/dicom/directories/SeriesRecord.h>
#include <common/dicom/directories/StudyRecord.h>
#include <common/io/files/DICOMDIRFile.h>
#include <common/io/files/VolumeChoiceFile.h>
#include <mast/util/StringConversion.h>
#include "DialogUtil.h"

namespace {

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

}

namespace mp {

//#################### CONSTRUCTORS ####################
VolumeChooserDialog::VolumeChooserDialog(const std::string& dicomdirFilename)
:	m_dicomdirFilename(dicomdirFilename),
	m_minX(m_bounds[0]), m_minY(m_bounds[1]), m_minZ(m_bounds[2]),
	m_maxX(m_bounds[3]), m_maxY(m_bounds[4]), m_maxZ(m_bounds[5])
{
	// Load the DICOMDIR.
	m_dicomdir = DICOMDIRFile::load(dicomdirFilename);

	Create(NULL, wxID_ANY, wxT("Volume Chooser"), wxDefaultPosition, wxDefaultSize);

	wxBookCtrlBase *notebook = GetBookCtrl();
	wxPanel *basicPage = create_basic_page(notebook);
	wxPanel *advancedPage = create_advanced_page(notebook);
	notebook->AddPage(basicPage, wxT("Basic"), true);
	notebook->AddPage(advancedPage, wxT("Advanced"), false);

	CreateButtons();
	LayoutDialog();
}

//#################### PUBLIC METHODS ####################
DICOMDirectory_CPtr VolumeChooserDialog::dicomdir() const
{
	return m_dicomdir;
}

const boost::optional<DICOMVolumeChoice>& VolumeChooserDialog::volume_choice() const
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

	m_volumeChoice.reset(DICOMVolumeChoice(m_dicomdirFilename, patientKey, studyKey, seriesKey, minX, minY, minZ, maxX, maxY, maxZ, windowSettings));

	return true;
}

wxPanel *VolumeChooserDialog::create_basic_page(wxWindow *parent)
{
	wxPanel *panel = new wxPanel(parent);

	wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
	panel->SetSizer(sizer);

	wxBoxSizer *innerSizer = new wxBoxSizer(wxVERTICAL);
	sizer->Add(innerSizer, 0, wxALL, 10);

	// Set up the tree control.
	m_tree = new wxTreeCtrl(panel, wxID_ANY, wxDefaultPosition, wxSize(400,300), wxTR_HAS_BUTTONS|wxTR_SINGLE);
	innerSizer->Add(m_tree, 0, wxALIGN_CENTRE_HORIZONTAL);
	wxTreeItemId rootID = m_tree->AddRoot(wxT("DICOMDIR"));

	// Add the DICOMDIR data to the tree control.
	const std::vector<PatientRecord_CPtr> patientRecords = m_dicomdir->patient_records();
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

	sizer->Fit(panel);
	return panel;
}

wxPanel *VolumeChooserDialog::create_advanced_page(wxWindow *parent)
{
	wxPanel *panel = new wxPanel(parent);

	wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
	panel->SetSizer(sizer);

	wxBoxSizer *innerSizer = new wxBoxSizer(wxVERTICAL);
	sizer->Add(innerSizer, 0, wxALL, 10);

	// Set up the volume selectors.
	wxPanel *volumeSelectors = new wxPanel(panel);
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
	innerSizer->Add(volumeSelectors);

	// Set up the image window settings controls.
	innerSizer->AddSpacer(10);

	wxButton *aboutWindowsButton = new wxButton(panel, BUTTONID_ABOUTWINDOWS, wxT("About Windows..."));
	innerSizer->Add(aboutWindowsButton);

	innerSizer->AddSpacer(10);

	m_autoWindowCheckbox = new wxCheckBox(panel, CHECKBOXID_AUTOWINDOW, wxT("Use Automatic Window Settings"));
	m_autoWindowCheckbox->SetValue(true);
	innerSizer->Add(m_autoWindowCheckbox);

	innerSizer->AddSpacer(10);

	wxPanel *windowControls = new wxPanel(panel);
	wxGridSizer *windowControlsSizer = new wxGridSizer(0, 2, 0, 0);
	windowControls->SetSizer(windowControlsSizer);
		wxStaticText *windowCentreLabel = new wxStaticText(windowControls, wxID_ANY, wxT("Window Centre:"));
		windowControlsSizer->Add(windowCentreLabel);
		m_windowCentre = new wxTextCtrl(windowControls, TEXTID_WINDOWCENTRE, wxT("40"));
		windowControlsSizer->Add(m_windowCentre);

		wxStaticText *windowWidthLabel = new wxStaticText(windowControls, wxID_ANY, wxT("Window Width:"));
		windowControlsSizer->Add(windowWidthLabel);
		m_windowWidth = new wxTextCtrl(windowControls, TEXTID_WINDOWWIDTH, wxT("400"));
		windowControlsSizer->Add(m_windowWidth);
	innerSizer->Add(windowControls);

	innerSizer->AddSpacer(10);

	// Add the Save Volume Choice button.
	wxButton *saveButton = new wxButton(panel, BUTTONID_SAVE, wxT("Save Volume Choice..."));
	innerSizer->Add(saveButton);

	sizer->Fit(panel);
	return panel;
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
	wxFileDialog_Ptr dialog = construct_save_dialog(this, "Save Volume Choice", "Volume Choice Files (*.vcf)|*.vcf");
	if(construct_volume_choice())
	{
		if(dialog->ShowModal() == wxID_OK)
		{
			std::string path = wxString_to_string(dialog->GetPath());
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
BEGIN_EVENT_TABLE(VolumeChooserDialog, wxPropertySheetDialog)
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

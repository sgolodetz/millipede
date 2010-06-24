/***
 * millipede: SegmentCTVolumeDialog.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "SegmentCTVolumeDialog.h"

#include <boost/lexical_cast.hpp>

#include <wx/bookctrl.h>
#include <wx/msgdlg.h>
#include <wx/panel.h>
#include <wx/radiobox.h>
#include <wx/sizer.h>
#include <wx/spinctrl.h>
#include <wx/stattext.h>

#include <mast/util/StringConversion.h>

namespace {

//#################### LOCAL CONSTANTS ####################
enum
{
	RADIOBOXID_SEGMENTATIONTYPE,
	SPINID_GRIDSIZE,
};

enum SegmentationType
{
	SEGTYPE_XY,
	SEGTYPE_XZ,
	SEGTYPE_YZ,
	SEGTYPE_3D,
	SEGTYPE_CUSTOM,
	SEGTYPE_COUNT,	// dummy value containing the number of segmentation types
};

}

namespace mp {

//#################### CONSTRUCTORS ####################
SegmentCTVolumeDialog::SegmentCTVolumeDialog(wxWindow *parent, const itk::Size<3>& volumeSize, const WindowSettings& windowSettings)
:	m_volumeSize(volumeSize), m_windowSettings(windowSettings)
{
	Create(parent, wxID_ANY, wxT("Segment CT Volume"), wxDefaultPosition, wxDefaultSize);

	wxBookCtrlBase *notebook = GetBookCtrl();
	wxPanel *basicPage = create_basic_page(notebook);
	wxPanel *advancedPage = create_advanced_page(notebook);
	wxPanel *modalityPage = create_modality_page(notebook);
	notebook->AddPage(basicPage, wxT("Basic"), true);
	notebook->AddPage(advancedPage, wxT("Advanced"), false);
	notebook->AddPage(modalityPage, wxT("Modality-Specific"), false);

	CreateButtons();
	LayoutDialog();
}

//#################### PUBLIC METHODS ####################
const boost::optional<CTSegmentationOptions>& SegmentCTVolumeDialog::segmentation_options() const
{
	return m_segmentationOptions;
}

//#################### PRIVATE METHODS ####################
bool SegmentCTVolumeDialog::construct_segmentation_options()
{
	itk::Size<3> subvolumeSize;
	for(int i=0; i<3; ++i)
	{
		subvolumeSize[i] = m_subvolumeSizes[i]->GetValue();

		if(m_volumeSize[i] % subvolumeSize[i] != 0)
		{
			wxMessageBox(wxT("Error: The subvolume dimensions must be factors of the volume dimensions."), wxT("Error"), wxOK|wxICON_ERROR|wxCENTRE, this);
			return false;
		}
	}
	int adfIterations = m_adfIterations->GetValue();
	CTSegmentationOptions::InputType inputType = CTSegmentationOptions::InputType(m_inputType->GetSelection());
	int waterfallLayerLimit = m_waterfallLayerLimit->GetValue();
	m_segmentationOptions = CTSegmentationOptions(adfIterations, inputType, subvolumeSize, waterfallLayerLimit, m_windowSettings);
	return true;
}

wxPanel *SegmentCTVolumeDialog::create_advanced_page(wxWindow *parent)
{
	wxPanel *panel = new wxPanel(parent);

	wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
	panel->SetSizer(sizer);

	// Set up the spin control to select the number of anisotropic diffusion filtering iterations.
	wxPanel *filteringPanel = new wxPanel(panel);
	sizer->Add(filteringPanel);
	wxGridSizer *filteringSizer = new wxGridSizer(0, 2, 0, 0);
	filteringPanel->SetSizer(filteringSizer);

	filteringSizer->Add(new wxStaticText(filteringPanel, wxID_ANY, wxT("ADF Iterations:")));
	m_adfIterations = new wxSpinCtrl(filteringPanel, wxID_ANY, wxT("30"), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 30, 30);
	filteringSizer->Add(m_adfIterations);

	// Set up the spin control to select a waterfall layer limit.
	wxPanel *waterfallPanel = new wxPanel(panel);
	sizer->Add(waterfallPanel);
	wxGridSizer *waterfallSizer = new wxGridSizer(0, 2, 0, 0);
	waterfallPanel->SetSizer(waterfallSizer);

	waterfallSizer->Add(new wxStaticText(waterfallPanel, wxID_ANY, wxT("Waterfall Layer Limit:")));
	m_waterfallLayerLimit = new wxSpinCtrl(waterfallPanel, wxID_ANY, wxT("5"), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 10, 5);
	waterfallSizer->Add(m_waterfallLayerLimit);

	sizer->Fit(panel);
	return panel;
}

wxPanel *SegmentCTVolumeDialog::create_basic_page(wxWindow *parent)
{
	wxPanel *panel = new wxPanel(parent);

	wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
	panel->SetSizer(sizer);

	// Set up the radio box to choose a segmentation type.
	wxString strings[SEGTYPE_COUNT];
	strings[SEGTYPE_XY] = wxT("Segment as &Axial (X-Y) Slices");
	strings[SEGTYPE_XZ] = wxT("Segment as &Coronal (X-Z) Slices");
	strings[SEGTYPE_YZ] = wxT("Segment as &Sagittal (Y-Z) Slices");
	strings[SEGTYPE_3D] = wxT("Segment as 3D &Volume");
	strings[SEGTYPE_CUSTOM] = wxT("Segment using C&ustomised Sub-Volume Size");
	m_segmentationType = new wxRadioBox(panel, RADIOBOXID_SEGMENTATIONTYPE, wxT("Segmentation Type"), wxDefaultPosition, wxDefaultSize, SEGTYPE_COUNT, strings, 1, wxRA_SPECIFY_COLS);
	sizer->Add(m_segmentationType, 0, wxALIGN_CENTER_HORIZONTAL);

	sizer->AddSpacer(10);

	// Set up the subvolume size options.
	wxStaticBoxSizer *subvolumeSizeOptions = new wxStaticBoxSizer(wxVERTICAL, panel, wxT("Sub-Volume Size"));
	sizer->Add(subvolumeSizeOptions);

	wxPanel *subvolumePanel = new wxPanel(panel);
	subvolumeSizeOptions->Add(subvolumePanel);
	wxGridSizer *subvolumeSizer = new wxGridSizer(0, 2, 0, 0);
	subvolumePanel->SetSizer(subvolumeSizer);

	wxString captions[] = {"X Size:", "Y Size:", "Z Size:"};
	for(int i=0; i<3; ++i)
	{
		subvolumeSizer->Add(new wxStaticText(subvolumePanel, wxID_ANY, captions[i]));
		int initial = i != 2 ? m_volumeSize[i] : 1;
		wxString initialValue = string_to_wxString(boost::lexical_cast<std::string>(initial));
		m_subvolumeSizes[i] = new wxSpinCtrl(subvolumePanel, SPINID_GRIDSIZE, initialValue, wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 1, m_volumeSize[i], initial);
		subvolumeSizer->Add(m_subvolumeSizes[i]);
	}

	sizer->Fit(panel);
	return panel;
}

wxPanel *SegmentCTVolumeDialog::create_modality_page(wxWindow *parent)
{
	wxPanel *panel = new wxPanel(parent);

	wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
	panel->SetSizer(sizer);

	// Set up the radio box to choose between Hounsfield and windowed input.
	wxString strings[CTSegmentationOptions::INPUTTYPE_COUNT];
	strings[CTSegmentationOptions::INPUTTYPE_WINDOWED] = wxT("Use &Windowed Input");
	strings[CTSegmentationOptions::INPUTTYPE_HOUNSFIELD] = wxT("Use &Hounsfield Input");
	m_inputType = new wxRadioBox(panel, wxID_ANY, wxT("Input Type"), wxDefaultPosition, wxDefaultSize, CTSegmentationOptions::INPUTTYPE_COUNT, strings, 1, wxRA_SPECIFY_COLS);
	sizer->Add(m_inputType);

	sizer->Fit(panel);
	return panel;
}

//#################### EVENT HANDLERS ####################

//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
void SegmentCTVolumeDialog::OnButtonOK(wxCommandEvent&)
{
	if(construct_segmentation_options())
	{
		Close();
	}
}

//~~~~~~~~~~~~~~~~~~~~ RADIO BOXES ~~~~~~~~~~~~~~~~~~~~
void SegmentCTVolumeDialog::OnRadioBoxSegmentationType(wxCommandEvent&)
{
	if(m_segmentationType->GetSelection() == SEGTYPE_CUSTOM) return;

	itk::Size<3> subvolumeSize = m_volumeSize;
	switch(m_segmentationType->GetSelection())
	{
		case SEGTYPE_XY:	subvolumeSize[2] = 1; break;
		case SEGTYPE_XZ:	subvolumeSize[1] = 1; break;
		case SEGTYPE_YZ:	subvolumeSize[0] = 1; break;
		default:			break;
	}

	for(int i=0; i<3; ++i) m_subvolumeSizes[i]->SetValue(subvolumeSize[i]);
}

//~~~~~~~~~~~~~~~~~~~~ UI UPDATES ~~~~~~~~~~~~~~~~~~~~
void SegmentCTVolumeDialog::OnUpdateGridSizeControl(wxUpdateUIEvent& e)
{
	// Enable the grid size controls iff custom segmentation is selected.
	e.Enable(m_segmentationType->GetSelection() == SEGTYPE_CUSTOM);
}

//#################### EVENT TABLE ####################
BEGIN_EVENT_TABLE(SegmentCTVolumeDialog, wxPropertySheetDialog)
	//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
	EVT_BUTTON(wxID_OK, SegmentCTVolumeDialog::OnButtonOK)

	//~~~~~~~~~~~~~~~~~~~~ RADIO BOXES ~~~~~~~~~~~~~~~~~~~~
	EVT_RADIOBOX(RADIOBOXID_SEGMENTATIONTYPE, SegmentCTVolumeDialog::OnRadioBoxSegmentationType)

	//~~~~~~~~~~~~~~~~~~~~ UI UPDATES ~~~~~~~~~~~~~~~~~~~~
	EVT_UPDATE_UI(SPINID_GRIDSIZE, SegmentCTVolumeDialog::OnUpdateGridSizeControl)
END_EVENT_TABLE()

}

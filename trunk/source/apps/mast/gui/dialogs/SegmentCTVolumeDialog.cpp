/***
 * millipede: SegmentCTVolumeDialog.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "SegmentCTVolumeDialog.h"

#include <boost/lexical_cast.hpp>

#include <wx/bookctrl.h>
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
SegmentCTVolumeDialog::SegmentCTVolumeDialog(wxWindow *parent, const itk::Size<3>& volumeSize)
:	m_volumeSize(volumeSize)
{
	Create(parent, wxID_ANY, wxT("Segment CT Volume"), wxDefaultPosition, wxDefaultSize);

	wxBookCtrlBase *notebook = GetBookCtrl();
	wxPanel *basicPage = create_basic_page(notebook);
	wxPanel *advancedPage = create_advanced_page(notebook);
	notebook->AddPage(basicPage, wxT("Basic"), true);
	notebook->AddPage(advancedPage, wxT("Advanced"), false);

	CreateButtons();
	LayoutDialog();
}

//#################### PUBLIC METHODS ####################
const boost::optional<CTSegmentationOptions>& SegmentCTVolumeDialog::segmentation_options() const
{
	return m_segmentationOptions;
}

//#################### PRIVATE METHODS ####################
void SegmentCTVolumeDialog::construct_segmentation_options()
{
	itk::Size<3> gridSize;
	for(int i=0; i<3; ++i) gridSize[i] = m_gridSizes[i]->GetValue();
	CTSegmentationOptions::InputType inputType = CTSegmentationOptions::InputType(m_inputType->GetSelection());
	int waterfallLayerLimit = m_waterfallLayerLimit->GetValue();
	m_segmentationOptions = CTSegmentationOptions(gridSize, inputType, waterfallLayerLimit);
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
	strings[SEGTYPE_CUSTOM] = wxT("Segment using Customised &Grid Size");
	m_segmentationType = new wxRadioBox(panel, RADIOBOXID_SEGMENTATIONTYPE, wxT("Segmentation Type"), wxDefaultPosition, wxDefaultSize, SEGTYPE_COUNT, strings, 1, wxRA_SPECIFY_COLS);
	sizer->Add(m_segmentationType, 0, wxALIGN_CENTER_HORIZONTAL);

	sizer->AddSpacer(10);

	// Set up the grid size options.
	wxStaticBoxSizer *gridSizeOptions = new wxStaticBoxSizer(wxVERTICAL, panel, wxT("Grid Size Options"));
	sizer->Add(gridSizeOptions);

	wxPanel *gridPanel = new wxPanel(panel);
	gridSizeOptions->Add(gridPanel);
	wxGridSizer *gridSizer = new wxGridSizer(0, 2, 0, 0);
	gridPanel->SetSizer(gridSizer);

	wxString captions[] = {"X Grid Size:", "Y Grid Size:", "Z Grid Size:"};
	for(int i=0; i<3; ++i)
	{
		gridSizer->Add(new wxStaticText(gridPanel, wxID_ANY, captions[i]));
		int initial = i != 2 ? m_volumeSize[i] : 1;
		wxString initialValue = string_to_wxString(boost::lexical_cast<std::string>(initial));
		m_gridSizes[i] = new wxSpinCtrl(gridPanel, SPINID_GRIDSIZE, initialValue, wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 1, m_volumeSize[i], initial);
		gridSizer->Add(m_gridSizes[i]);
	}

	sizer->Fit(panel);
	return panel;
}

wxPanel *SegmentCTVolumeDialog::create_advanced_page(wxWindow *parent)
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

	sizer->AddSpacer(10);

	// Set up the spin control to select a waterfall layer limit.
	wxPanel *limitPanel = new wxPanel(panel);
	sizer->Add(limitPanel);
	wxGridSizer *limitSizer = new wxGridSizer(0, 2, 0, 0);
	limitPanel->SetSizer(limitSizer);

	limitSizer->Add(new wxStaticText(limitPanel, wxID_ANY, wxT("Waterfall Layer Limit:")));
	m_waterfallLayerLimit = new wxSpinCtrl(limitPanel, wxID_ANY, wxT("4"), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 10, 4);
	limitSizer->Add(m_waterfallLayerLimit);

	sizer->Fit(panel);
	return panel;
}

//#################### EVENT HANDLERS ####################

//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
void SegmentCTVolumeDialog::OnButtonOK(wxCommandEvent&)
{
	construct_segmentation_options();
	Close();
}

//~~~~~~~~~~~~~~~~~~~~ RADIO BOXES ~~~~~~~~~~~~~~~~~~~~
void SegmentCTVolumeDialog::OnRadioBoxSegmentationType(wxCommandEvent&)
{
	if(m_segmentationType->GetSelection() == SEGTYPE_CUSTOM) return;

	itk::Size<3> gridSize = m_volumeSize;
	switch(m_segmentationType->GetSelection())
	{
		case SEGTYPE_XY:	gridSize[2] = 1; break;
		case SEGTYPE_XZ:	gridSize[1] = 1; break;
		case SEGTYPE_YZ:	gridSize[0] = 1; break;
		default:			break;
	}

	for(int i=0; i<3; ++i) m_gridSizes[i]->SetValue(gridSize[i]);
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

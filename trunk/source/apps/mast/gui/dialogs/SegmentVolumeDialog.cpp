/***
 * millipede: SegmentVolumeDialog.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "SegmentVolumeDialog.h"

#include <boost/lexical_cast.hpp>

#include <wx/bookctrl.h>
#include <wx/radiobox.h>
#include <wx/sizer.h>
#include <wx/stattext.h>

#include <mast/util/StringConversion.h>

namespace mp {

//#################### CONSTRUCTORS ####################
SegmentVolumeDialog::SegmentVolumeDialog(wxWindow *parent, const itk::Size<3>& volumeSize)
:	m_volumeSize(volumeSize)
{
	Create(parent, wxID_ANY, wxT("Segment Volume"), wxDefaultPosition, wxDefaultSize);

	wxBookCtrlBase *notebook = GetBookCtrl();
	wxPanel *basicPage = create_basic_page(notebook);
	wxPanel *advancedPage = create_advanced_page(notebook);
	notebook->AddPage(basicPage, wxT("Basic"), true);
	notebook->AddPage(advancedPage, wxT("Advanced"), false);

	CreateButtons();
	LayoutDialog();
}

//#################### PRIVATE METHODS ####################
wxPanel *SegmentVolumeDialog::create_basic_page(wxWindow *parent)
{
	wxPanel *panel = new wxPanel(parent);

	wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
	panel->SetSizer(sizer);

	// Set up the radio box to choose a segmentation type.
	wxArrayString strings;
	strings.Add(wxT("Segment as &Axial (X-Y) Slices"));
	strings.Add(wxT("Segment as &Coronal (X-Z) Slices"));
	strings.Add(wxT("Segment as &Sagittal (Y-Z) Slices"));
	strings.Add(wxT("Segment as 3D &Volume"));
	strings.Add(wxT("Segment using Customised &Grid Size"));
	wxRadioBox *segmentationTypeBox = new wxRadioBox(panel, wxID_ANY, wxT("Segmentation Type"), wxDefaultPosition, wxDefaultSize, strings, 1, wxRA_SPECIFY_COLS);
	sizer->Add(segmentationTypeBox, 0, wxALIGN_CENTER_HORIZONTAL);

	sizer->AddSpacer(10);

	// Set up the grid size options.
	wxStaticBoxSizer *gridSizeOptions = new wxStaticBoxSizer(wxVERTICAL, panel, wxT("Partition Forest Grid Size Options"));
	sizer->Add(gridSizeOptions);

	wxPanel *gridPanel = new wxPanel(panel);
	gridSizeOptions->Add(gridPanel);
	wxGridSizer *gridSizer = new wxGridSizer(0, 2, 0, 0);
	gridPanel->SetSizer(gridSizer);

	wxString captions[] = {"X Grid Size:", "Y Grid Size:", "Z Grid Size:"};
	int ids[] = {wxID_ANY, wxID_ANY, wxID_ANY};
	for(int i=0; i<3; ++i)
	{
		gridSizer->Add(new wxStaticText(gridPanel, wxID_ANY, captions[i]));
		int initial = i != 2 ? m_volumeSize[i] : 1;
		wxString initialValue = string_to_wxString(boost::lexical_cast<std::string>(initial));
		wxSpinCtrl *spin = new wxSpinCtrl(gridPanel, ids[i], initialValue, wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 1, initial, m_volumeSize[i]);
		gridSizer->Add(spin);
	}

	sizer->Fit(panel);
	return panel;
}

wxPanel *SegmentVolumeDialog::create_advanced_page(wxWindow *parent)
{
	wxPanel *panel = new wxPanel(parent);

	wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
	panel->SetSizer(sizer);

	// Set up the radio box to choose between Hounsfield and windowed input.
	wxArrayString strings;
	strings.Add(wxT("Use &Windowed Input"));
	strings.Add(wxT("Use &Hounsfield Input"));
	wxRadioBox *inputTypeBox = new wxRadioBox(panel, wxID_ANY, wxT("Input Type"), wxDefaultPosition, wxDefaultSize, strings, 1, wxRA_SPECIFY_COLS);
	sizer->Add(inputTypeBox);

	sizer->AddSpacer(10);

	// Set up the spin control to select a waterfall layer limit.
	wxPanel *limitPanel = new wxPanel(panel);
	sizer->Add(limitPanel);
	wxGridSizer *limitSizer = new wxGridSizer(0, 2, 0, 0);
	limitPanel->SetSizer(limitSizer);

	limitSizer->Add(new wxStaticText(limitPanel, wxID_ANY, wxT("Waterfall Layer Limit:")));
	wxSpinCtrl *spin = new wxSpinCtrl(limitPanel, wxID_ANY, wxT("4"), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 10, 4);
	limitSizer->Add(spin);

	sizer->Fit(panel);
	return panel;
}

//#################### EVENT TABLE ####################
BEGIN_EVENT_TABLE(SegmentVolumeDialog, wxPropertySheetDialog)
	// TODO
END_EVENT_TABLE()

}

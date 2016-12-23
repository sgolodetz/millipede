/***
 * millipede: SegmentDICOMVolumeDialog.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "SegmentDICOMVolumeDialog.h"

namespace mp {

//#################### CONSTRUCTORS ####################
SegmentDICOMVolumeDialog::SegmentDICOMVolumeDialog(wxWindow *parent, const itk::Size<3>& volumeSize, const WindowSettings& windowSettings)
:	SegmentVolumeDialog<DICOMSegmentationOptions>(volumeSize), m_windowSettings(windowSettings)
{
	initialise(parent, "Segment DICOM Volume");
}

//#################### PRIVATE METHODS ####################
bool SegmentDICOMVolumeDialog::construct_segmentation_options()
{
	itk::Size<3> subvolumeSize;
	if(!construct_subvolume_size(subvolumeSize)) return false;
	DICOMSegmentationOptions::InputType inputType = DICOMSegmentationOptions::InputType(m_inputType->GetSelection());
	m_segmentationOptions = DICOMSegmentationOptions(m_adfIterations->GetValue(), inputType, subvolumeSize, m_waterfallLayerLimit->GetValue(), m_windowSettings);
	return true;
}

wxPanel *SegmentDICOMVolumeDialog::create_advanced_page(wxWindow *parent)
{
	wxPanel *panel = new wxPanel(parent);

	wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
	panel->SetSizer(sizer);

	// Set up the radio box to choose between base and windowed input.
	wxString strings[DICOMSegmentationOptions::INPUTTYPE_COUNT];
	strings[DICOMSegmentationOptions::INPUTTYPE_BASE] = wxT("Use &Base Input");
	strings[DICOMSegmentationOptions::INPUTTYPE_WINDOWED] = wxT("Use &Windowed Input");
	m_inputType = new wxRadioBox(panel, wxID_ANY, wxT("Input Type"), wxDefaultPosition, wxDefaultSize, DICOMSegmentationOptions::INPUTTYPE_COUNT, strings, 1, wxRA_SPECIFY_COLS);
	m_inputType->SetSelection(DICOMSegmentationOptions::INPUTTYPE_WINDOWED);
	sizer->Add(m_inputType);

	sizer->AddSpacer(10);

	// Set up the spin control to select the number of anisotropic diffusion filtering iterations.
	wxPanel *filteringPanel = new wxPanel(panel);
	sizer->Add(filteringPanel);
	wxGridSizer *filteringSizer = new wxGridSizer(0, 2, 0, 0);
	filteringPanel->SetSizer(filteringSizer);

	filteringSizer->Add(new wxStaticText(filteringPanel, wxID_ANY, wxT("ADF Iterations:")));
	m_adfIterations = new wxSpinCtrl(filteringPanel, wxID_ANY, wxT("20"), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 30, 20);
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

}

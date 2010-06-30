/***
 * millipede: SegmentCTVolumeDialog.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "SegmentCTVolumeDialog.h"

namespace mp {

//#################### CONSTRUCTORS ####################
SegmentCTVolumeDialog::SegmentCTVolumeDialog(wxWindow *parent, const itk::Size<3>& volumeSize, const WindowSettings& windowSettings)
:	SegmentVolumeDialog<CTSegmentationOptions>(parent, volumeSize, windowSettings)
{
	initialise(parent);
}

//#################### PRIVATE METHODS ####################
bool SegmentCTVolumeDialog::construct_segmentation_options()
{
	itk::Size<3> subvolumeSize;
	if(!construct_subvolume_size(subvolumeSize)) return false;
	CTSegmentationOptions::InputType inputType = CTSegmentationOptions::InputType(m_inputType->GetSelection());
	m_segmentationOptions = CTSegmentationOptions(adf_iterations(), subvolumeSize, waterfall_layer_limit(), window_settings(), inputType);
	return true;
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

}

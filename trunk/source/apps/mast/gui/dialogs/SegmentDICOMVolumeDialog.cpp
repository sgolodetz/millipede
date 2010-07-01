/***
 * millipede: SegmentDICOMVolumeDialog.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "SegmentDICOMVolumeDialog.h"

namespace mp {

//#################### CONSTRUCTORS ####################
SegmentDICOMVolumeDialog::SegmentDICOMVolumeDialog(wxWindow *parent, const itk::Size<3>& volumeSize, const WindowSettings& windowSettings)
:	SegmentVolumeDialog<DICOMSegmentationOptions>(parent, volumeSize, windowSettings)
{
	initialise(parent, "Segment DICOM Volume");
}

//#################### PRIVATE METHODS ####################
bool SegmentDICOMVolumeDialog::construct_segmentation_options()
{
	itk::Size<3> subvolumeSize;
	if(!construct_subvolume_size(subvolumeSize)) return false;
	DICOMSegmentationOptions::InputType inputType = DICOMSegmentationOptions::InputType(m_inputType->GetSelection());
	m_segmentationOptions = DICOMSegmentationOptions(adf_iterations(), inputType, subvolumeSize, waterfall_layer_limit(), window_settings());
	return true;
}

wxPanel *SegmentDICOMVolumeDialog::create_modality_page(wxWindow *parent)
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

	sizer->Fit(panel);
	return panel;
}

}

/***
 * millipede: SegmentDICOMVolumeDialog.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "SegmentDICOMVolumeDialog.h"

#include <boost/lexical_cast.hpp>
using boost::bad_lexical_cast;
using boost::lexical_cast;

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

	double adfConductance;
	try
	{
		adfConductance = lexical_cast<double>(m_adfConductance->GetValue());
	}
	catch(bad_lexical_cast&)
	{
		wxMessageBox(wxT("Error: The ADF conductance must be a decimal number."), wxT("Error"), wxOK|wxICON_ERROR|wxCENTRE, this);
		return false;
	}

	DICOMSegmentationOptions::InputType inputType = DICOMSegmentationOptions::InputType(m_inputType->GetSelection());
	DICOMSegmentationOptions::WaterfallAlgorithm waterfallAlgorithm = DICOMSegmentationOptions::WaterfallAlgorithm(m_waterfallAlgorithm->GetSelection());
	m_segmentationOptions = DICOMSegmentationOptions(adfConductance, m_adfIterations->GetValue(), inputType, subvolumeSize, waterfallAlgorithm, m_waterfallLayerLimit->GetValue(), m_windowSettings);
	return true;
}

wxPanel *SegmentDICOMVolumeDialog::create_advanced_page(wxWindow *parent)
{
	wxPanel *panel = new wxPanel(parent);

	wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
	panel->SetSizer(sizer);

	// Set up the radio box to choose between base and windowed input.
	wxString inputStrings[DICOMSegmentationOptions::INPUTTYPE_COUNT];
	inputStrings[DICOMSegmentationOptions::INPUTTYPE_BASE] = wxT("Use &Base Input");
	inputStrings[DICOMSegmentationOptions::INPUTTYPE_WINDOWED] = wxT("Use &Windowed Input");
	m_inputType = new wxRadioBox(panel, wxID_ANY, wxT("Input Type"), wxDefaultPosition, wxDefaultSize, DICOMSegmentationOptions::INPUTTYPE_COUNT, inputStrings, 1, wxRA_SPECIFY_COLS);
	m_inputType->SetSelection(DICOMSegmentationOptions::INPUTTYPE_WINDOWED);
	sizer->Add(m_inputType);

	sizer->AddSpacer(10);

	// Set up the controls that allow the user to set the parameters for anisotropic diffusion filtering.
	wxGridSizer *filteringSizer = new wxGridSizer(0, 2, 0, 5);
	sizer->Add(filteringSizer);

	filteringSizer->Add(new wxStaticText(panel, wxID_ANY, wxT("ADF Conductance:")), 0, wxALIGN_CENTRE_VERTICAL);
	m_adfConductance = new wxTextCtrl(panel, wxID_ANY, wxT("1.0"));
	filteringSizer->Add(m_adfConductance, 0, wxALIGN_CENTRE_VERTICAL);

	filteringSizer->Add(new wxStaticText(panel, wxID_ANY, wxT("ADF Iterations:")), 0, wxALIGN_CENTRE_VERTICAL);
	m_adfIterations = new wxSpinCtrl(panel, wxID_ANY, wxT("20"), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 30, 20);
	filteringSizer->Add(m_adfIterations, 0, wxALIGN_CENTRE_VERTICAL);

	sizer->AddSpacer(10);

	// Set up the radio box to choose between different waterfall algorithms.
	wxString waterfallStrings[DICOMSegmentationOptions::WATERFALLALGORITHM_COUNT];
	waterfallStrings[DICOMSegmentationOptions::WATERFALLALGORITHM_DEEP] = wxT("Use &Deep Waterfall Algorithm");
	waterfallStrings[DICOMSegmentationOptions::WATERFALLALGORITHM_GOLODETZ] = wxT("Use &Golodetz Waterfall Algorithm");
	waterfallStrings[DICOMSegmentationOptions::WATERFALLALGORITHM_MARCOTEGUI] = wxT("Use &Marcotegui Waterfall Algorithm");
	waterfallStrings[DICOMSegmentationOptions::WATERFALLALGORITHM_NICHOLLS_CORRECT] = wxT("Use Nicholls Waterfall Algorithm (&Correct)");
	waterfallStrings[DICOMSegmentationOptions::WATERFALLALGORITHM_NICHOLLS_TWEAKED] = wxT("Use Nicholls Waterfall Algorithm (&Tweaked)");
	m_waterfallAlgorithm = new wxRadioBox(panel, wxID_ANY, wxT("Waterfall Algorithm"), wxDefaultPosition, wxDefaultSize, DICOMSegmentationOptions::WATERFALLALGORITHM_COUNT, waterfallStrings, 1, wxRA_SPECIFY_COLS);
	m_waterfallAlgorithm->SetSelection(DICOMSegmentationOptions::WATERFALLALGORITHM_NICHOLLS_TWEAKED);
	sizer->Add(m_waterfallAlgorithm);

	sizer->AddSpacer(10);

	// Set up the control that allows the user to set the waterfall layer limit.
	wxGridSizer *waterfallSizer = new wxGridSizer(0, 2, 0, 5);
	sizer->Add(waterfallSizer);

	waterfallSizer->Add(new wxStaticText(panel, wxID_ANY, wxT("Waterfall Layer Limit:")), 0, wxALIGN_CENTRE_VERTICAL);
	m_waterfallLayerLimit = new wxSpinCtrl(panel, wxID_ANY, wxT("5"), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 1000, 5);
	waterfallSizer->Add(m_waterfallLayerLimit, 0, wxALIGN_CENTRE_VERTICAL);

	sizer->Fit(panel);
	return panel;
}

}

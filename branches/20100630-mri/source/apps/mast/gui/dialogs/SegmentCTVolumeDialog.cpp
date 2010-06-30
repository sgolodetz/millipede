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
:	SegmentVolumeDialog<CTSegmentationOptions>(parent, volumeSize, windowSettings)
{
	initialise(parent);
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
	int waterfallLayerLimit = m_waterfallLayerLimit->GetValue();
	CTSegmentationOptions::InputType inputType = CTSegmentationOptions::InputType(m_inputType->GetSelection());
	m_segmentationOptions = CTSegmentationOptions(adfIterations, subvolumeSize, waterfallLayerLimit, m_windowSettings, inputType);
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

/***
 * millipede: SegmentCTVolumeDialog.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SEGMENTCTVOLUMEDIALOG
#define H_MILLIPEDE_SEGMENTCTVOLUMEDIALOG

#include <common/segmentation/CTSegmentationOptions.h>
#include "SegmentVolumeDialog.h"

namespace mp {

class SegmentCTVolumeDialog : public SegmentVolumeDialog<CTSegmentationOptions>
{
	//#################### PRIVATE VARIABLES ####################
private:
	wxRadioBox *m_inputType;

	//#################### CONSTRUCTORS ####################
public:
	SegmentCTVolumeDialog(wxWindow *parent, const itk::Size<3>& volumeSize, const WindowSettings& windowSettings);

	//#################### PRIVATE METHODS ####################
private:
	bool construct_segmentation_options();
	wxPanel *create_modality_page(wxWindow *parent);

	//#################### EVENT HANDLERS ####################
public:
	//~~~~~~~~~~~~~~~~~~~~ BUTTONS ~~~~~~~~~~~~~~~~~~~~
	void OnButtonOK(wxCommandEvent&);

	//~~~~~~~~~~~~~~~~~~~~ RADIO BOXES ~~~~~~~~~~~~~~~~~~~~
	void OnRadioBoxSegmentationType(wxCommandEvent&);

	//~~~~~~~~~~~~~~~~~~~~ UI UPDATES ~~~~~~~~~~~~~~~~~~~~
	void OnUpdateGridSizeControl(wxUpdateUIEvent& e);

	//#################### EVENT TABLE ####################
	DECLARE_EVENT_TABLE()
};

}

#endif

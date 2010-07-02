/***
 * millipede: SegmentDICOMVolumeDialog.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SEGMENTDICOMVOLUMEDIALOG
#define H_MILLIPEDE_SEGMENTDICOMVOLUMEDIALOG

#include <common/segmentation/DICOMSegmentationOptions.h>
#include "SegmentVolumeDialog.h"

namespace mp {

class SegmentDICOMVolumeDialog : public SegmentVolumeDialog<DICOMSegmentationOptions>
{
	//#################### PRIVATE VARIABLES ####################
private:
	WindowSettings m_windowSettings;

	wxSpinCtrl *m_adfIterations;
	wxRadioBox *m_inputType;
	wxSpinCtrl *m_waterfallLayerLimit;

	//#################### CONSTRUCTORS ####################
public:
	SegmentDICOMVolumeDialog(wxWindow *parent, const itk::Size<3>& volumeSize, const WindowSettings& windowSettings);

	//#################### PRIVATE METHODS ####################
private:
	bool construct_segmentation_options();
	wxPanel *create_advanced_page(wxWindow *parent);
};

}

#endif

/***
 * millipede: SegmentMRVolumeDialog.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SEGMENTMRVOLUMEDIALOG
#define H_MILLIPEDE_SEGMENTMRVOLUMEDIALOG

#include <common/segmentation/MRSegmentationOptions.h>
#include "SegmentVolumeDialog.h"

namespace mp {

class SegmentMRVolumeDialog : public SegmentVolumeDialog<MRSegmentationOptions>
{
	//#################### CONSTRUCTORS ####################
public:
	SegmentMRVolumeDialog(wxWindow *parent, const itk::Size<3>& volumeSize, const WindowSettings& windowSettings);

	//#################### PRIVATE METHODS ####################
private:
	bool construct_segmentation_options();
	wxPanel *create_modality_page(wxWindow *parent);
};

}

#endif

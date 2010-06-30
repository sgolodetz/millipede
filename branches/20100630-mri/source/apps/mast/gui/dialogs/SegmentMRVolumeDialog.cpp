/***
 * millipede: SegmentMRVolumeDialog.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "SegmentMRVolumeDialog.h"

namespace mp {

//#################### CONSTRUCTORS ####################
SegmentMRVolumeDialog::SegmentMRVolumeDialog(wxWindow *parent, const itk::Size<3>& volumeSize, const WindowSettings& windowSettings)
:	SegmentVolumeDialog<MRSegmentationOptions>(parent, volumeSize, windowSettings)
{
	initialise(parent, "Segment MR Volume");
}

//#################### PRIVATE METHODS ####################
bool SegmentMRVolumeDialog::construct_segmentation_options()
{
	itk::Size<3> subvolumeSize;
	if(!construct_subvolume_size(subvolumeSize)) return false;
	m_segmentationOptions = MRSegmentationOptions(adf_iterations(), subvolumeSize, waterfall_layer_limit(), window_settings());
	return true;
}

wxPanel *SegmentMRVolumeDialog::create_modality_page(wxWindow *parent)
{
	return NULL;
}

}

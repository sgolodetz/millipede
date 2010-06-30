/***
 * millipede: MRSegmentationOptions.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MRSEGMENTATIONOPTIONS
#define H_MILLIPEDE_MRSEGMENTATIONOPTIONS

#include "SegmentationOptions.h"

namespace mp {

struct MRSegmentationOptions : SegmentationOptions
{
	//#################### CONSTRUCTORS ####################
	MRSegmentationOptions(int adfIterations_, const itk::Size<3>& subvolumeSize_, int waterfallLayerLimit_, const WindowSettings& windowSettings_);
};

}

#endif

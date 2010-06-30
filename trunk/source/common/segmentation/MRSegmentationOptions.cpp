/***
 * millipede: MRSegmentationOptions.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "MRSegmentationOptions.h"

namespace mp {

//#################### CONSTRUCTORS ####################
MRSegmentationOptions::MRSegmentationOptions(int adfIterations_, const itk::Size<3>& subvolumeSize_, int waterfallLayerLimit_, const WindowSettings& windowSettings_)
:	SegmentationOptions(adfIterations_, subvolumeSize_, waterfallLayerLimit_, windowSettings_)
{}

}

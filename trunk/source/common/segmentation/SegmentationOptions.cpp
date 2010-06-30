/***
 * millipede: SegmentationOptions.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "SegmentationOptions.h"

namespace mp {

//#################### CONSTRUCTORS ####################
SegmentationOptions::SegmentationOptions(int adfIterations_, const itk::Size<3>& subvolumeSize_, int waterfallLayerLimit_, const WindowSettings& windowSettings_)
:	adfIterations(adfIterations_), subvolumeSize(subvolumeSize_), waterfallLayerLimit(waterfallLayerLimit_), windowSettings(windowSettings_)
{}

//#################### DESTRUCTOR ####################
SegmentationOptions::~SegmentationOptions()
{}

}

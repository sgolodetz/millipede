/***
 * millipede: CTSegmentationOptions.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "CTSegmentationOptions.h"

namespace mp {

//#################### CONSTRUCTORS ####################
CTSegmentationOptions::CTSegmentationOptions(int adfIterations_, const itk::Size<3>& subvolumeSize_, int waterfallLayerLimit_, const WindowSettings& windowSettings_,
											 InputType inputType_)
:	SegmentationOptions(adfIterations_, subvolumeSize_, waterfallLayerLimit_, windowSettings_), inputType(inputType_)
{}

}

/***
 * millipede: CTSegmentationOptions.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "CTSegmentationOptions.h"

namespace mp {

//#################### CONSTRUCTORS ####################
CTSegmentationOptions::CTSegmentationOptions(int adfIterations_, InputType inputType_, const itk::Size<3>& subvolumeSize_, int waterfallLayerLimit_,
											 const WindowSettings& windowSettings_)
:	adfIterations(adfIterations_), inputType(inputType_), subvolumeSize(subvolumeSize_), waterfallLayerLimit(waterfallLayerLimit_), windowSettings(windowSettings_)
{}

}

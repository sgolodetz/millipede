/***
 * millipede: CTSegmentationOptions.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "CTSegmentationOptions.h"

namespace mp {

//#################### CONSTRUCTORS ####################
CTSegmentationOptions::CTSegmentationOptions(InputType inputType_, const itk::Size<3>& subvolumeSize_, int waterfallLayerLimit_)
:	inputType(inputType_), subvolumeSize(subvolumeSize_), waterfallLayerLimit(waterfallLayerLimit_)
{}

}

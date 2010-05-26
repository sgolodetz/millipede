/***
 * millipede: CTSegmentationOptions.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "CTSegmentationOptions.h"

namespace mp {

//#################### CONSTRUCTORS ####################
CTSegmentationOptions::CTSegmentationOptions(const itk::Size<3>& gridSize_, InputType inputType_, int waterfallLayerLimit_)
:	gridSize(gridSize_), inputType(inputType_), waterfallLayerLimit(waterfallLayerLimit_)
{}

}

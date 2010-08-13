/***
 * millipede: DICOMSegmentationOptions.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "DICOMSegmentationOptions.h"

namespace mp {

//#################### CONSTRUCTORS ####################
DICOMSegmentationOptions::DICOMSegmentationOptions(double adfConductance_, int adfIterations_, InputType inputType_, const itk::Size<3>& subvolumeSize_,
												   WaterfallAlgorithm waterfallAlgorithm_, int waterfallLayerLimit_, const WindowSettings& windowSettings_)
:	adfConductance(adfConductance_),
	adfIterations(adfIterations_),
	inputType(inputType_),
	subvolumeSize(subvolumeSize_),
	waterfallAlgorithm(waterfallAlgorithm_),
	waterfallLayerLimit(waterfallLayerLimit_),
	windowSettings(windowSettings_)
{}

}

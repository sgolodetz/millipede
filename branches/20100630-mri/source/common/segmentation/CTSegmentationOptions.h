/***
 * millipede: CTSegmentationOptions.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_CTSEGMENTATIONOPTIONS
#define H_MILLIPEDE_CTSEGMENTATIONOPTIONS

#include "SegmentationOptions.h"

namespace mp {

struct CTSegmentationOptions : SegmentationOptions
{
	//#################### CONSTANTS ####################
	enum InputType
	{
		INPUTTYPE_WINDOWED,
		INPUTTYPE_HOUNSFIELD,
		INPUTTYPE_COUNT,		// dummy value containing the number of input types
	};

	//#################### PUBLIC VARIABLES ####################
	InputType inputType;

	//#################### CONSTRUCTORS ####################
	CTSegmentationOptions(int adfIterations_, const itk::Size<3>& subvolumeSize_, int waterfallLayerLimit_, const WindowSettings& windowSettings_, InputType inputType_);
};

}

#endif

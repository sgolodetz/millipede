/***
 * millipede: CTSegmentationOptions.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_CTSEGMENTATIONOPTIONS
#define H_MILLIPEDE_CTSEGMENTATIONOPTIONS

#include <itkSize.h>

namespace mp {

struct CTSegmentationOptions
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
	itk::Size<3> subvolumeSize;
	int waterfallLayerLimit;

	//#################### CONSTRUCTORS ####################
	CTSegmentationOptions(InputType inputType_, const itk::Size<3>& subvolumeSize_, int waterfallLayerLimit_);
};

}

#endif

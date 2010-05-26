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
	itk::Size<3> gridSize;
	InputType inputType;
	int waterfallLayerLimit;

	//#################### CONSTRUCTORS ####################
	CTSegmentationOptions(const itk::Size<3>& gridSize_, InputType inputType_, int waterfallLayerLimit_);
};

}

#endif

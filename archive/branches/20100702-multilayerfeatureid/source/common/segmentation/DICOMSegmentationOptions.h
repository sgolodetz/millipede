/***
 * millipede: DICOMSegmentationOptions.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_DICOMSEGMENTATIONOPTIONS
#define H_MILLIPEDE_DICOMSEGMENTATIONOPTIONS

#include <itkSize.h>

#include <common/dicom/util/WindowSettings.h>

namespace mp {

struct DICOMSegmentationOptions
{
	//#################### CONSTANTS ####################
	enum InputType
	{
		INPUTTYPE_BASE,
		INPUTTYPE_WINDOWED,
		INPUTTYPE_COUNT,		// dummy value containing the number of input types
	};

	//#################### PUBLIC VARIABLES ####################
	int adfIterations;
	InputType inputType;
	itk::Size<3> subvolumeSize;
	int waterfallLayerLimit;
	WindowSettings windowSettings;

	//#################### CONSTRUCTORS ####################
	DICOMSegmentationOptions(int adfIterations_, InputType inputType_, const itk::Size<3>& subvolumeSize_, int waterfallLayerLimit_, const WindowSettings& windowSettings_);
};

}

#endif

/***
 * millipede: SegmentationOptions.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SEGMENTATIONOPTIONS
#define H_MILLIPEDE_SEGMENTATIONOPTIONS

#include <itkSize.h>

#include <common/dicom/util/WindowSettings.h>

namespace mp {

class SegmentationOptions
{
	//#################### PUBLIC VARIABLES ####################
public:
	int adfIterations;
	itk::Size<3> subvolumeSize;
	int waterfallLayerLimit;
	WindowSettings windowSettings;

	//#################### CONSTRUCTORS ####################
public:
	SegmentationOptions(int adfIterations_, const itk::Size<3>& subvolumeSize_, int waterfallLayerLimit_, const WindowSettings& windowSettings_);

	//#################### DESTRUCTOR ####################
protected:
	~SegmentationOptions();
};

}

#endif

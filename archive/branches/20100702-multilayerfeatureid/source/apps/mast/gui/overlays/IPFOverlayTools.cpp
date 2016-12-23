/***
 * millipede: IPFOverlayTools.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "IPFOverlayTools.h"

namespace mp {

namespace IPFOverlayTools {

//#################### FUNCTIONS ####################
void calculate_slice_parameters(const itk::Size<3>& volumeSize, const SliceLocation& sliceLocation, SliceOrientation sliceOrientation,
								itk::Index<3>& sliceBegin, itk::Index<3>& sliceEnd, int& width, int& height)
{
	sliceBegin.Fill(0);
	sliceEnd = ITKImageUtil::make_index_from_size(volumeSize);
	width = -1, height = -1;
	switch(sliceOrientation)
	{
		case ORIENT_XY:
			sliceBegin[2] = sliceLocation.z;
			sliceEnd[2] = sliceLocation.z + 1;
			width = volumeSize[0], height = volumeSize[1];
			break;
		case ORIENT_XZ:
			sliceBegin[1] = sliceLocation.y;
			sliceEnd[1] = sliceLocation.y + 1;
			width = volumeSize[0], height = volumeSize[2];
			break;
		case ORIENT_YZ:
			sliceBegin[0] = sliceLocation.x;
			sliceEnd[0] = sliceLocation.x + 1;
			width = volumeSize[1], height = volumeSize[2];
			break;
	}
}

}

}

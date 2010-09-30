/***
 * millipede: IPFOverlayTools.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "IPFOverlayTools.h"

#include <cassert>

#include <itkImageRegionIterator.h>
#include <itkShapedNeighborhoodIterator.h>
#include <itkZeroFluxNeumannBoundaryCondition.h>

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

void draw_boundaries(RGBA32Image::Pointer sourceImage, RGBA32Image::Pointer destImage, const boost::optional<RGBA32>& colour, bool keepBackground)
{
	assert(sourceImage->GetLargestPossibleRegion().GetSize() == destImage->GetLargestPossibleRegion().GetSize());

	typedef itk::Image<bool,2> BoundaryImage;
	BoundaryImage::Pointer boundariesImage = ITKImageUtil::make_image<bool>(sourceImage->GetLargestPossibleRegion().GetSize());

	// Set up an iterator to traverse the image, whilst allowing us to access the neighbours of each pixel.
	typedef itk::ConstShapedNeighborhoodIterator<RGBA32Image> NIT;
	itk::Size<2> radius = {{1,1}};
	NIT it(radius, sourceImage, sourceImage->GetLargestPossibleRegion());
	std::vector<itk::Offset<2> > offsets = ITKImageUtil::make_4_connected_offsets();
	for(std::vector<itk::Offset<2> >::const_iterator kt=offsets.begin(), kend=offsets.end(); kt!=kend; ++kt)
	{
		it.ActivateOffset(*kt);
	}

	// Set up a boundary condition that makes pixels beyond the image boundary equal to those on them.
	itk::ZeroFluxNeumannBoundaryCondition<RGBA32Image> condition;
	it.OverrideBoundaryCondition(&condition);

	// Traverse the image, and set boundaries in the boundaries image where necessary.
	for(it.GoToBegin(); !it.IsAtEnd(); ++it)
	{
		boundariesImage->SetPixel(it.GetIndex(), false);

		for(NIT::ConstIterator jt=it.Begin(), jend=it.End(); jt!=jend; ++jt)
		{
			// If one of the pixel's neighbours has a different colour, the pixel is a boundary.
			if(jt.Get() != it.GetCenterPixel())
			{
				boundariesImage->SetPixel(it.GetIndex(), true);
				break;
			}
		}
	}

	itk::ImageRegionIterator<RGBA32Image> sourceIt(sourceImage, sourceImage->GetLargestPossibleRegion());
	itk::ImageRegionIterator<RGBA32Image> destIt(destImage, destImage->GetLargestPossibleRegion());
	itk::ImageRegionIterator<BoundaryImage> boundariesIt(boundariesImage, boundariesImage->GetLargestPossibleRegion());
	RGBA32 transparent = ITKImageUtil::make_rgba32(0, 0, 0, 0);

	if(colour)
	{
		// Write the specified colour into the destination image wherever there is a boundary. If the rest of the destination image (the background)
		// should not be kept, write a transparent colour into all non-boundary destination pixels.
		for(sourceIt.GoToBegin(), destIt.GoToBegin(), boundariesIt.GoToBegin(); !sourceIt.IsAtEnd(); ++sourceIt, ++destIt, ++boundariesIt)
		{
			if(boundariesIt.Get() == true)
			{
				destIt.Set(*colour);
			}
			else if(!keepBackground)
			{
				destIt.Set(transparent);
			}
		}
	}
	else
	{
		// Copy the pixel from the source image to the destination image wherever there is a boundary. If the rest of the destination image (the background)
		// should not be kept, write a transparent colour into all non-boundary destination pixels.
		for(sourceIt.GoToBegin(), destIt.GoToBegin(), boundariesIt.GoToBegin(); !sourceIt.IsAtEnd(); ++sourceIt, ++destIt, ++boundariesIt)
		{
			if(boundariesIt.Get() == true)
			{
				destIt.Set(sourceIt.Get());
			}
			else if(!keepBackground)
			{
				destIt.Set(transparent);
			}
		}
	}
}

}

}

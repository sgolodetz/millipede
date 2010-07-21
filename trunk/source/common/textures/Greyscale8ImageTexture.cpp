/***
 * millipede: Greyscale8ImageTexture.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "Greyscale8ImageTexture.h"

#include <itkLinearInterpolateImageFunction.h>
#include <itkResampleImageFilter.h>

namespace mp {

//#################### CONSTRUCTORS ####################
Greyscale8ImageTexture::Greyscale8ImageTexture(const ImagePointer& image, bool clamp)
:	ITKImageTexture<Greyscale8Image>(image, clamp)
{
	reload();
}

//#################### PUBLIC METHODS ####################
boost::shared_ptr<ITKImageTexture<Greyscale8Image> > Greyscale8ImageTexture::clone() const
{
	return boost::shared_ptr<ITKImageTexture<Greyscale8Image> >(new Greyscale8ImageTexture(clone_image(), is_clamped()));
}

//#################### PRIVATE METHODS ####################
void Greyscale8ImageTexture::reload_image() const
{
	typedef itk::LinearInterpolateImageFunction<Image> Interpolator;
	typedef itk::ResampleImageFilter<Image,Image> Resampler;
	ImagePointer input = scaled_image<Resampler,Interpolator>(50);
	itk::Size<2> size = input->GetLargestPossibleRegion().GetSize();
	glTexImage2D(GL_TEXTURE_2D, 0, 1, size[0], size[1], 0, GL_LUMINANCE, GL_UNSIGNED_BYTE, input->GetBufferPointer());
}

void Greyscale8ImageTexture::reload_partial_image(int minX, int minY, int maxX, int maxY) const
{
	typedef itk::LinearInterpolateImageFunction<Image> Interpolator;
	typedef itk::ResampleImageFilter<Image,Image> Resampler;
	int xOffset = -1, yOffset = -1;
	ImagePointer input = scaled_partial_image<Resampler,Interpolator>(minX, minY, maxX, maxY, 50, xOffset, yOffset);
	itk::Size<2> size = input->GetLargestPossibleRegion().GetSize();
	glTexSubImage2D(GL_TEXTURE_2D, 0, xOffset, yOffset, size[0], size[1], GL_LUMINANCE, GL_UNSIGNED_BYTE, input->GetBufferPointer());
}

}

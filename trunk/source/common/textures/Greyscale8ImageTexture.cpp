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

//#################### PRIVATE METHODS ####################
void Greyscale8ImageTexture::reload_image() const
{
	typedef itk::LinearInterpolateImageFunction<Image> Interpolator;
	typedef itk::ResampleImageFilter<Image,Image> Resampler;
	ImagePointer input = input_image<Resampler,Interpolator>(50);
	itk::Size<2> size = input->GetLargestPossibleRegion().GetSize();
	glTexImage2D(GL_TEXTURE_2D, 0, 1, size[0], size[1], 0, GL_LUMINANCE, GL_UNSIGNED_BYTE, input->GetBufferPointer());
}

void Greyscale8ImageTexture::reload_partial_image(int minX, int minY, int maxX, int maxY) const
{
	// TEMPORARY: Until this is properly implemented, just reload the whole image.
	reload_image();
}

}

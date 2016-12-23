/***
 * millipede: RGBA32ImageTexture.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "RGBA32ImageTexture.h"

#include <itkVectorLinearInterpolateImageFunction.h>
#include <itkVectorResampleImageFilter.h>

namespace mp {

//#################### CONSTRUCTORS ####################
RGBA32ImageTexture::RGBA32ImageTexture(const ImagePointer& image, bool clamp)
:	ITKImageTexture<RGBA32Image>(image, clamp)
{
	reload();
}

//#################### PUBLIC METHODS ####################
boost::shared_ptr<ITKImageTexture<RGBA32Image> > RGBA32ImageTexture::clone() const
{
	return boost::shared_ptr<ITKImageTexture<RGBA32Image> >(new RGBA32ImageTexture(clone_image(), is_clamped()));
}

//#################### PRIVATE METHODS ####################
std::vector<unsigned char> RGBA32ImageTexture::make_buffer(const RGBA32 *const pixels, const itk::Size<2>& size)
{
	int pixelCount = size[0] * size[1];
	std::vector<unsigned char> data(pixelCount * 4);
	for(int i=0; i<pixelCount; ++i)
	{
		data[i*4]	= pixels[i][0];
		data[i*4+1]	= pixels[i][1];
		data[i*4+2]	= pixels[i][2];
		data[i*4+3]	= pixels[i][3];
	}
	return data;
}

void RGBA32ImageTexture::reload_image() const
{
	typedef itk::VectorLinearInterpolateImageFunction<Image> Interpolator;
	typedef itk::VectorResampleImageFilter<Image,Image> Resampler;
	ImagePointer input = scaled_image<Resampler,Interpolator>(50);

	const RGBA32 *const pixels = input->GetBufferPointer();
	itk::Size<2> size = input->GetLargestPossibleRegion().GetSize();
	std::vector<unsigned char> data = make_buffer(pixels, size);

	glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, size[0], size[1], 0, GL_RGBA, GL_UNSIGNED_BYTE, &data[0]);
}

void RGBA32ImageTexture::reload_partial_image(int minX, int minY, int maxX, int maxY) const
{
	typedef itk::VectorLinearInterpolateImageFunction<Image> Interpolator;
	typedef itk::VectorResampleImageFilter<Image,Image> Resampler;
	int xOffset = -1, yOffset = -1;
	ImagePointer input = scaled_partial_image<Resampler,Interpolator>(minX, minY, maxX, maxY, 50, xOffset, yOffset);

	const RGBA32 *const pixels = input->GetBufferPointer();
	itk::Size<2> size = input->GetLargestPossibleRegion().GetSize();
	std::vector<unsigned char> data = make_buffer(pixels, size);

	glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
	glTexSubImage2D(GL_TEXTURE_2D, 0, xOffset, yOffset, size[0], size[1], GL_RGBA, GL_UNSIGNED_BYTE, &data[0]);
}

}

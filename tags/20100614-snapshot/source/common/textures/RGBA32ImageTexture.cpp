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

//#################### PRIVATE METHODS ####################
void RGBA32ImageTexture::reload_image() const
{
	typedef itk::VectorLinearInterpolateImageFunction<Image> Interpolator;
	typedef itk::VectorResampleImageFilter<Image,Image> Resampler;
	ImagePointer input = input_image<Resampler,Interpolator>(50);

	const RGBA32 *const pixels = input->GetBufferPointer();
	itk::Size<2> size = input->GetLargestPossibleRegion().GetSize();

	int pixelCount = size[0] * size[1];
	std::vector<unsigned char> data(pixelCount * 4);
	for(int i=0; i<pixelCount; ++i)
	{
		data[i*4]	= pixels[i][0];
		data[i*4+1]	= pixels[i][1];
		data[i*4+2]	= pixels[i][2];
		data[i*4+3]	= pixels[i][3];
	}
	glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, size[0], size[1], 0, GL_RGBA, GL_UNSIGNED_BYTE, &data[0]);
}

}

/***
 * millipede: RGB24ImageTexture.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "RGB24ImageTexture.h"

#include <itkVectorLinearInterpolateImageFunction.h>
#include <itkVectorResampleImageFilter.h>

namespace mp {

//#################### CONSTRUCTORS ####################
RGB24ImageTexture::RGB24ImageTexture(const ImagePointer& image, const boost::optional<RGB24>& colourKey, bool clamp)
:	ITKImageTexture<RGB24Image>(image, clamp), m_colourKey(colourKey)
{
	reload();
}

//#################### PRIVATE METHODS ####################
void RGB24ImageTexture::reload_image() const
{
	typedef itk::VectorLinearInterpolateImageFunction<Image> Interpolator;
	typedef itk::VectorResampleImageFilter<Image,Image> Resampler;
	ImagePointer input = input_image<Resampler,Interpolator>(50);

	const RGB24 *const pixels = input->GetBufferPointer();
	itk::Size<2> size = input->GetLargestPossibleRegion().GetSize();

	if(m_colourKey)	reload_image_with_colour_key(pixels, size);
	else			reload_image_without_colour_key(pixels, size);
}

void RGB24ImageTexture::reload_image_with_colour_key(const RGB24 *const pixels, const itk::Size<2>& size) const
{
	int pixelCount = size[0] * size[1];
	std::vector<unsigned char> data(pixelCount * 4);
	for(int i=0; i<pixelCount; ++i)
	{
		data[i*4]	= pixels[i][0];
		data[i*4+1]	= pixels[i][1];
		data[i*4+2] = pixels[i][2];
		if(pixels[i] == *m_colourKey) data[i*4+3] = 0;
		else data[i*4+3] = 255;
	}
	glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, size[0], size[1], 0, GL_RGBA, GL_UNSIGNED_BYTE, &data[0]);
}

void RGB24ImageTexture::reload_image_without_colour_key(const RGB24 *const pixels, const itk::Size<2>& size) const
{
	int pixelCount = size[0] * size[1];
	std::vector<unsigned char> data(pixelCount * 3);
	for(int i=0; i<pixelCount; ++i)
	{
		data[i*3]	= pixels[i][0];
		data[i*3+1]	= pixels[i][1];
		data[i*3+2] = pixels[i][2];
	}
	glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, size[0], size[1], 0, GL_RGB, GL_UNSIGNED_BYTE, &data[0]);
}

}

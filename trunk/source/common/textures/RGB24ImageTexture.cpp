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
std::vector<unsigned char> RGB24ImageTexture::make_buffer_with_colour_key(const RGB24 *const pixels, const itk::Size<2>& size) const
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
	return data;
}

std::vector<unsigned char> RGB24ImageTexture::make_buffer_without_colour_key(const RGB24 *const pixels, const itk::Size<2>& size)
{
	int pixelCount = size[0] * size[1];
	std::vector<unsigned char> data(pixelCount * 3);
	for(int i=0; i<pixelCount; ++i)
	{
		data[i*3]	= pixels[i][0];
		data[i*3+1]	= pixels[i][1];
		data[i*3+2] = pixels[i][2];
	}
	return data;
}

void RGB24ImageTexture::reload_image() const
{
	typedef itk::VectorLinearInterpolateImageFunction<Image> Interpolator;
	typedef itk::VectorResampleImageFilter<Image,Image> Resampler;
	ImagePointer input = scaled_image<Resampler,Interpolator>(50);

	const RGB24 *const pixels = input->GetBufferPointer();
	itk::Size<2> size = input->GetLargestPossibleRegion().GetSize();

	glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
	if(m_colourKey)
	{
		std::vector<unsigned char> data = make_buffer_with_colour_key(pixels, size);
		glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, size[0], size[1], 0, GL_RGBA, GL_UNSIGNED_BYTE, &data[0]);
	}
	else
	{
		std::vector<unsigned char> data = make_buffer_without_colour_key(pixels, size);
		glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, size[0], size[1], 0, GL_RGB, GL_UNSIGNED_BYTE, &data[0]);
	}
}

void RGB24ImageTexture::reload_partial_image(int minX, int minY, int maxX, int maxY) const
{
	// TEMPORARY: Until this is properly implemented, just reload the whole image.
	reload_image();
}

}

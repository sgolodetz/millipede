/***
 * millipede: Greyscale8ImageTexture.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include "Greyscale8ImageTexture.h"

#include <itkAffineTransform.h>
#include <itkLinearInterpolateImageFunction.h>
#include <itkResampleImageFilter.h>

namespace mp {

//#################### CONSTRUCTORS ####################
Greyscale8ImageTexture::Greyscale8ImageTexture(const ImagePointer& image, bool clamp)
:	Texture(clamp), m_image(image)
{
	reload();
}

//#################### PROTECTED METHODS ####################
void Greyscale8ImageTexture::reload_image() const
{
	typedef itk::LinearInterpolateImageFunction<Image> Interpolator;
	typedef itk::ResampleImageFilter<Image,Image> Resampler;
	typedef itk::AffineTransform<double,2> Transform;

	Image::SizeType size = m_image->GetLargestPossibleRegion().GetSize();

	// Resize the image so that its dimensions are powers of two if necessary.
	unsigned int desiredWidth = 1, desiredHeight = 1;
	while(desiredWidth < size[0]) desiredWidth *= 2;
	while(desiredHeight < size[1]) desiredHeight *= 2;
	if(desiredWidth != size[0] || desiredHeight != size[1])
	{
		Resampler::Pointer resampler = Resampler::New();

		Transform::Pointer transform = Transform::New();
		resampler->SetTransform(transform);

		resampler->SetInterpolator(Interpolator::New());

		resampler->SetDefaultPixelValue(50);

		Image::SizeType newSize = {{desiredWidth, desiredHeight}};
		resampler->SetSize(newSize);

		resampler->SetOutputOrigin(m_image->GetOrigin());

		Image::SpacingType newSpacing = m_image->GetSpacing();
		newSpacing[0] *= (double)size[0] / desiredWidth;
		newSpacing[1] *= (double)size[1] / desiredHeight;
		resampler->SetOutputSpacing(newSpacing);

		resampler->SetInput(m_image);
		resampler->Update();

		glTexImage2D(GL_TEXTURE_2D, 0, 1, desiredWidth, desiredHeight, 0, GL_LUMINANCE, GL_UNSIGNED_BYTE, resampler->GetOutput()->GetBufferPointer());
	}
	else
	{
		glTexImage2D(GL_TEXTURE_2D, 0, 1, size[0], size[1], 0, GL_LUMINANCE, GL_UNSIGNED_BYTE, m_image->GetBufferPointer());
	}
}

}

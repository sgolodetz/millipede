/***
 * millipede: ITKImageTexture.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_ITKIMAGETEXTURE
#define H_MILLIPEDE_ITKIMAGETEXTURE

#include <itkAffineTransform.h>
#include <itkLinearInterpolateImageFunction.h>
#include <itkResampleImageFilter.h>

#include "Texture.h"

namespace mp {

template <typename ImageType>
class ITKImageTexture : public Texture
{
	//#################### TYPEDEFS ####################
protected:
	typedef ImageType Image;
	typedef typename Image::Pointer ImagePointer;

	//#################### PRIVATE VARIABLES ####################
private:
	ImagePointer m_image;

	//#################### CONSTRUCTORS ####################
public:
	explicit ITKImageTexture(const ImagePointer& image, bool clamp)
	:	Texture(clamp), m_image(image)
	{}

	//#################### PROTECTED METHODS ####################
protected:
	ImagePointer image() const
	{
		return m_image;
	}

	/**
	Calculate a version of the image that is resized so that its dimensions are powers of two if necessary.
	*/
	ImagePointer input_image() const
	{
		itk::Size<2> size = image()->GetLargestPossibleRegion().GetSize();

		unsigned int desiredWidth = 1, desiredHeight = 1;
		while(desiredWidth < size[0]) desiredWidth *= 2;
		while(desiredHeight < size[1]) desiredHeight *= 2;
		if(desiredWidth == size[0] && desiredHeight == size[1])
		{
			return image();
		}

		typedef itk::LinearInterpolateImageFunction<Image> Interpolator;
		typedef itk::ResampleImageFilter<Image,Image> Resampler;
		typedef itk::AffineTransform<double,2> Transform;

		Resampler::Pointer resampler = Resampler::New();
		resampler->SetTransform(Transform::New());
		resampler->SetInterpolator(Interpolator::New());
		resampler->SetDefaultPixelValue(50);
		Image::SizeType newSize = {{desiredWidth, desiredHeight}};
		resampler->SetSize(newSize);
		resampler->SetOutputOrigin(image()->GetOrigin());
		Image::SpacingType newSpacing = image()->GetSpacing();
		newSpacing[0] *= (double)size[0] / desiredWidth;
		newSpacing[1] *= (double)size[1] / desiredHeight;
		resampler->SetOutputSpacing(newSpacing);
		resampler->SetInput(image());
		resampler->Update();

		return resampler->GetOutput();
	}
};

}

#endif

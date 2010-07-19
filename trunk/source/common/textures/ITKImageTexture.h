/***
 * millipede: ITKImageTexture.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_ITKIMAGETEXTURE
#define H_MILLIPEDE_ITKIMAGETEXTURE

#include <itkAffineTransform.h>
#include <itkImage.h>

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

	//#################### PUBLIC METHODS ####################
public:
	ImagePointer image() const
	{
		return m_image;
	}

	//#################### PROTECTED METHODS ####################
protected:
	/**
	Calculate a version of the image that is resized so that its dimensions are powers of two if necessary.
	*/
	template <typename Resampler, typename Interpolator>
	ImagePointer input_image(const typename Image::PixelType& defaultValue) const
	{
		itk::Size<2> size = image()->GetLargestPossibleRegion().GetSize();

		unsigned int desiredWidth = 1, desiredHeight = 1;
		while(desiredWidth < size[0]) desiredWidth *= 2;
		while(desiredHeight < size[1]) desiredHeight *= 2;
		if(desiredWidth == size[0] && desiredHeight == size[1])
		{
			return image();
		}

		typename Resampler::Pointer resampler = Resampler::New();
		typedef itk::AffineTransform<double,2> Transform;
		resampler->SetTransform(Transform::New());
		resampler->SetInterpolator(Interpolator::New());
		resampler->SetDefaultPixelValue(defaultValue);
		itk::Size<2> newSize = {{desiredWidth, desiredHeight}};
		resampler->SetSize(newSize);
		resampler->SetOutputOrigin(image()->GetOrigin());
		typename Image::SpacingType newSpacing = image()->GetSpacing();
		newSpacing[0] *= static_cast<double>(size[0]) / desiredWidth;
		newSpacing[1] *= static_cast<double>(size[1]) / desiredHeight;
		resampler->SetOutputSpacing(newSpacing);
		resampler->SetInput(image());
		resampler->Update();

		return resampler->GetOutput();
	}
};

}

#endif

/***
 * millipede: ITKImageTexture.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_ITKIMAGETEXTURE
#define H_MILLIPEDE_ITKIMAGETEXTURE

#include <climits>
#include <cmath>

#include <itkAffineTransform.h>
#include <itkExtractImageFilter.h>
#include <itkImage.h>
#include <itkImageDuplicator.h>

#include "Texture.h"

namespace mp {

template <typename ImageType>
class ITKImageTexture : public Texture
{
	//#################### TYPEDEFS ####################
protected:
	typedef ImageType Image;
	typedef typename Image::Pointer ImagePointer;
	typedef typename ImageType::PixelType Pixel;

	//#################### PRIVATE VARIABLES ####################
private:
	mutable itk::Index<2> m_dirtyLower, m_dirtyUpper;
	ImagePointer m_image;

	//#################### CONSTRUCTORS ####################
public:
	explicit ITKImageTexture(const ImagePointer& image, bool clamp)
	:	Texture(clamp), m_image(image)
	{
		reset_dirty_region();
	}

	//#################### COPY CONSTRUCTOR & ASSIGNMENT OPERATOR ####################
private:
	ITKImageTexture(const ITKImageTexture&);
	ITKImageTexture& operator=(const ITKImageTexture&);

	//#################### PUBLIC ABSTRACT METHODS ####################
public:
	virtual boost::shared_ptr<ITKImageTexture<Image> > clone() const = 0;

	//#################### PUBLIC METHODS ####################
public:
	void bind() const
	{
		Texture::bind();
		if(is_dirty())
		{
			reload_dirty_region();
		}
	}

	const Pixel& get_pixel(const itk::Index<2>& index) const
	{
		return m_image->GetPixel(index);
	}

	void reload() const
	{
		Texture::reload();
		reset_dirty_region();
	}

	void reload_dirty_region() const
	{
		reload_partial(m_dirtyLower[0], m_dirtyLower[1], m_dirtyUpper[0], m_dirtyUpper[1]);
	}

	void reload_partial(int minX, int minY, int maxX, int maxY) const
	{
		Texture::reload_partial(minX, minY, maxX, maxY);
		reset_dirty_region();
	}

	void set_pixel(const itk::Index<2>& index, const Pixel& pixel)
	{
		m_image->SetPixel(index, pixel);

		// Update the dirty region for the image based on the pixel's position.
		for(int i=0; i<2; ++i)
		{
			m_dirtyLower[i] = std::min(m_dirtyLower[i], index[i]);
			m_dirtyUpper[i] = std::max(m_dirtyUpper[i], index[i]);
		}
	}

	//#################### PROTECTED METHODS ####################
protected:
	ImagePointer clone_image() const
	{
		typedef itk::ImageDuplicator<Image> Duplicator;
		typename Duplicator::Pointer duplicator = Duplicator::New();
		duplicator->SetInputImage(m_image);
		duplicator->Update();
		return duplicator->GetOutput();
	}

	/**
	Calculate a version of the image that is resized so that its dimensions are powers of two if necessary.
	*/
	template <typename Resampler, typename Interpolator>
	ImagePointer scaled_image(const typename Image::PixelType& defaultValue) const
	{
		itk::Size<2> size = m_image->GetLargestPossibleRegion().GetSize();

		unsigned int desiredWidth = 1, desiredHeight = 1;
		while(desiredWidth < size[0]) desiredWidth *= 2;
		while(desiredHeight < size[1]) desiredHeight *= 2;
		if(desiredWidth == size[0] && desiredHeight == size[1])
		{
			return m_image;
		}

		typename Resampler::Pointer resampler = Resampler::New();
		typedef itk::AffineTransform<double,2> Transform;
		resampler->SetTransform(Transform::New());
		resampler->SetInterpolator(Interpolator::New());
		resampler->SetDefaultPixelValue(defaultValue);
		itk::Size<2> newSize = {{desiredWidth, desiredHeight}};
		resampler->SetSize(newSize);
		resampler->SetOutputOrigin(m_image->GetOrigin());
		typename Image::SpacingType newSpacing = m_image->GetSpacing();
		newSpacing[0] *= static_cast<double>(size[0]) / desiredWidth;
		newSpacing[1] *= static_cast<double>(size[1]) / desiredHeight;
		resampler->SetOutputSpacing(newSpacing);
		resampler->SetInput(m_image);
		resampler->Update();

		return resampler->GetOutput();
	}

	template <typename Resampler, typename Interpolator>
	ImagePointer scaled_partial_image(int minX, int minY, int maxX, int maxY, const typename Image::PixelType& defaultValue, int& xOffset, int& yOffset) const
	{
		ImagePointer scaledImage = scaled_image<Resampler,Interpolator>(defaultValue);

		itk::Size<2> initialSize = m_image->GetLargestPossibleRegion().GetSize();
		itk::Size<2> scaledSize = scaledImage->GetLargestPossibleRegion().GetSize();

		double scaleX = static_cast<double>(scaledSize[0]) / initialSize[0];
		double scaleY = static_cast<double>(scaledSize[1]) / initialSize[1];

		typedef itk::ExtractImageFilter<Image,Image> Extractor;
		typename Extractor::Pointer extractor = Extractor::New();
		extractor->SetInput(scaledImage);

		itk::Index<2> partialMins = {{floor(scaleX * minX), floor(scaleY * minY)}};
		itk::Index<2> partialMaxs = {{ceil(scaleX * maxX), ceil(scaleY * maxY)}};
		itk::Size<2> partialSize = {{partialMaxs[0] + 1 - partialMins[0], partialMaxs[1] + 1 - partialMins[1]}};

		itk::ImageRegion<2> region;
		region.SetIndex(partialMins);
		region.SetSize(partialSize);
		extractor->SetExtractionRegion(region);
		extractor->Update();

		xOffset = partialMins[0];
		yOffset = partialMins[1];

		return extractor->GetOutput();
	}

	//#################### PRIVATE METHODS ####################
private:
	bool is_dirty() const
	{
		return  m_dirtyLower[0] != INT_MAX;
	}

	void reset_dirty_region() const
	{
		m_dirtyLower[0] = m_dirtyLower[1] = INT_MAX;
		m_dirtyUpper[0] = m_dirtyUpper[1] = INT_MIN;
	}
};

}

#endif

/***
 * millipede: ITKImageUtil.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_ITKIMAGEUTIL
#define H_MILLIPEDE_ITKIMAGEUTIL

#include <vector>

#include <itkImageRegionIterator.h>
#include <itkRGBAPixel.h>

#include <common/slices/SliceOrientation.h>

namespace mp {

namespace ITKImageUtil {

//#################### FUNCTIONS ####################
itk::Index<3> make_index(long x, long y, long z);
itk::RGBAPixel<unsigned char> make_rgba32(unsigned char r, unsigned char g, unsigned char b, unsigned char a);
std::vector<itk::Offset<2> > make_4_connected_offsets();
std::vector<itk::Offset<3> > make_4_connected_offsets(SliceOrientation ori);
std::vector<itk::Offset<3> > make_6_connected_offsets();

//#################### TEMPLATE FUNCTIONS ####################
template <typename TPixel, unsigned int Dimension>
void fill_image(const typename itk::Image<TPixel,Dimension>::Pointer& image, const TPixel *const pixels, itk::Image<TPixel,Dimension>& /* dummy */)
{
	// Note:	This version of fill_image() takes a dummy parameter to allow it to deduce TPixel and Dimension.
	//			These cannot be deduced from Image<TPixel,Dimension>::Pointer - it's a "non-deducible context".
	//			Deducing TPixel is important in order to constrain the type of the pixels passed in.
	typedef itk::Image<TPixel,Dimension> Image;
	itk::ImageRegionIterator<Image> it(image, image->GetLargestPossibleRegion());
	const TPixel *p = pixels;
	for(it.GoToBegin(); !it.IsAtEnd(); ++it)
	{
		it.Set(*p++);
	}
}

template <typename ImagePointer, typename TPixel>
void fill_image(const ImagePointer& image, const TPixel *const pixels)
{
	fill_image(image, pixels, *image);
}

template <typename TPixel, unsigned int Dimension>
typename itk::Image<TPixel,Dimension>::Pointer make_image(const itk::Size<Dimension>& size)
{
	typedef itk::Image<TPixel,Dimension> Image;
	typename Image::IndexType index;
	index.Fill(0);
	typename Image::RegionType region;
	region.SetIndex(index);
	region.SetSize(size);
	typename Image::Pointer image = Image::New();
	image->SetRegions(region);
	image->Allocate();
	return image;
}

template <typename TPixel>
typename itk::Image<TPixel,2>::Pointer make_image(int sizeX, int sizeY)
{
	itk::Size<2> size = {{sizeX, sizeY}};
	return make_image<TPixel,2>(size);
}

template <typename TPixel>
typename itk::Image<TPixel,3>::Pointer make_image(int sizeX, int sizeY, int sizeZ)
{
	itk::Size<3> size = {{sizeX, sizeY, sizeZ}};
	return make_image<TPixel,3>(size);
}

template <typename TPixel, unsigned int Dimension>
typename itk::Image<TPixel,Dimension>::Pointer make_filled_image(const itk::Size<Dimension>& size, const TPixel *const pixels)
{
	typename itk::Image<TPixel,Dimension>::Pointer image = make_image<TPixel>(size);
	fill_image(image, pixels);
	return image;
}

template <typename TPixel>
typename itk::Image<TPixel,2>::Pointer make_filled_image(int sizeX, int sizeY, const TPixel *const pixels)
{
	typename itk::Image<TPixel,2>::Pointer image = make_image<TPixel>(sizeX, sizeY);
	fill_image(image, pixels);
	return image;
}

template <typename TPixel>
typename itk::Image<TPixel,3>::Pointer make_filled_image(int sizeX, int sizeY, int sizeZ, const TPixel *const pixels)
{
	typename itk::Image<TPixel,3>::Pointer image = make_image<TPixel>(sizeX, sizeY, sizeZ);
	fill_image(image, pixels);
	return image;
}

template <unsigned int Dimension>
typename itk::Index<Dimension> make_index_from_size(const itk::Size<Dimension>& size)
{
	itk::Index<Dimension> index;
	for(unsigned int i=0; i<Dimension; ++i) index[i] = static_cast<long>(size[i]);
	return index;
}

template <typename TPixel>
void output_image(std::ostream& os, const itk::Image<TPixel,2>& image)
{
	typedef itk::Image<TPixel,2> Image;

	// Note: An index has signed values, whereas a size has unsigned ones. Doing this avoids signed/unsigned mismatch warnings.
	typename Image::IndexType size = make_index_from_size(image.GetLargestPossibleRegion().GetSize());

	typename Image::IndexType index;
	for(index[1]=0; index[1]<size[1]; ++index[1])
	{
		for(index[0]=0; index[0]<size[0]; ++index[0])
		{
			os << image.GetPixel(index) << '\t';
		}
		os << '\n';
	}
}

template <typename TPixel>
void output_image(std::ostream& os, const itk::Image<TPixel,3>& image)
{
	typedef itk::Image<TPixel,3> Image;

	// Note: An index has signed values, whereas a size has unsigned ones. Doing this avoids signed/unsigned mismatch warnings.
	typename Image::IndexType size = make_index_from_size(image.GetLargestPossibleRegion().GetSize());

	typename Image::IndexType index;
	for(index[2]=0; index[2]<size[2]; ++index[2])
	{
		if(index[2] > 0) os << "---\n";
		for(index[1]=0; index[1]<size[1]; ++index[1])
		{
			for(index[0]=0; index[0]<size[0]; ++index[0])
			{
				os << image.GetPixel(index) << '\t';
			}
			os << '\n';
		}
	}
}

template <typename ImagePointer>
void output_image(std::ostream& os, const ImagePointer& image)
{
	output_image(os, *image);
}

}

}

#endif

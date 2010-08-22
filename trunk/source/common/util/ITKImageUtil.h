/***
 * millipede: ITKImageUtil.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_ITKIMAGEUTIL
#define H_MILLIPEDE_ITKIMAGEUTIL

#include <vector>

#include <itkImageRegionIterator.h>
#include <itkPasteImageFilter.h>
#include <itkRGBPixel.h>
#include <itkRGBAPixel.h>

#include <common/slices/SliceOrientation.h>

namespace mp {

//#################### TYPEDEFS ####################
typedef itk::RGBPixel<unsigned char> RGB24;
typedef itk::RGBAPixel<unsigned char> RGBA32;

namespace ITKImageUtil {

//#################### FUNCTIONS ####################
std::vector<itk::Offset<2> > make_4_connected_offsets();
std::vector<itk::Offset<3> > make_4_connected_offsets(SliceOrientation ori);
std::vector<itk::Offset<3> > make_6_connected_offsets();
itk::Index<2> make_index(long x, long y);
itk::Index<3> make_index(long x, long y, long z);
RGB24 make_rgb24(unsigned char r, unsigned char g, unsigned char b);
RGBA32 make_rgba32(unsigned char r, unsigned char g, unsigned char b, unsigned char a);
itk::Vector<double,3> make_vector3d(double x, double y, double z);

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
typename itk::Image<TPixel,Dimension>::Pointer make_bordered_image(const itk::Image<TPixel,Dimension> *source, const TPixel& borderColour,
																   const itk::Index<Dimension>& borderSize)
{
	// Calculate the destination image size.
	itk::Size<Dimension> destSize = source->GetLargestPossibleRegion().GetSize();
	for(int i=0; i<3; ++i) destSize[i] += 2 * borderSize[i];

	// Create the destination image and fill it with the border colour.
	typedef itk::Image<TPixel,Dimension> Image;
	typename Image::Pointer dest = make_image<TPixel,Dimension>(destSize);
	dest->FillBuffer(borderColour);

	// Paste the source image into the destination image at the correct offset.
	typedef itk::PasteImageFilter<Image> Paster;
	typename Paster::Pointer paster = Paster::New();
	paster->InPlaceOn();
	paster->SetSourceImage(const_cast<Image*>(source));
	paster->SetSourceRegion(source->GetLargestPossibleRegion());
	paster->SetDestinationImage(dest);
	paster->SetDestinationIndex(borderSize);
	paster->Update();
	dest = paster->GetOutput();

	return dest;
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

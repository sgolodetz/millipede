/***
 * test-watershed: main.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include <cassert>
#include <iostream>

#include <common/ipfs/construction/Watershed.h>
using namespace mp;

template <typename PixelType>
typename itk::Image<PixelType,2>::Pointer create_2d_image(const PixelType *const pixels, int width, int height)
{
	typedef itk::Image<PixelType,2> Image;
	typedef typename Image::Pointer ImagePointer;
	typedef typename Image::IndexType Index;
	typedef typename Image::RegionType Region;
	typedef typename Image::SizeType Size;

	Index start;
	start.Fill(0);
	Size size;
	size[0] = width;
	size[1] = height;
	Region region;
	region.SetIndex(start);
	region.SetSize(size);
	ImagePointer image = Image::New();
	image->SetRegions(region);
	image->Allocate();

	const PixelType *p = pixels;
	for(int y=0; y<height; ++y)
	{
		for(int x=0; x<width; ++x)
		{
			Index index;
			index[0] = x;
			index[1] = y;
			image->SetPixel(index, *p++);
		}
	}

	return image;
}

template <typename ImagePointer>
void output_2d_image(std::ostream& os, const ImagePointer& image)
{
	// FIXME: This is a hacky bit of test code - not for production use.
	typedef typename ImagePointer::ObjectType Image;
	typedef typename Image::IndexType Index;
	typedef typename Image::SizeType Size;

	assert(Size::GetSizeDimension() == 2);

	const Size& size = image->GetLargestPossibleRegion().GetSize();
	int width = size[0], height = size[1];

	for(int y=0; y<height; ++y)
	{
		for(int x=0; x<width; ++x)
		{
			Index index;
			index[0] = x;
			index[1] = y;
			os << image->GetPixel(index) << '\t';
		}
		os << '\n';
	}
}

int main()
try
{
	typedef itk::Image<signed int,2> Image;

	signed int pixels[] =
	{
		2,2,3,4,4,1,1,
		2,2,3,4,4,2,2,
		4,4,5,5,5,4,6,
		6,6,5,5,5,2,6,
		6,6,5,5,5,2,1
	};

	Image::Pointer function = create_2d_image(pixels, 7, 5);

	output_2d_image(std::cout, function);

	typedef Watershed<signed int,2> WS;
	WS ws(function);

	std::cout << '\n';
	output_2d_image(std::cout, ws.lower_complete());

	std::cout << '\n';
	output_2d_image(std::cout, ws.labels());

	std::cout << '\n';
	output_2d_image(std::cout, ws.arrows());

	return 0;
}
catch(Exception& e)
{
	std::cout << e.cause() << std::endl;
}

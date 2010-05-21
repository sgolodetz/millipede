/***
 * millipede: CTImageUtil.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "CTImageUtil.h"

namespace mp {

namespace CTImageUtil {

boost::shared_ptr<CTImageLeafLayer> make_leaf_layer(const itk::Image<int,2>::Pointer& hounsfieldImage, const itk::Image<unsigned char,2>::Pointer& windowedImage)
{
	assert(hounsfieldImage->GetLargestPossibleRegion().GetSize() == windowedImage->GetLargestPossibleRegion().GetSize());

	typedef itk::Image<int,2> HounsfieldImage;
	typedef itk::Image<unsigned char,2> WindowedImage;

	const HounsfieldImage::SizeType& size = hounsfieldImage->GetLargestPossibleRegion().GetSize();

	std::vector<CTPixelProperties> nodeProperties;
	nodeProperties.reserve(size[0] * size[1]);

	WindowedImage::IndexType windowedIndex;
	HounsfieldImage::IndexType hounsfieldIndex;
	for(long y=0; y<size[1]; ++y)
		for(long x=0; x<size[0]; ++x)
		{
			windowedIndex[0] = hounsfieldIndex[0] = x;
			windowedIndex[1] = hounsfieldIndex[1] = y;
			nodeProperties.push_back(CTPixelProperties(windowedImage->GetPixel(windowedIndex), hounsfieldImage->GetPixel(hounsfieldIndex)));
		}

	return boost::shared_ptr<CTImageLeafLayer>(new CTImageLeafLayer(nodeProperties, size[0], size[1]));
}

boost::shared_ptr<CTImageLeafLayer> make_leaf_layer(const itk::Image<int,3>::Pointer& hounsfieldImage, const itk::Image<unsigned char,3>::Pointer& windowedImage)
{
	assert(hounsfieldImage->GetLargestPossibleRegion().GetSize() == windowedImage->GetLargestPossibleRegion().GetSize());

	typedef itk::Image<int,3> HounsfieldImage;
	typedef itk::Image<unsigned char,3> WindowedImage;

	const HounsfieldImage::SizeType& size = hounsfieldImage->GetLargestPossibleRegion().GetSize();

	std::vector<CTPixelProperties> nodeProperties;
	nodeProperties.reserve(size[0] * size[1] * size[2]);

	WindowedImage::IndexType windowedIndex;
	HounsfieldImage::IndexType hounsfieldIndex;
	for(long z=0; z<size[2]; ++z)
		for(long y=0; y<size[1]; ++y)
			for(long x=0; x<size[0]; ++x)
			{
				windowedIndex[0] = hounsfieldIndex[0] = x;
				windowedIndex[1] = hounsfieldIndex[1] = y;
				windowedIndex[2] = hounsfieldIndex[2] = z;
				nodeProperties.push_back(CTPixelProperties(windowedImage->GetPixel(windowedIndex), hounsfieldImage->GetPixel(hounsfieldIndex)));
			}

	return boost::shared_ptr<CTImageLeafLayer>(new CTImageLeafLayer(nodeProperties, size[0], size[1], size[2]));
}

}

}

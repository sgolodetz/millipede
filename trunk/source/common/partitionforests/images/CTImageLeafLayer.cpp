/***
 * millipede: CTImageLeafLayer.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "CTImageLeafLayer.h"

namespace mp {

//#################### CONSTRUCTORS ####################
CTImageLeafLayer::CTImageLeafLayer(const std::vector<CTPixelProperties>& nodeProperties, int sizeX, int sizeY, int sizeZ)
{
	initialise(nodeProperties, sizeX, sizeY, sizeZ);
}

CTImageLeafLayer::CTImageLeafLayer(const itk::Image<int,2>::Pointer& hounsfieldImage,
								   const itk::Image<unsigned char,2>::Pointer& windowedImage,
								   const itk::Image<int,2>::Pointer& gradientMagnitudeImage)
{
	assert(hounsfieldImage->GetLargestPossibleRegion().GetSize() == windowedImage->GetLargestPossibleRegion().GetSize());

	typedef itk::Image<int,2> GradientMagnitudeImage;
	typedef itk::Image<unsigned char,2> WindowedImage;
	typedef itk::Image<int,2> HounsfieldImage;

	const HounsfieldImage::SizeType& size = hounsfieldImage->GetLargestPossibleRegion().GetSize();

	std::vector<CTPixelProperties> nodeProperties;
	nodeProperties.reserve(size[0] * size[1]);

	GradientMagnitudeImage::IndexType gradientMagnitudeIndex;
	WindowedImage::IndexType windowedIndex;
	HounsfieldImage::IndexType hounsfieldIndex;
	for(size_t y=0; y<size[1]; ++y)
		for(size_t x=0; x<size[0]; ++x)
		{
			gradientMagnitudeIndex[0] = windowedIndex[0] = hounsfieldIndex[0] = x;
			gradientMagnitudeIndex[1] = windowedIndex[1] = hounsfieldIndex[1] = y;
			nodeProperties.push_back(CTPixelProperties(gradientMagnitudeImage->GetPixel(gradientMagnitudeIndex),
													   windowedImage->GetPixel(windowedIndex),
													   hounsfieldImage->GetPixel(hounsfieldIndex)));
		}

	initialise(nodeProperties, size[0], size[1]);
}

CTImageLeafLayer::CTImageLeafLayer(const itk::Image<int,3>::Pointer& hounsfieldImage,
								   const itk::Image<unsigned char,3>::Pointer& windowedImage,
								   const itk::Image<int,3>::Pointer& gradientMagnitudeImage)
{
	assert(hounsfieldImage->GetLargestPossibleRegion().GetSize() == windowedImage->GetLargestPossibleRegion().GetSize());

	typedef itk::Image<int,3> GradientMagnitudeImage;
	typedef itk::Image<unsigned char,3> WindowedImage;
	typedef itk::Image<int,3> HounsfieldImage;

	const HounsfieldImage::SizeType& size = hounsfieldImage->GetLargestPossibleRegion().GetSize();

	std::vector<CTPixelProperties> nodeProperties;
	nodeProperties.reserve(size[0] * size[1] * size[2]);

	GradientMagnitudeImage::IndexType gradientMagnitudeIndex;
	WindowedImage::IndexType windowedIndex;
	HounsfieldImage::IndexType hounsfieldIndex;
	for(size_t z=0; z<size[2]; ++z)
		for(size_t y=0; y<size[1]; ++y)
			for(size_t x=0; x<size[0]; ++x)
			{
				gradientMagnitudeIndex[0] = windowedIndex[0] = hounsfieldIndex[0] = x;
				gradientMagnitudeIndex[1] = windowedIndex[1] = hounsfieldIndex[1] = y;
				gradientMagnitudeIndex[2] = windowedIndex[2] = hounsfieldIndex[2] = z;
				nodeProperties.push_back(CTPixelProperties(gradientMagnitudeImage->GetPixel(gradientMagnitudeIndex),
														   windowedImage->GetPixel(windowedIndex),
														   hounsfieldImage->GetPixel(hounsfieldIndex)));
			}

	initialise(nodeProperties, size[0], size[1], size[2]);
}

//#################### PUBLIC METHODS ####################
// Precondition: has_edge(u, v)
CTImageLeafLayer::EdgeWeight CTImageLeafLayer::edge_weight(int u, int v) const
{
	return std::max(m_nodes[u].properties().gradient_magnitude_value(), m_nodes[v].properties().gradient_magnitude_value());
}

}

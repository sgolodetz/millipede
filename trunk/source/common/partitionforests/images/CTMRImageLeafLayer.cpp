/***
 * millipede: CTMRImageLeafLayer.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "CTMRImageLeafLayer.h"

namespace mp {

//#################### CONSTRUCTORS ####################
CTMRImageLeafLayer::CTMRImageLeafLayer(const std::vector<CTMRPixelProperties>& nodeProperties, int sizeX, int sizeY, int sizeZ)
{
	initialise(nodeProperties, sizeX, sizeY, sizeZ);
}

CTMRImageLeafLayer::CTMRImageLeafLayer(const itk::Image<int,2>::Pointer& baseImage,
									   const itk::Image<unsigned char,2>::Pointer& windowedImage,
									   const itk::Image<short,2>::Pointer& gradientMagnitudeImage)
{
	assert(baseImage->GetLargestPossibleRegion().GetSize() == windowedImage->GetLargestPossibleRegion().GetSize());

	typedef itk::Image<int,2> BaseImage;
	typedef itk::Image<short,2> GradientMagnitudeImage;
	typedef itk::Image<unsigned char,2> WindowedImage;

	const BaseImage::SizeType& size = baseImage->GetLargestPossibleRegion().GetSize();

	std::vector<CTMRPixelProperties> nodeProperties;
	nodeProperties.reserve(size[0] * size[1]);

	BaseImage::IndexType baseIndex;
	GradientMagnitudeImage::IndexType gradientMagnitudeIndex;
	WindowedImage::IndexType windowedIndex;
	for(size_t y=0; y<size[1]; ++y)
		for(size_t x=0; x<size[0]; ++x)
		{
			baseIndex[0] = gradientMagnitudeIndex[0] = windowedIndex[0] = x;
			baseIndex[1] = gradientMagnitudeIndex[1] = windowedIndex[1] = y;
			nodeProperties.push_back(CTMRPixelProperties(baseImage->GetPixel(baseIndex),
														 gradientMagnitudeImage->GetPixel(gradientMagnitudeIndex),
														 windowedImage->GetPixel(windowedIndex)));
		}

	initialise(nodeProperties, size[0], size[1]);
}

CTMRImageLeafLayer::CTMRImageLeafLayer(const itk::Image<int,3>::Pointer& baseImage,
									   const itk::Image<unsigned char,3>::Pointer& windowedImage,
									   const itk::Image<short,3>::Pointer& gradientMagnitudeImage)
{
	assert(baseImage->GetLargestPossibleRegion().GetSize() == windowedImage->GetLargestPossibleRegion().GetSize());

	typedef itk::Image<int,3> BaseImage;
	typedef itk::Image<short,3> GradientMagnitudeImage;
	typedef itk::Image<unsigned char,3> WindowedImage;

	const BaseImage::SizeType& size = baseImage->GetLargestPossibleRegion().GetSize();

	std::vector<CTMRPixelProperties> nodeProperties;
	nodeProperties.reserve(size[0] * size[1] * size[2]);

	BaseImage::IndexType baseIndex;
	GradientMagnitudeImage::IndexType gradientMagnitudeIndex;
	WindowedImage::IndexType windowedIndex;
	for(size_t z=0; z<size[2]; ++z)
		for(size_t y=0; y<size[1]; ++y)
			for(size_t x=0; x<size[0]; ++x)
			{
				baseIndex[0] = gradientMagnitudeIndex[0] = windowedIndex[0] = x;
				baseIndex[1] = gradientMagnitudeIndex[1] = windowedIndex[1] = y;
				baseIndex[2] = gradientMagnitudeIndex[2] = windowedIndex[2] = z;
				nodeProperties.push_back(CTMRPixelProperties(baseImage->GetPixel(baseIndex),
															 gradientMagnitudeImage->GetPixel(gradientMagnitudeIndex),
															 windowedImage->GetPixel(windowedIndex)));
			}

	initialise(nodeProperties, size[0], size[1], size[2]);
}

//#################### PUBLIC METHODS ####################
// Precondition: has_edge(u, v)
CTMRImageLeafLayer::EdgeWeight CTMRImageLeafLayer::edge_weight(int u, int v) const
{
	return std::max(m_nodes[u].properties().gradient_magnitude_value(), m_nodes[v].properties().gradient_magnitude_value());
}

}

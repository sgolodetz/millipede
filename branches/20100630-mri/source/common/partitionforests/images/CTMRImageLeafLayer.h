/***
 * millipede: CTMRImageLeafLayer.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_CTMRIMAGELEAFLAYER
#define H_MILLIPEDE_CTMRIMAGELEAFLAYER

#include <itkImage.h>

#include "CTMRPixelProperties.h"
#include "CTMRRegionProperties.h"
#include "ImageLeafLayer.h"

namespace mp {

class CTMRImageLeafLayer : public ImageLeafLayer<CTMRPixelProperties,CTMRRegionProperties>
{
	//#################### CONSTRUCTORS ####################
public:
	CTMRImageLeafLayer(const std::vector<CTMRPixelProperties>& nodeProperties, int sizeX, int sizeY, int sizeZ = 1);
	CTMRImageLeafLayer(const itk::Image<int,2>::Pointer& hounsfieldImage,
					   const itk::Image<unsigned char,2>::Pointer& windowedImage,
					   const itk::Image<short,2>::Pointer& gradientMagnitudeImage);
	CTMRImageLeafLayer(const itk::Image<int,3>::Pointer& hounsfieldImage,
					   const itk::Image<unsigned char,3>::Pointer& windowedImage,
					   const itk::Image<short,3>::Pointer& gradientMagnitudeImage);

	//#################### PUBLIC METHODS ####################
public:
	EdgeWeight edge_weight(int u, int v) const;
};

}

#endif

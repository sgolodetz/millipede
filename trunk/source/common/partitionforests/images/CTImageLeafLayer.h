/***
 * millipede: CTImageLeafLayer.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_CTIMAGELEAFLAYER
#define H_MILLIPEDE_CTIMAGELEAFLAYER

#include <itkImage.h>

#include "CTPixelProperties.h"
#include "CTRegionProperties.h"
#include "ImageLeafLayer.h"

namespace mp {

class CTImageLeafLayer : public ImageLeafLayer<CTPixelProperties,CTRegionProperties>
{
	//#################### CONSTRUCTORS ####################
public:
	CTImageLeafLayer(const std::vector<CTPixelProperties>& nodeProperties, int sizeX, int sizeY, int sizeZ = 1);
	CTImageLeafLayer(const itk::Image<int,2>::Pointer& hounsfieldImage,
					 const itk::Image<unsigned char,2>::Pointer& windowedImage,
					 const itk::Image<int,2>::Pointer& gradientMagnitudeImage);
	CTImageLeafLayer(const itk::Image<int,3>::Pointer& hounsfieldImage,
					 const itk::Image<unsigned char,3>::Pointer& windowedImage,
					 const itk::Image<int,3>::Pointer& gradientMagnitudeImage);

	//#################### PUBLIC METHODS ####################
public:
	EdgeWeight edge_weight(int u, int v) const;
};

}

#endif

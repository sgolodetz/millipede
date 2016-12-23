/***
 * millipede: DICOMImageLeafLayer.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_DICOMIMAGELEAFLAYER
#define H_MILLIPEDE_DICOMIMAGELEAFLAYER

#include <itkImage.h>

#include "DICOMPixelProperties.h"
#include "DICOMRegionProperties.h"
#include "ImageLeafLayer.h"

namespace mp {

class DICOMImageLeafLayer : public ImageLeafLayer<DICOMPixelProperties,DICOMRegionProperties>
{
	//#################### CONSTRUCTORS ####################
public:
	DICOMImageLeafLayer(const std::vector<DICOMPixelProperties>& nodeProperties, int sizeX, int sizeY, int sizeZ = 1);
	DICOMImageLeafLayer(const itk::Image<int,2>::Pointer& baseImage,
						const itk::Image<unsigned char,2>::Pointer& windowedImage,
						const itk::Image<short,2>::Pointer& gradientMagnitudeImage);
	DICOMImageLeafLayer(const itk::Image<int,3>::Pointer& baseImage,
						const itk::Image<unsigned char,3>::Pointer& windowedImage,
						const itk::Image<short,3>::Pointer& gradientMagnitudeImage);

	//#################### PUBLIC METHODS ####################
public:
	EdgeWeight edge_weight(int u, int v) const;
};

}

#endif

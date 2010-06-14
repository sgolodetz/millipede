/***
 * millipede: SimpleImageLeafLayer.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SIMPLEIMAGELEAFLAYER
#define H_MILLIPEDE_SIMPLEIMAGELEAFLAYER

#include "ImageLeafLayer.h"
#include "SimplePixelProperties.h"
#include "SimpleRegionProperties.h"

namespace mp {

class SimpleImageLeafLayer : public ImageLeafLayer<SimplePixelProperties,SimpleRegionProperties>
{
	//#################### CONSTRUCTORS ####################
public:
	SimpleImageLeafLayer(const std::vector<SimplePixelProperties>& nodeProperties, int sizeX, int sizeY, int sizeZ = 1);

	//#################### PUBLIC METHODS ####################
public:
	EdgeWeight edge_weight(int u, int v) const;
};

}

#endif

/***
 * millipede: TestImageForestLayers.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_TESTIMAGEFORESTLAYERS
#define H_MILLIPEDE_TESTIMAGEFORESTLAYERS

#include "ImageBranchLayer.h"
#include "ImageLeafLayer.h"
#include "TestImageNodeProperties.h"

namespace mp {

struct TestImageLeafLayer : ImageLeafLayer<TestPixelProperties,TestRegionProperties>
{
	//#################### CONSTRUCTORS ####################
	TestImageLeafLayer(const std::vector<TestPixelProperties>& nodeProperties, int sizeX, int sizeY, int sizeZ = 1)
	{
		initialise(nodeProperties, sizeX, sizeY, sizeZ);
	}

	//#################### PUBLIC METHODS ####################
	EdgeWeight edge_weight(int u, int v) const
	{
		return abs(u - v);
	}
};

typedef ImageBranchLayer<TestRegionProperties> TestImageBranchLayer;

}

#endif

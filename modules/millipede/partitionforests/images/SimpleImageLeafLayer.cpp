/***
 * millipede: SimpleImageLeafLayer.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "SimpleImageLeafLayer.h"

namespace mp {

//#################### CONSTRUCTORS ####################
SimpleImageLeafLayer::SimpleImageLeafLayer(const std::vector<SimplePixelProperties>& nodeProperties, int sizeX, int sizeY, int sizeZ)
{
	initialise(nodeProperties, sizeX, sizeY, sizeZ);
}

//#################### PUBLIC METHODS ####################
// Precondition: has_edge(u, v)
SimpleImageLeafLayer::EdgeWeight SimpleImageLeafLayer::edge_weight(int u, int v) const
{
	return abs(u - v);
}

}

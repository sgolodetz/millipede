/***
 * millipede: VoxelProperties.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VOXELPROPERTIES
#define H_MILLIPEDE_VOXELPROPERTIES

namespace mp {

struct VoxelProperties
{
	int greyValue;

	VoxelProperties(int greyValue_) : greyValue(greyValue_) {}
};

}

#endif

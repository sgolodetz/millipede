/***
 * millipede: Voxel.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_VOXEL
#define H_MILLIPEDE_VOXEL

namespace mp {

struct Voxel
{
	int x, y, z;

	Voxel(int x_, int y_, int z_)
	:	x(x_), y(y_), z(z_)
	{}
};

}

#endif

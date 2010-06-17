/***
 * millipede: SliceLocation.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SLICELOCATION
#define H_MILLIPEDE_SLICELOCATION

namespace mp {

struct SliceLocation
{
	int x, y, z, layer;

	SliceLocation(int x_, int y_, int z_, int layer_)
	:	x(x_), y(y_), z(z_), layer(layer_)
	{}

	bool operator==(const SliceLocation& rhs) const
	{
		return x == rhs.x && y == rhs.y && z == rhs.z && layer == rhs.layer;
	}

	bool operator!=(const SliceLocation& rhs) const
	{
		return !(*this == rhs);
	}
};

}

#endif

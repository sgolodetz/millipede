/***
 * millipede: SliceLocation.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SLICELOCATION
#define H_MILLIPEDE_SLICELOCATION

#include <common/exceptions/Exception.h>
#include "SliceOrientation.h"

namespace mp {

struct SliceLocation
{
	int x, y, z, layer;

	SliceLocation(int x_, int y_, int z_, int layer_)
	:	x(x_), y(y_), z(z_), layer(layer_)
	{}

	int& operator[](SliceOrientation ori)
	{
		return const_cast<int&>(const_cast<const SliceLocation*>(this)->operator[](ori));
	}

	const int& operator[](SliceOrientation ori) const
	{
		switch(ori)
		{
			case ORIENT_XY:	return z;
			case ORIENT_XZ:	return y;
			case ORIENT_YZ:	return x;
			default:		throw Exception("Unexpected slice orientation");
		}
	}

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

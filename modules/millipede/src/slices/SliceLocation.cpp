/***
 * millipede: SliceLocation.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "slices/SliceLocation.h"

#include "exceptions/Exception.h"

namespace mp {

//#################### CONSTRUCTORS ####################
SliceLocation::SliceLocation(int x_, int y_, int z_, int layer_)
:	x(x_), y(y_), z(z_), layer(layer_)
{}

//#################### PUBLIC OPERATORS ####################
int& SliceLocation::operator[](SliceOrientation ori)
{
	return const_cast<int&>(const_cast<const SliceLocation*>(this)->operator[](ori));
}

const int& SliceLocation::operator[](SliceOrientation ori) const
{
	switch(ori)
	{
		case ORIENT_XY:	return z;
		case ORIENT_XZ:	return y;
		case ORIENT_YZ:	return x;
		default:		throw Exception("Unexpected slice orientation");
	}
}

bool SliceLocation::operator==(const SliceLocation& rhs) const
{
	return x == rhs.x && y == rhs.y && z == rhs.z && layer == rhs.layer;
}

bool SliceLocation::operator!=(const SliceLocation& rhs) const
{
	return !(*this == rhs);
}

}

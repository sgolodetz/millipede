/***
 * millipede: SliceLocation.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SLICELOCATION
#define H_MILLIPEDE_SLICELOCATION

#include "SliceOrientation.h"

namespace mp {

struct SliceLocation
{
	//#################### PUBLIC VARIABLES ####################
	int x, y, z, layer;

	//#################### CONSTRUCTORS ####################
	SliceLocation(int x_, int y_, int z_, int layer_);

	//#################### PUBLIC OPERATORS ####################
	int& operator[](SliceOrientation ori);
	const int& operator[](SliceOrientation ori) const;
	bool operator==(const SliceLocation& rhs) const;
	bool operator!=(const SliceLocation& rhs) const;
};

}

#endif

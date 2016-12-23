/***
 * millipede: GridUtil.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "util/GridUtil.h"

namespace mp {

namespace GridUtil {

int x_of(int n, int sizeX)
{
	return n % sizeX;
}

int y_of(int n, int sizeX, int sizeY)
{
	return (n / sizeX) % sizeY;
}

int z_of(int n, int sizeXY)
{
	return n / sizeXY;
}

}

}

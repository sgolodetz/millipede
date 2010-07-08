/***
 * millipede: MeshTriangle.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "MeshTriangle.h"

namespace mp {

//#################### CONSTRUCTORS ####################
MeshTriangle::MeshTriangle(int index0, int index1, int index2)
{
	indices[0] = index0;
	indices[1] = index1;
	indices[2] = index2;
}

}

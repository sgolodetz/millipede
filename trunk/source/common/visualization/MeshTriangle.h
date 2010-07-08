/***
 * millipede: MeshTriangle.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MESHTRIANGLE
#define H_MILLIPEDE_MESHTRIANGLE

namespace mp {

struct MeshTriangle
{
	//#################### PUBLIC VARIABLES ####################
	int indices[3];	///< indices into the global node table

	//#################### CONSTRUCTORS ####################
	MeshTriangle(int index0, int index1, int index2);
};

}

#endif

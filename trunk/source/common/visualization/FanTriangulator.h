/***
 * millipede: FanTriangulator.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_FANTRIANGULATOR
#define H_MILLIPEDE_FANTRIANGULATOR

#include "MeshTriangle.h"

namespace mp {

template <typename Label>
class FanTriangulator
{
	//#################### PRIVATE VARIABLES ####################
private:
	int m_anchorIndex;

	//#################### CONSTRUCTORS ####################
public:
	explicit FanTriangulator(int anchorIndex)
	:	m_anchorIndex(anchorIndex)
	{}

	//#################### PUBLIC METHODS ####################
public:
	std::list<MeshTriangle> triangulate(const std::vector<int>& nodeLoop) const
	{
		std::list<MeshTriangle> triangles;

		// NYI
		return triangles;
	}
};

}

#endif

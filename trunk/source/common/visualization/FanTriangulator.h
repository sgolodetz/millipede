/***
 * millipede: FanTriangulator.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_FANTRIANGULATOR
#define H_MILLIPEDE_FANTRIANGULATOR

#include "MeshTriangle.h"
#include "NodeLoop.h"

namespace mp {

template <typename Label>
class FanTriangulator
{
	//#################### TYPEDEFS ####################
private:
	typedef MeshTriangle<Label> MeshTriangleT;
	typedef NodeLoop<Label> NodeLoopT;

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
	std::list<MeshTriangleT> triangulate(const NodeLoopT& nodeLoop) const
	{
		std::list<MeshTriangleT> triangles;

		// NYI
		return triangles;
	}
};

}

#endif

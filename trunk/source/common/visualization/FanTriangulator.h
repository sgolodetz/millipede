/***
 * millipede: FanTriangulator.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_FANTRIANGULATOR
#define H_MILLIPEDE_FANTRIANGULATOR

#include "MeshTriangle.h"
#include "NodeLoop.h"

namespace mp {

/**
@brief	A FanTriangulator triangulates non-planar node loops as a triangle fan emanating from an anchor node.
		It is only appropriate when a suitable anchor node is available.

@tparam	Label	The type of label stored in the mesh nodes
*/
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
		/*
		Examples:

		(0-1-2-3-4, 0) or (1-2-3-4-0, 0) or (3-4-0-1-2, 0) -> (012, 023, 034)

		Algorithm:

		Consider consecutive pairs of nodes, e.g. (0-1), (1-2), (2-3), (3-4), (4-0).
		In each case, if neither node is the anchor index, add a triangle.
		*/

		std::list<MeshTriangleT> triangles;

		for(int i=0, size=nodeLoop.size(); i<size; ++i)
		{
			int j = (i+1) % size;
			if(nodeLoop.index(i) != m_anchorIndex && nodeLoop.index(j) != m_anchorIndex)
			{
				triangles.push_back(MeshTriangleT(m_anchorIndex, nodeLoop.index(i), nodeLoop.index(j), nodeLoop.labels()));
			}
		}

		return triangles;
	}
};

}

#endif

/***
 * millipede: Mesh.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MESH
#define H_MILLIPEDE_MESH

#include <list>
#include <vector>

#include <boost/shared_ptr.hpp>

#include "MeshNode.h"
#include "MeshTriangle.h"

namespace mp {

template <typename Label>
class Mesh
{
	//#################### TYPEDEFS ####################
private:
	typedef MeshNode<Label> MeshNodeT;
	typedef std::vector<MeshNodeT> MeshNodeVector;
	typedef boost::shared_ptr<MeshNodeVector> MeshNodeVector_Ptr;
	typedef MeshTriangle<Label> MeshTriangleT;
	typedef std::list<MeshTriangleT> MeshTriangleList;
	typedef boost::shared_ptr<MeshTriangleList> MeshTriangleList_Ptr;

	//#################### PRIVATE VARIABLES ####################
private:
	MeshNodeVector_Ptr m_nodes;
	MeshTriangleList_Ptr m_triangles;

	//#################### CONSTRUCTORS ####################
public:
	Mesh(const MeshNodeVector_Ptr& nodes, const MeshTriangleList_Ptr& triangles)
	:	m_nodes(nodes), m_triangles(triangles)
	{}

	//#################### PUBLIC METHODS ####################
public:
	MeshNodeVector& nodes()
	{
		return m_nodes;
	}

	MeshTriangleList& triangles()
	{
		return m_triangles;
	}
};

}

#endif

/***
 * millipede: SchroederTriangulator.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SCHROEDERTRIANGULATOR
#define H_MILLIPEDE_SCHROEDERTRIANGULATOR

#include <list>

#include "GlobalNodeTable.h"
#include "MeshTriangle.h"

namespace mp {

template <typename Label>
class SchroederTriangulator
{
	//#################### TYPEDEFS ####################
private:
	typedef GlobalNodeTable<Label> GlobalNodeTableT;
	typedef std::vector<int> NodeLoop;
	typedef std::pair<NodeLoop,NodeLoop> Split;

	//#################### PRIVATE VARIABLES ####################
private:
	const GlobalNodeTableT& m_globalNodeTable;

	//#################### CONSTRUCTORS ####################
public:
	explicit SchroederTriangulator(const GlobalNodeTableT& globalNodeTable)
	:	m_globalNodeTable(globalNodeTable)
	{}

	//#################### PUBLIC METHODS ####################
public:
	std::list<MeshTriangle> triangulate(const NodeLoop& nodeLoop) const
	{
		std::list<MeshTriangle> triangles;

		size_t nodeCount = nodeLoop.size();
		if(nodeCount == 3)
		{
			// If there are only three nodes, there's only one possible triangulation (bar winding order, which is dealt with elsewhere).
			triangles.push_back(make_triangle(nodeLoop[0], nodeLoop[1], nodeLoop[2]));
		}
		else
		{
#if 0
			Split loopHalves = split_node_loop(nodeLoop);

			std::list<MeshTriangle> result = triangulate(loopHalves.first);
			triangles.splice(triangles.end(), result);

			result = triangulate(loopHalves.second);
			triangles.splice(triangles.end(), result);
#endif
		}

		return triangles;
	}

	//#################### PRIVATE METHODS ####################
private:
	MeshTriangle make_triangle(int index0, int index1, int index2) const
	{
		int indices[3] = {index0,index1,index2};
		Vector3d p[3];
		for(int i=0; i<3; ++i) p[i] = m_globalNodeTable(indices[i]).position;
		Vector3d a = p[1] - p[0];
		Vector3d b = p[2] - p[0];
		Vector3d normal = a.cross(b);
		if(normal.length() >= MathConstants::SMALL_EPSILON) normal.normalize();
		return MeshTriangle(index0, index1, index2, normal);
	}

	Split split_node_loop(const NodeLoop& nodeLoop) const
	{
		// NYI
		throw 23;
	}
};

}

#endif

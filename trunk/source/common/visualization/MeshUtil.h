/***
 * millipede: MeshUtil.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MESHUTIL
#define H_MILLIPEDE_MESHUTIL

#include <common/math/Plane.h>
#include "GlobalNodeTable.h"

namespace mp {

namespace MeshUtil {

/**
@brief	Determine an "average plane" for the node loop specified. This is most easily defined in terms of
		an imaginary average node at the centre of the node loop.

@param[in]	nodeLoop			The node loop whose average plane should be calculated
@param[in]	globalNodeTable		The global node table used throughout the mesh building process
@return	The average plane
*/
template <typename Label>
Plane calculate_average_plane(const std::vector<int>& nodeLoop, const GlobalNodeTable<Label>& globalNodeTable)
{
	int nodeCount = static_cast<int>(nodeLoop.size());

	// Calculate the imaginary average node.
	Vector3d centre;
	for(int i=0; i<nodeCount; ++i)
	{
		centre += globalNodeTable(nodeLoop[i]).position;
	}
	centre /= nodeCount;

	// Calculate the average plane normal as a weighted sum of the normals of the triangles surrounding the average node.
	Vector3d avgPlaneNormal;
	for(int i=0; i<nodeCount; ++i)
	{
		int j = (i+1)%nodeCount;
		const Vector3d& u = globalNodeTable(nodeLoop[i]).position;
		const Vector3d& v = globalNodeTable(nodeLoop[j]).position;
		Vector3d n = (u-centre).cross(v-centre);
		double area = n.length() / 2;	// the area of the triangle centre-u-v
		n.normalize();
		avgPlaneNormal += area * n;
	}
	avgPlaneNormal.normalize();

	return Plane(avgPlaneNormal, centre);
}

}

}

#endif

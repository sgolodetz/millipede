/***
 * millipede: MeshUtil.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MESHUTIL
#define H_MILLIPEDE_MESHUTIL

#include <common/math/Plane.h>
#include "GlobalNodeTable.h"
#include "MeshTriangle.h"
#include "NodeLoop.h"

namespace mp {

namespace MeshUtil {

/**
@brief	Determine an "average plane" for the node loop specified. This is most easily defined in terms of
		an imaginary average node at the centre of the node loop.

@param[in]	nodeLoop			The node loop whose average plane should be calculated
@param[in]	globalNodeTable		The global node table that stores the actual nodes
@return	The average plane
*/
template <typename Label>
Plane calculate_average_plane(const NodeLoop<Label>& nodeLoop, const GlobalNodeTable<Label>& globalNodeTable)
{
	int nodeCount = nodeLoop.size();

	// Calculate the imaginary average node.
	Vector3d centre;
	for(int i=0; i<nodeCount; ++i)
	{
		centre += globalNodeTable(nodeLoop.index(i)).position();
	}
	centre /= nodeCount;

	// Calculate the average plane normal as a weighted sum of the normals of the triangles surrounding the average node.
	Vector3d avgPlaneNormal;
	for(int i=0; i<nodeCount; ++i)
	{
		int j = (i+1)%nodeCount;
		const Vector3d& u = globalNodeTable(nodeLoop.index(i)).position();
		const Vector3d& v = globalNodeTable(nodeLoop.index(j)).position();
		Vector3d n = (u-centre).cross(v-centre);
		double area = n.length() / 2;	// the area of the triangle centre-u-v
		n.normalize();
		avgPlaneNormal += area * n;
	}
	avgPlaneNormal.normalize();

	return Plane(avgPlaneNormal, centre);
}

/**
@brief	Calculates the normal of the specified mesh triangle. Counter-clockwise winding order is assumed.

@param[in]	tri					The mesh triangle whose normal is to be calculated
@param[in]	globalNodeTable		The global node table that stores the actual nodes
@return	The normal
*/
template <typename Label>
Vector3d calculate_normal(const MeshTriangle<Label>& tri, const GlobalNodeTable<Label>& globalNodeTable)
{
	Vector3d p[3];
	for(int i=0; i<3; ++i) p[i] = globalNodeTable(tri.index(i)).position();
	Vector3d a = p[1] - p[0];
	Vector3d b = p[2] - p[0];
	Vector3d normal = a.cross(b);
	if(normal.length() >= MathConstants::SMALL_EPSILON) normal.normalize();
	return normal;
}

/**
@brief	Classifies a node loop against a plane.

@param[in]	nodeLoop			The node loop to be classified
@param[in]	plane				The plane against which to classify it
@param[in]	globalNodeTable		The global node table that stores the actual nodes
@return	A PlaneClassification::Enum containing the result of the classification
*/
template <typename Label>
PlaneClassification::Enum classify_node_loop_against_plane(const NodeLoop<Label>& nodeLoop, const Plane& plane,
														   const GlobalNodeTable<Label>& globalNodeTable)
{
	int backCount = 0, frontCount = 0;

	for(int i=0, size=nodeLoop.size(); i<size; ++i)
	{
		switch(plane.classify_point(globalNodeTable(nodeLoop.index(i)).position()))
		{
			case PlaneClassification::BACK:
				++backCount;
				break;
			case PlaneClassification::COPLANAR:
				break;
			case PlaneClassification::FRONT:
				++frontCount;
				break;
			default:
				// STRADDLE can't happen (it's a point!), but this keeps the compiler happier.
				break;
		}
		if(backCount && frontCount) return PlaneClassification::STRADDLE;
	}

	// If we get here, the node loop doesn't straddle the plane (we'd have returned via the early-out above).
	// Thus either one, or neither, of backCount and frontCount can be non-zero here.
	if(backCount) return PlaneClassification::BACK;
	else if(frontCount) return PlaneClassification::FRONT;
	else return PlaneClassification::COPLANAR;
}

}

}

#endif

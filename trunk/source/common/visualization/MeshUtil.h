/***
 * millipede: MeshUtil.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MESHUTIL
#define H_MILLIPEDE_MESHUTIL

#include <algorithm>

#include <boost/optional.hpp>

#include <common/math/Plane.h>
#include "MeshNode.h"
#include "MeshTriangle.h"
#include "NodeLoop.h"

namespace mp {

namespace MeshNodeType {

enum Enum
{
	SIMPLE,
	EDGE,
	CORNER,
};

}

namespace MeshUtil {

/**
@brief	Determine an "average plane" for the node loop specified. This is most easily defined in terms of
		an imaginary average node at the centre of the node loop.

@param[in]	nodeLoop	The node loop whose average plane should be calculated
@param[in]	nodes		The master array containing the actual nodes referred to by the node loop
@return	The average plane
*/
template <typename Label>
Plane calculate_average_plane(const NodeLoop<Label>& nodeLoop, const std::vector<MeshNode<Label> >& nodes)
{
	int nodeCount = nodeLoop.size();

	// Calculate the imaginary average node.
	Vector3d centre;
	for(int i=0; i<nodeCount; ++i)
	{
		centre += nodes[nodeLoop.index(i)].position();
	}
	centre /= nodeCount;

	// Calculate the average plane normal as a weighted sum of the normals of the triangles surrounding the average node.
	Vector3d avgPlaneNormal;
	for(int i=0; i<nodeCount; ++i)
	{
		int j = (i+1)%nodeCount;
		const Vector3d& u = nodes[nodeLoop.index(i)].position();
		const Vector3d& v = nodes[nodeLoop.index(j)].position();
		Vector3d n = (u-centre).cross(v-centre);
		double area = n.length() / 2;	// the area of the triangle centre-u-v
		n.normalize();
		avgPlaneNormal += area * n;
	}
	avgPlaneNormal.normalize();

	return Plane(avgPlaneNormal, centre);
}

/**
@brief	Calculates the *unit* normal of the specified mesh triangle. Counter-clockwise winding order is assumed.

@param[in]	tri		The mesh triangle whose normal is to be calculated
@param[in]	nodes	The master array containing the actual nodes referred to by the mesh triangle
@return	The normal
*/
template <typename Label>
Vector3d calculate_normal(const MeshTriangle<Label>& tri, const std::vector<MeshNode<Label> >& nodes)
{
	Vector3d normal = calculate_unnormalized_normal(tri, nodes);
	if(normal.length() >= MathConstants::SMALL_EPSILON) normal.normalize();
	return normal;
}

/**
@brief	Calculates the *unnormalized* normal of the specified mesh triangle. Counter-clockwise winding order is assumed.

@param[in]	tri		The mesh triangle whose normal is to be calculated
@param[in]	nodes	The master array containing the actual nodes referred to by the mesh triangle
@return	The normal
*/
template <typename Label>
Vector3d calculate_unnormalized_normal(const MeshTriangle<Label>& tri, const std::vector<MeshNode<Label> >& nodes)
{
	Vector3d p[3];
	for(int i=0; i<3; ++i) p[i] = nodes[tri.index(i)].position();
	Vector3d a = p[1] - p[0];
	Vector3d b = p[2] - p[0];
	return a.cross(b);
}

/**
@brief	Determines the type of a mesh node.

@param[in]	i						The index of the node to be classified
@param[in]	nodes					The master array containing the actual nodes
@param[out]	laplacianNeighbours		The neighbours of this node to be used for Laplacian smoothing
@return	The type of the mesh node
*/
template <typename Label>
MeshNodeType::Enum classify_node(int i, const std::vector<MeshNode<Label> >& nodes, boost::optional<std::set<int>&> laplacianNeighbours)
{
	MeshNodeType::Enum nodeType = MeshNodeType::SIMPLE;

	if(nodes[i].label_count() == 2)
	{
		// This is a simple node.
		if(laplacianNeighbours)
		{
			*laplacianNeighbours = nodes[i].adjacent_nodes();
		}
	}
	else
	{
		// Count the number of adjacent nodes with at least the same labels as this one.
		// Iff it's equal to two, this is an edge node. Otherwise, it's a corner.
		int edgeCriterion = 0;
		std::set<int> labels = nodes[i].labels();
		const std::set<int>& adjacentNodes = nodes[i].adjacent_nodes();
		for(std::set<int>::const_iterator jt=adjacentNodes.begin(), jend=adjacentNodes.end(); jt!=jend; ++jt)
		{
			std::set<int> adjacentLabels = nodes[*jt].labels();
			std::set<int> commonLabels;
			std::set_intersection(labels.begin(), labels.end(), adjacentLabels.begin(), adjacentLabels.end(), std::inserter(commonLabels, commonLabels.begin()));
			if(commonLabels.size() == labels.size())
			{
				++edgeCriterion;
				if(laplacianNeighbours)
				{
					laplacianNeighbours->insert(*jt);
				}
			}
		}

		if(edgeCriterion == 2) nodeType = MeshNodeType::EDGE;
		else nodeType = MeshNodeType::CORNER;
	}

	return nodeType;
}

/**
@brief	Classifies a node loop against a plane.

@param[in]	nodeLoop	The node loop to be classified
@param[in]	plane		The plane against which to classify it
@param[in]	nodes		The master array containing the actual nodes referred to by the node loop
@return	A PlaneClassification::Enum containing the result of the classification
*/
template <typename Label>
PlaneClassification::Enum classify_node_loop_against_plane(const NodeLoop<Label>& nodeLoop, const Plane& plane,
														   const std::vector<MeshNode<Label> >& nodes)
{
	int backCount = 0, frontCount = 0;

	for(int i=0, size=nodeLoop.size(); i<size; ++i)
	{
		switch(plane.classify_point(nodes[nodeLoop.index(i)].position()))
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

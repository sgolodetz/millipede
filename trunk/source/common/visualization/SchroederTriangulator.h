/***
 * millipede: SchroederTriangulator.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SCHROEDERTRIANGULATOR
#define H_MILLIPEDE_SCHROEDERTRIANGULATOR

#include <cassert>
#include <list>

#include "GlobalNodeTable.h"
#include "MeshTriangle.h"
#include "MeshUtil.h"

namespace mp {

/**
@brief	A SchroederTriangulator triangulates non-planar node loops using the recursive, divide-and-conquer approach
		described in "Decimation of Triangle Meshes", Schroeder et al., 1992.

@tparam	Label	The type of label stored in the nodes
*/
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
			Split loopHalves = split_node_loop(nodeLoop);

			std::list<MeshTriangle> result = triangulate(loopHalves.first);
			triangles.splice(triangles.end(), result);

			result = triangulate(loopHalves.second);
			triangles.splice(triangles.end(), result);
		}

		return triangles;
	}

	//#################### PRIVATE METHODS ####################
private:
	PlaneClassification::Enum classify_node_loop_against_plane(const NodeLoop& nodeLoop, const Plane& plane) const
	{
		int backCount = 0, frontCount = 0;

		for(size_t i=0, size=nodeLoop.size(); i<size; ++i)
		{
			switch(plane.classify_point(m_globalNodeTable(nodeLoop[i]).position))
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

	/**
	@brief	Constructs a split of the specified node loop into two halves.

	@param[in]	nodeLoop	The node loop
	@param[in]	e0			The first endpoint of the split line
	@param[in]	e1			The second endpoint of the split line
	@return	The constructed split (namely a pair of node loops representing the two halves)
	*/
	static Split construct_split(const NodeLoop& nodeLoop, int e0, int e1)
	{
		Split split;

		int nodeCount = static_cast<int>(nodeLoop.size());

		for(int i=e0; i!=e1; i=(i+1)%nodeCount) split.first.push_back(nodeLoop[i]);
		split.first.push_back(nodeLoop[e1]);

		for(int i=e1; i!=e0; i=(i+1)%nodeCount) split.second.push_back(nodeLoop[i]);
		split.second.push_back(nodeLoop[e0]);

		return split;
	}

	std::pair<bool,double> evaluate_split(const NodeLoop& nodeLoop, int e0, int e1, const Split& split, const Vector3d& avgPlaneNormal) const
	{
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		// Step 1:	Construct the split plane.
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		// We need to obtain two vectors lying in the plane: one is the split line, the other is the average plane normal.
		const Vector3d& u = m_globalNodeTable(nodeLoop[e0]).position;
		const Vector3d& v = m_globalNodeTable(nodeLoop[e1]).position;
		Vector3d splitLine = v - u;

		// These two vectors can be used to calculate the split plane normal (provided they're not parallel, which is unlikely
		// due to the way we originally constructed the average plane).
		Vector3d splitPlaneNormal = splitLine.cross(avgPlaneNormal);
		if(splitPlaneNormal.length_squared() < MathConstants::EPSILON) return std::make_pair(false, 0);

		// Construct the plane from its normal and either of the two points (u or v) lying in it.
		Plane splitPlane(splitPlaneNormal, u);
		assert(splitPlane.classify_point(u) == PlaneClassification::COPLANAR);
		assert(splitPlane.classify_point(v) == PlaneClassification::COPLANAR);

		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		// Step 2:	Check that the two half-polygons are separated by the split plane.
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		PlaneClassification::Enum cp0 = classify_node_loop_against_plane(split.first, splitPlane);
		if(cp0 == PlaneClassification::COPLANAR || cp0 == PlaneClassification::STRADDLE) return std::make_pair(false, 0);
		PlaneClassification::Enum cp1 = classify_node_loop_against_plane(split.second, splitPlane);
		if(cp1 == PlaneClassification::COPLANAR || cp1 == PlaneClassification::STRADDLE || cp1 == cp0) return std::make_pair(false, 0);

		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		// Step 3:	Calculate the metric (see the referenced paper).
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		double minDistance = INT_MAX;
		for(int i=0, size=nodeLoop.size(); i<size; ++i)
		{
			double distance = splitPlane.distance_to_point(m_globalNodeTable(nodeLoop[i]).position);
			if(distance > MathConstants::SMALL_EPSILON && distance < minDistance) minDistance = distance;
		}

		return std::make_pair(true, minDistance / splitLine.length());
	}

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
		Vector3d avgPlaneNormal = MeshUtil::calculate_average_plane(nodeLoop, m_globalNodeTable).normal();

		bool splitFound = false;
		Split bestSplit;
		double bestMetric = 0;	// metric values are guaranteed to be +ve and we take the split with the largest metric value

		int nodeCount = static_cast<int>(nodeLoop.size());

		for(int i=0; i<nodeCount-2; ++i)
		{
			for(int j=i+2; j<nodeCount; ++j)
			{
				if(i == 0 && j == nodeCount-1) continue;	// avoid trying to split along the edge between the first and last vertices

				Split split = construct_split(nodeLoop, i, j);
				std::pair<bool,double> result = evaluate_split(nodeLoop, i, j, split, avgPlaneNormal);
				if(result.first && result.second > bestMetric)
				{
					splitFound = true;
					bestSplit = split;
					bestMetric = result.second;
				}
			}
		}

		if(splitFound) return bestSplit;
		else throw Exception("Unable to find an appropriate split line");
	}
};

}

#endif

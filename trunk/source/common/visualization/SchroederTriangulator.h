/***
 * millipede: SchroederTriangulator.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SCHROEDERTRIANGULATOR
#define H_MILLIPEDE_SCHROEDERTRIANGULATOR

#include <cassert>
#include <list>

#include "MeshNode.h"
#include "MeshTriangle.h"
#include "MeshUtil.h"

namespace mp {

/**
@brief	A SchroederTriangulator triangulates non-planar node loops using the recursive, divide-and-conquer approach
		described in "Decimation of Triangle Meshes", Schroeder et al., 1992.

@tparam	Label	The type of label stored in the mesh nodes
*/
template <typename Label>
class SchroederTriangulator
{
	//#################### TYPEDEFS ####################
private:
	typedef MeshNode<Label> MeshNodeT;
	typedef std::vector<MeshNodeT> MeshNodeVector;
	typedef MeshTriangle<Label> MeshTriangleT;
	typedef NodeLoop<Label> NodeLoopT;
	typedef std::pair<NodeLoopT,NodeLoopT> Split;

	//#################### PRIVATE VARIABLES ####################
private:
	const MeshNodeVector& m_nodes;

	//#################### CONSTRUCTORS ####################
public:
	explicit SchroederTriangulator(const MeshNodeVector& nodes)
	:	m_nodes(nodes)
	{}

	//#################### PUBLIC METHODS ####################
public:
	std::list<MeshTriangleT> triangulate(const NodeLoopT& nodeLoop) const
	{
		std::list<MeshTriangleT> triangles;

		if(nodeLoop.size() == 3)
		{
			// If there are only three nodes, there's only one possible triangulation (bar winding order, which is dealt with elsewhere).
			triangles.push_back(MeshTriangleT(nodeLoop.index(0), nodeLoop.index(1), nodeLoop.index(2), nodeLoop.labels()));
		}
		else
		{
			Split loopHalves = split_node_loop(nodeLoop);

			std::list<MeshTriangleT> result = triangulate(loopHalves.first);
			triangles.splice(triangles.end(), result);

			result = triangulate(loopHalves.second);
			triangles.splice(triangles.end(), result);
		}

		return triangles;
	}

	//#################### PRIVATE METHODS ####################
private:
	/**
	@brief	Constructs a split of the specified node loop into two halves.

	@param[in]	nodeLoop	The node loop
	@param[in]	e0			The first endpoint of the split line
	@param[in]	e1			The second endpoint of the split line
	@return	The constructed split (namely a pair of node loops representing the two halves)
	*/
	static Split construct_split(const NodeLoopT& nodeLoop, int e0, int e1)
	{
		int nodeCount = nodeLoop.size();

		std::vector<int> half1;
		for(int i=e0; i!=e1; i=(i+1)%nodeCount) half1.push_back(nodeLoop.index(i));
		half1.push_back(nodeLoop.index(e1));

		std::vector<int> half2;
		for(int i=e1; i!=e0; i=(i+1)%nodeCount) half2.push_back(nodeLoop.index(i));
		half2.push_back(nodeLoop.index(e0));

		return Split(NodeLoopT(half1, nodeLoop.labels()), NodeLoopT(half2, nodeLoop.labels()));
	}

	std::pair<bool,double> evaluate_split(const NodeLoopT& nodeLoop, int e0, int e1, const Split& split, const Vector3d& avgPlaneNormal) const
	{
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		// Step 1:	Construct the split plane.
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		// We need to obtain two vectors lying in the plane: one is the split line, the other is the average plane normal.
		const Vector3d& u = m_nodes[nodeLoop.index(e0)].position();
		const Vector3d& v = m_nodes[nodeLoop.index(e1)].position();
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

		PlaneClassification::Enum cp0 = MeshUtil::classify_node_loop_against_plane(split.first, splitPlane, m_nodes);
		if(cp0 == PlaneClassification::COPLANAR || cp0 == PlaneClassification::STRADDLE) return std::make_pair(false, 0);
		PlaneClassification::Enum cp1 = MeshUtil::classify_node_loop_against_plane(split.second, splitPlane, m_nodes);
		if(cp1 == PlaneClassification::COPLANAR || cp1 == PlaneClassification::STRADDLE || cp1 == cp0) return std::make_pair(false, 0);

		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		// Step 3:	Calculate the metric (see the referenced paper).
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		double minDistance = INT_MAX;
		for(int i=0, size=nodeLoop.size(); i<size; ++i)
		{
			double distance = splitPlane.distance_to_point(m_nodes[nodeLoop.index(i)].position());
			if(distance > MathConstants::SMALL_EPSILON && distance < minDistance) minDistance = distance;
		}

		return std::make_pair(true, minDistance / splitLine.length());
	}

	Split split_node_loop(const NodeLoopT& nodeLoop) const
	{
		Vector3d avgPlaneNormal = MeshUtil::calculate_average_plane(nodeLoop, m_nodes).normal();

		boost::optional<Split> bestSplit;
		double bestMetric = 0;	// metric values are guaranteed to be +ve and we take the split with the largest metric value

		int nodeCount = nodeLoop.size();

		for(int i=0; i<nodeCount-2; ++i)
		{
			for(int j=i+2; j<nodeCount; ++j)
			{
				if(i == 0 && j == nodeCount-1) continue;	// avoid trying to split along the edge between the first and last vertices

				Split split = construct_split(nodeLoop, i, j);
				std::pair<bool,double> result = evaluate_split(nodeLoop, i, j, split, avgPlaneNormal);
				if(result.first && result.second > bestMetric)
				{
					bestSplit = split;
					bestMetric = result.second;
				}
			}
		}

		if(bestSplit) return *bestSplit;
		else throw Exception("Unable to find an appropriate split line");
	}
};

}

#endif

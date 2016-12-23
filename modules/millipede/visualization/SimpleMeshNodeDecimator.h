/***
 * millipede: SimpleMeshNodeDecimator.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_SIMPLEMESHNODEDECIMATOR
#define H_MILLIPEDE_SIMPLEMESHNODEDECIMATOR

#include <boost/optional.hpp>

#include <common/adts/PriorityQueue.h>
#include "Mesh.h"
#include "MeshNodeDecimator.h"
#include "MeshUtil.h"
#include "NodeLoop.h"
#include "SchroederTriangulator.h"

namespace mp {

template <typename Label>
class SimpleMeshNodeDecimator : public MeshNodeDecimator<Label>
{
	//#################### TYPEDEFS ####################
private:
	typedef Mesh<Label> MeshT;
	typedef boost::shared_ptr<MeshT> Mesh_Ptr;
	typedef MeshNode<Label> MeshNodeT;
	typedef std::vector<MeshNodeT> MeshNodeVector;
	typedef MeshTriangle<Label> MeshTriangleT;
	typedef std::list<MeshTriangleT> MeshTriangleList;
	typedef std::set<MeshTriangleT> MeshTriangleSet;
	typedef NodeLoop<Label> NodeLoopT;

	//#################### NESTED CLASSES ####################
private:
	struct AdjacentPair
	{
		std::set<int> adjacentNodes;
	};

	struct Details
	{
		double metric;			// the decimation metric (calculated as the distance between the node and the average plane)
		NodeLoopT nodeLoop;		// the loop of nodes surrounding the node in the mesh

		Details(const NodeLoopT& nodeLoop_, double metric_)
		:	metric(metric_), nodeLoop(nodeLoop_)
		{}
	};

	//#################### PRIVATE VARIABLES ####################
private:
	const MeshTriangleSet& m_adjacentTriangles;
	boost::optional<Details> m_details;
	int m_i;
	Mesh_Ptr m_mesh;

	//#################### CONSTRUCTORS ####################
public:
	SimpleMeshNodeDecimator(int i, const Mesh_Ptr& mesh, const MeshTriangleSet& adjacentTriangles)
	:	m_adjacentTriangles(adjacentTriangles), m_i(i), m_mesh(mesh)
	{
		calculate_details();
	}

	//#################### PUBLIC METHODS ####################
public:
	void calculate_details()
	{
		boost::optional<NodeLoopT> nodeLoop = calculate_node_loop();

		if(!nodeLoop)
		{
			m_details = boost::none;
			return;
		}

		const MeshNodeVector& nodes = m_mesh->nodes();
		Plane averagePlane = MeshUtil::calculate_average_plane(*nodeLoop, nodes);
		double metric = averagePlane.distance_to_point(nodes[m_i].position());
		m_details = Details(*nodeLoop, metric);
	}

	MeshTriangleList decimate() const
	{
		SchroederTriangulator<Label> triangulator(m_mesh->nodes());
		try
		{
			return triangulator.triangulate(m_details->nodeLoop);
		}
		catch(Exception&)
		{
			// The decimation of this node could not be completed.
			return MeshTriangleList();
		}
	}

	int index() const
	{
		return m_i;
	}

	double metric() const
	{
		return m_details->metric;
	}

	bool valid() const
	{
		return m_details;	// implicitly converted to bool
	}

	//#################### PRIVATE METHODS ####################
private:
	boost::optional<NodeLoopT> calculate_node_loop()
	{
		const MeshNodeVector& nodes = m_mesh->nodes();

		// Build a table mapping nodes in the loop to the two nodes to which they're joined.
		std::map<int,AdjacentPair> table;

		for(std::set<int>::const_iterator it=nodes[m_i].adjacent_nodes().begin(), iend=nodes[m_i].adjacent_nodes().end(); it!=iend; ++it)
		{
			typename std::map<int,AdjacentPair>::iterator jt = table.insert(std::make_pair(*it, AdjacentPair())).first;
			std::set_intersection(nodes[*it].adjacent_nodes().begin(), nodes[*it].adjacent_nodes().end(),
								  nodes[m_i].adjacent_nodes().begin(), nodes[m_i].adjacent_nodes().end(),
								  std::inserter(jt->second.adjacentNodes, jt->second.adjacentNodes.begin()));

			if(jt->second.adjacentNodes.size() != 2)
			{
				// If a node which is supposedly part of the loop around this node doesn't have exactly two
				// neighbours which are part of the loop, then this node isn't suitable for decimation. This
				// is theoretically possible at the boundaries of the volume, and must be checked for.
				return boost::none;
			}
		}

		// Walk through the table, building up the loop as we go.
		std::vector<int> nodeIndices;
		typename std::map<int,AdjacentPair>::iterator it = table.begin();
		while(!it->second.adjacentNodes.empty())
		{
			const int& cur = it->first;
			int next = *(it->second.adjacentNodes.begin());
			it->second.adjacentNodes.erase(it->second.adjacentNodes.begin());
			nodeIndices.push_back(cur);

			it = table.find(next);
			if(!it->second.adjacentNodes.empty())
			{
				// Remove the link back to the node we've just left, if we're not at the end of the loop.
				it->second.adjacentNodes.erase(cur);
			}
		}

		// Ensure that the winding of the node loop is the same as those of the triangles surrounding this node.
		// It is safe to assume that the triangles themselves all have the same winding (by earlier construction).

		// Find the two other node indices of the first adjacent triangle (in the triangle's winding order).
		assert(!m_adjacentTriangles.empty());
		const MeshTriangleT& tri = *m_adjacentTriangles.begin();
		int triIndices[2];
		if(tri.index(0) == m_i)
		{
			triIndices[0] = tri.index(1);
			triIndices[1] = tri.index(2);
		}
		else if(tri.index(1) == m_i)
		{
			triIndices[0] = tri.index(2);
			triIndices[1] = tri.index(0);
		}
		else	// tri.index(2) == m_i
		{
			triIndices[0] = tri.index(0);
			triIndices[1] = tri.index(1);
		}

		// Walk round the node loop and check that they appear in the same order there - if not, reverse the node loop winding.
		for(int i=0, size=static_cast<int>(nodeIndices.size()); i<size; ++i)
		{
			if(nodeIndices[i] == triIndices[0])
			{
				int j = (i+1)%size;
				if(nodeIndices[j] != triIndices[1])
				{
					std::reverse(nodeIndices.begin(), nodeIndices.end());
				}
				break;
			}
			else if(nodeIndices[i] == triIndices[1])
			{
				int j = (i+size-1)%size;
				if(nodeIndices[j] != triIndices[0])
				{
					std::reverse(nodeIndices.begin(), nodeIndices.end());
				}
				break;
			}
		}

		return NodeLoopT(nodeIndices, nodes[m_i].labels());
	}
};

}

#endif

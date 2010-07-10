/***
 * millipede: CubeTriangleGenerator.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_CUBETRIANGLEGENERATOR
#define H_MILLIPEDE_CUBETRIANGLEGENERATOR

#include <algorithm>
#include <cassert>
#include <list>
#include <utility>

#include <common/adts/Edge.h>
#include <common/jobs/SimpleJob.h>
#include "FanTriangulator.h"
#include "NodeLoop.h"
#include "SchroederTriangulator.h"

namespace mp {

template <typename Label>
class CubeTriangleGenerator : public SimpleJob
{
	//#################### CONSTANTS ####################
private:
	enum TriangulateFlag
	{
		TRIANGULATE_FAN,		///< a node loop with this flag should be triangulated using the fan method
		TRIANGULATE_SCHROEDER,	///< a node loop with this flag should be triangulated using the Schroeder method
	};

	//#################### TYPEDEFS ####################
private:
	typedef GlobalNodeTable<Label> GlobalNodeTableT;
	typedef MeshBuildingData<Label> MeshBuildingDataT;
	typedef boost::shared_ptr<MeshBuildingDataT> MeshBuildingData_Ptr;
	typedef MeshNode<Label> MeshNodeT;
	typedef MeshTriangle<Label> MeshTriangleT;
	typedef std::list<MeshTriangleT> MeshTriangleList;
	typedef NodeLoop<Label> NodeLoopT;
	typedef std::pair<NodeLoopT,TriangulateFlag> TypedNodeLoop;
	typedef std::list<TypedNodeLoop> TypedNodeLoopList;

	//#################### PRIVATE VARIABLES ####################
private:
	MeshBuildingData_Ptr m_data;
	MeshTriangleList& m_triangles;
	int m_x, m_y, m_z;

	//#################### CONSTRUCTORS ####################
public:
	CubeTriangleGenerator(const MeshBuildingData_Ptr& data, int x, int y, int z)
	:	m_data(data), m_triangles(*data->triangles()), m_x(x), m_y(y), m_z(z)
	{}

	//#################### PUBLIC METHODS ####################
public:
	int length() const
	{
		return 1;
	}

	//#################### PRIVATE METHODS ####################
private:
	void ensure_consistent_triangle_orientation(MeshTriangleList& triangles)
	{
		const GlobalNodeTableT& globalNodeTable = m_data->global_node_table();

		for(typename MeshTriangleList::iterator it=triangles.begin(), iend=triangles.end(); it!=iend; ++it)
		{
			// Find the smaller of the two labels for this triangle.
			const MeshTriangleT& tri = *it;
			assert(tri.labels().size() == 2);
			Label smallerLabel = *tri.labels().begin();

			// Find a source corresponding to this label from the first triangle node (there is guaranteed to be one).
			const MeshNodeT& node = globalNodeTable(tri.index(0));
			Vector3d smallerLabelSource = Vector3d(node.find_source_of_label(smallerLabel));

			// Classify the source against the triangle's plane, and flip the triangle's winding if it's not pointing away from the source.
			Plane plane(MeshUtil::calculate_normal(tri, globalNodeTable), node.position());
			if(plane.classify_point(smallerLabelSource) == PlaneClassification::FRONT)
			{
				it->flip_winding();
			}
		}
	}

	void execute_impl()
	{
		// Find the typed node loops.
		TypedNodeLoopList typedNodeLoops = find_typed_node_loops();

		// Triangulate them according to their type.
		MeshTriangleList triangles = triangulate_typed_node_loops(typedNodeLoops);

		// Make sure each triangle is pointing consistently away from the lower of the two labels it separates.
		// (Note that the "away from" is arbitrary: the important thing is the consistency.)
		ensure_consistent_triangle_orientation(triangles);

		// Ensure that the adjacent node sets for each global node reflect the new edges which have been added during triangulation.
		fill_in_adjacent_nodes(triangles);

		// Splice the triangles onto the global triangle list.
		m_triangles.splice(m_triangles.end(), triangles);
	}

	/**
	@brief	Ensure that the adjacent node sets for each node reflect the new edges which have been added during triangulation.

	@param[in]	triangles	The triangles that have been generated
	*/
	void fill_in_adjacent_nodes(const MeshTriangleList& triangles)
	{
		GlobalNodeTableT& globalNodeTable = m_data->global_node_table();

		for(typename MeshTriangleList::const_iterator it=triangles.begin(), iend=triangles.end(); it!=iend; ++it)
		{
			const MeshTriangleT& tri = *it;
			int i0 = tri.index(0), i1 = tri.index(1), i2 = tri.index(2);
			MeshNodeT& n0 = globalNodeTable(i0);
			MeshNodeT& n1 = globalNodeTable(i1);
			MeshNodeT& n2 = globalNodeTable(i2);
			n0.add_adjacent_node(i1);		n0.add_adjacent_node(i2);
			n1.add_adjacent_node(i0);		n1.add_adjacent_node(i2);
			n2.add_adjacent_node(i0);		n2.add_adjacent_node(i1);
		}
	}

	boost::optional<TypedNodeLoop> find_typed_node_loop(std::map<int,MeshNodeT>& localNodeMap) const
	{
		// Step 1:	Find a start node with exactly two labels and a remaining edge. If no such node exists, we've found all the loops.
		int startIndex = -1;
		for(typename std::map<int,MeshNodeT>::const_iterator it=localNodeMap.begin(), iend=localNodeMap.end(); it!=iend; ++it)
		{
			const MeshNodeT& n = it->second;
			if(n.label_count() == 2 && n.adjacent_node_count() != 0)
			{
				startIndex = it->first;
				break;
			}
		}

		if(startIndex == -1) return boost::none;

		// Step 2:	Follow the trail laid by the labels - at each step, follow an unused edge to an adjacent node with at
		//			least the two labels of the start node. If no such adjacent node exists, back up and try another route.
		//			If one of the nodes is the cube centre node, make a note and carry on - we'll need to triangulate this
		//			loop using a fan approach. Terminate when we reach the start node again. The way M3C works guarantees
		//			that there is a loop back to each valid start node, so termination is guaranteed.

		// Create a 'used' map of the (undirected) edges to store which edges we've already seen.
		std::map<Edge,bool,UndirectedEdgePredicate> used;
		for(typename std::map<int,MeshNodeT>::const_iterator it=localNodeMap.begin(), iend=localNodeMap.end(); it!=iend; ++it)
		{
			const int u = it->first;
			const MeshNodeT& n = it->second;
			for(std::set<int>::const_iterator jt=n.adjacent_nodes().begin(), jend=n.adjacent_nodes().end(); jt!=jend; ++jt)
			{
				const int v = *jt;
				used.insert(std::make_pair(Edge(u,v), false));
			}
		}

		// Make a note of the two labels every node in the loop must have.
		const MeshNodeT& startNode = localNodeMap.find(startIndex)->second;
		std::set<Label> startLabels = startNode.labels();

		// Find the node loop using the trail-following algorithm described above.
		std::vector<int> nodeIndices;
		int curIndex = startIndex;
		TriangulateFlag flag = TRIANGULATE_SCHROEDER;
		int cubeCentreIndex = m_data->cube_table().lookup_cube_centre_node(m_x, m_y, m_z);

		do
		{
			nodeIndices.push_back(curIndex);
			if(curIndex == cubeCentreIndex) flag = TRIANGULATE_FAN;

			const MeshNodeT& curNode = localNodeMap.find(curIndex)->second;
			int adjIndex = -1;
			for(std::set<int>::const_iterator it=curNode.adjacent_nodes().begin(), iend=curNode.adjacent_nodes().end(); it!=iend; ++it)
			{
				const MeshNodeT& adjNode = localNodeMap.find(*it)->second;

				// If the edge has not yet been used, and the adjacent node has at least the two labels of the start node, traverse the edge.
				if(!used[Edge(curIndex, *it)] && adjNode.has_labels(startLabels))
				{
					adjIndex = *it;
					break;
				}
			}

			if(adjIndex != -1)
			{
				used[Edge(curIndex, adjIndex)] = true;
				curIndex = adjIndex;
			}
			else
			{
				// If we couldn't find an adjacent node with the right labels, backtrack and try another route.
				// Note that there's no danger of setting the current index back to the start index here: the first
				// step will always be a valid one.
				nodeIndices.pop_back();
				if(!nodeIndices.empty())
				{
					curIndex = nodeIndices.back();
					nodeIndices.pop_back();
				}
				else
				{
					throw Exception("Something went wrong: couldn't find an adjacent node with the right labels.");
				}
			}
		} while(curIndex != startIndex);

		// Step 3:	Remove edges from further consideration in future loops if at least one of their endpoints has only two labels.
		for(int i=0, nodeCount = static_cast<int>(nodeIndices.size()); i<nodeCount; ++i)
		{
			int j = (i+1)%nodeCount;
			int curIndex = nodeIndices[i];
			int adjIndex = nodeIndices[j];
			MeshNodeT& curNode = localNodeMap.find(curIndex)->second;
			MeshNodeT& adjNode = localNodeMap.find(adjIndex)->second;

			// Remove the edge iff one of its endpoints has only two labels.
			if(curNode.label_count() == 2 || adjNode.label_count() == 2)
			{
				curNode.remove_adjacent_node(adjIndex);
				adjNode.remove_adjacent_node(curIndex);
			}
		}

		return std::make_pair(NodeLoopT(nodeIndices, startLabels), flag);
	}

	TypedNodeLoopList find_typed_node_loops() const
	{
		TypedNodeLoopList typedNodeLoops;

		std::set<int> nodeSet = m_data->cube_table().lookup_cube_nodes(m_x, m_y, m_z);

		// Make a local node map with only the local nodes in it. This is necessary for two reasons:
		// (a) Nodes in the global node table refer to adjacent nodes not in this cube
		// (b) We want to be able to remove edges from further consideration without damaging the global node table
		const GlobalNodeTableT& globalNodeTable = m_data->global_node_table();
		std::map<int,MeshNodeT> localNodeMap;
		for(std::set<int>::const_iterator it=nodeSet.begin(), iend=nodeSet.end(); it!=iend; ++it)
		{
			typename std::map<int,MeshNodeT>::iterator loc = localNodeMap.insert(std::make_pair(*it, globalNodeTable(*it))).first;
			MeshNodeT& n = loc->second;
			std::set<int> relevantNodes;
			std::set_intersection(n.adjacent_nodes().begin(), n.adjacent_nodes().end(), nodeSet.begin(), nodeSet.end(), std::inserter(relevantNodes, relevantNodes.begin()));
			n.set_adjacent_nodes(relevantNodes);
		}

		// Iteratively try to find a typed node loop from the local node map until we've got them all.
		boost::optional<TypedNodeLoop> typedNodeLoop;
		while((typedNodeLoop = find_typed_node_loop(localNodeMap)))
		{
			typedNodeLoops.push_back(*typedNodeLoop);
		}

		return typedNodeLoops;
	}

	MeshTriangleList triangulate_typed_node_loops(const TypedNodeLoopList& typedNodeLoops)
	{
		MeshTriangleList triangles;

		FanTriangulator<Label> fanTriangulator(m_data->cube_table().lookup_cube_centre_node(m_x, m_y, m_z));
		SchroederTriangulator<Label> schroederTriangulator(m_data->global_node_table());

		for(typename TypedNodeLoopList::const_iterator it=typedNodeLoops.begin(), iend=typedNodeLoops.end(); it!=iend; ++it)
		{
			const NodeLoopT& nodeLoop = it->first;
			TriangulateFlag flag = it->second;
			if(flag == TRIANGULATE_FAN)
			{
				MeshTriangleList result = fanTriangulator.triangulate(nodeLoop);
				triangles.splice(triangles.end(), result);
			}
			else	// flag == TRIANGULATE_SCHROEDER
			{
				MeshTriangleList result = schroederTriangulator.triangulate(nodeLoop);
				triangles.splice(triangles.end(), result);
			}
		}

		return triangles;
	}
};

}

#endif

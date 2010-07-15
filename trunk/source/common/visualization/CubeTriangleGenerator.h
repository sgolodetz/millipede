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
#include <common/io/util/OSSWrapper.h>
#include <common/jobs/SimpleJob.h>
#include "FanTriangulator.h"
#include "NodeLoop.h"
#include "SchroederTriangulator.h"

namespace mp {

/**
@brief	A CubeTriangleGenerator finds node loops in a given cube and triangulates them,
		ensuring that the resulting triangles are oriented consistently as it does so.

@tparam	Label	The type of label stored at the cube vertices and in the mesh nodes
*/
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
	/**
	@brief	Constructs a CubeTriangleGenerator job to work on the specified cube.

	@param[in]	data			The mesh building data shared by all sub-jobs of MeshBuilder
	@param[in]	x				The x position of the cube in the volume
	@param[in]	y				The y position of the cube in the volume
	@param[in]	z				The z position of the cube in the volume
	*/
	CubeTriangleGenerator(const MeshBuildingData_Ptr& data, int x, int y, int z)
	:	m_data(data), m_triangles(*data->triangles()), m_x(x), m_y(y), m_z(z)
	{}

	//#################### PUBLIC METHODS ####################
public:
	/**
	@brief	Returns the length of the job.

	@return	As described
	*/
	int length() const
	{
		return 1;
	}

	//#################### PRIVATE METHODS ####################
private:
	/**
	@brief	Make sure each triangle is pointing consistently away from the lower of the two labels it separates.

	Note that the "away from" is somewhat arbitrary: the important thing is the consistency, so that the triangles
	can be sensibly used later.

	@param[in,out]	triangles	The triangles that have been generated
	*/
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
			Plane plane(MeshUtil::calculate_normal(tri, *globalNodeTable.master_array()), node.position());
			if(plane.classify_point(smallerLabelSource) == PlaneClassification::FRONT)
			{
				it->flip_winding();
			}
		}
	}

	/**
	@brief	Executes the job.
	*/
	void execute_impl()
	{
		set_status(OSSWrapper() << "Generating triangles for cube (" << m_x << ',' << m_y << ',' << m_z << ')');

		TypedNodeLoopList typedNodeLoops = find_typed_node_loops();
		MeshTriangleList triangles = triangulate_typed_node_loops(typedNodeLoops);
		ensure_consistent_triangle_orientation(triangles);

		// Splice the triangles onto the global triangle list.
		m_triangles.splice(m_triangles.end(), triangles);
	}

	/**
	@brief	Finds a typed node loop from the remaining nodes and (implicit) edges in the local node map, if possible.

	Note that some (implicit) edges in the local node map will be removed as each node loop is found. This is essential,
	as find_typed_node_loops() - i.e. the caller of this method - would not otherwise terminate.

	@param[in,out]	localNodeMap	A map containing the nodes within this cube
	@return	The typed node loop found, if any, or boost::none otherwise
	*/
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

	/**
	@brief	Finds all the typed node loops in the cube.

	@return	The typed node loops as a std::list
	*/
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

	/**
	@brief	Triangulates the typed node loops according to type.

	Node loops that go through the cube centre node (if any) will be triangulated using the
	fan approach. All other node loops will be triangulated using the Schroeder method.

	@param[in]	typedNodeLoops	The typed node loops
	@return	A std::list of the mesh triangles resulting from triangulating all the node loops
	*/
	MeshTriangleList triangulate_typed_node_loops(const TypedNodeLoopList& typedNodeLoops)
	{
		MeshTriangleList triangles;

		FanTriangulator<Label> fanTriangulator(m_data->cube_table().lookup_cube_centre_node(m_x, m_y, m_z));
		SchroederTriangulator<Label> schroederTriangulator(*m_data->global_node_table().master_array());

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

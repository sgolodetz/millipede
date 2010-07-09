/***
 * millipede: CubeTriangleGenerator.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_CUBETRIANGLEGENERATOR
#define H_MILLIPEDE_CUBETRIANGLEGENERATOR

#include <algorithm>
#include <list>
#include <utility>

#include <common/jobs/SimpleJob.h>

namespace mp {

template <typename Label>
class CubeTriangleGenerator : public SimpleJob
{
	//#################### CONSTANTS ####################
private:
	enum TriangulateFlag
	{
		FAN_TRIANGULATE,		///< a node loop with this flag should be triangulated using the fan method
		SCHROEDER_TRIANGULATE,	///< a node loop with this flag should be triangulated using the Schroeder method
	};

	//#################### TYPEDEFS ####################
private:
	typedef GlobalNodeTable<Label> GlobalNodeTableT;
	typedef MeshBuildingData<Label> MeshBuildingDataT;
	typedef boost::shared_ptr<MeshBuildingDataT> MeshBuildingData_Ptr;
	typedef MeshNode<Label> MeshNodeT;
	typedef std::vector<int> NodeLoop;
	typedef std::pair<NodeLoop,TriangulateFlag> TypedNodeLoop;
	typedef std::list<TypedNodeLoop> TypedNodeLoopList;

	//#################### PRIVATE VARIABLES ####################
private:
	MeshBuildingData_Ptr m_data;
	int m_x, m_y, m_z;

	//#################### CONSTRUCTORS ####################
public:
	CubeTriangleGenerator(const MeshBuildingData_Ptr& data, int x, int y, int z)
	:	m_data(data), m_x(x), m_y(y), m_z(z)
	{}

	//#################### PUBLIC METHODS ####################
public:
	int length() const
	{
		return 1;
	}

	//#################### PRIVATE METHODS ####################
private:
	void execute_impl()
	{
		// Find the typed node loops.
		TypedNodeLoopList typedNodeLoops = find_typed_node_loops();

		// Triangulate them according to their type.
		// TODO

		// Make sure each triangle is pointing consistently away from the lower of the two labels it separates.
		// (Note that the "away from" is arbitrary: the important thing is the consistency.)
		// TODO

		// Ensure that the adjacent node sets for each global node reflect the new edges which have been added during triangulation.
		// TODO
	}

	boost::optional<TypedNodeLoop> find_typed_node_loop(std::map<int,MeshNodeT>& localNodeMap) const
	{
		// NYI
		return boost::none;
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
			std::map<int,MeshNodeT>::iterator loc = localNodeMap.insert(std::make_pair(*it, globalNodeTable(*it))).first;
			MeshNodeT& n = loc->second;
			std::set<int> relevantNodes;
			std::set_intersection(n.adjacentNodes.begin(), n.adjacentNodes.end(), nodeSet.begin(), nodeSet.end(), std::inserter(relevantNodes, relevantNodes.begin()));
			n.adjacentNodes = relevantNodes;
		}

		// Iteratively try to find a typed node loop from the local node map until we've got them all.
		boost::optional<TypedNodeLoop> typedNodeLoop;
		while((typedNodeLoop = find_typed_node_loop(localNodeMap)) != boost::none)
		{
			typedNodeLoops.push_back(*typedNodeLoop);
		}

		return typedNodeLoops;
	}
};

}

#endif

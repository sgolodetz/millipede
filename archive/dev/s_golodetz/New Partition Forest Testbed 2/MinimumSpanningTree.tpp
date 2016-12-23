/***
 * millipede: MinimumSpanningTree.tpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include <algorithm>
#include <utility>
#include <vector>

#include "DisjointSetForest.h"

namespace mp {

//#################### CONSTRUCTORS ####################
template <typename Node, typename EdgeValue>
MinimumSpanningTree<Node,EdgeValue>::MinimumSpanningTree(const IAdjacencyGraphT& graph)
:	m_mst(new AdjacencyGraphT)
{
	// Build a minimum spanning tree using Kruskal's algorithm.

	// Step 1:	Extract and sort the edges.
	std::vector<Edge> edges;

	IAdjacencyGraphT::EdgeIterator_Ptr eit = graph.edges();
	while(eit->has_next()) edges.push_back(eit->next());

	std::sort(edges.begin(), edges.end(), EdgeValueAsc());

	// Step 2:	Extract the nodes and add them to the MST and a disjoint set forest.
	DisjointSetForest<> forest;
	IAdjacencyGraphT::NodeCIterator_Ptr nit = graph.nodes();
	while(nit->has_next())
	{
		std::pair<int,Node_CPtr> p = nit->next();
		m_mst->add_node(p.first, Node_Ptr(new Node(*p.second)));
		forest.add_node(p.first);
	}

	// Step 3:	Keep adding edges which don't form a cycle until all the nodes are connected.
	for(std::vector<Edge>::const_iterator it=edges.begin(), iend=edges.end(); it!=iend; ++it)
	{
		if(forest.tree_count() == 1) break;

		int u = it->u();
		int v = it->v();
		if(forest.find_set(u) != forest.find_set(v))
		{
			m_mst->add_edge(u, v, it->value());
			forest.union_nodes(u, v);
		}
	}
}

}

/***
 * millipede: RootedMST.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_ROOTEDMST
#define H_MILLIPEDE_ROOTEDMST

#include <set>

#include <boost/shared_ptr.hpp>

#include "AdjacencyGraph.h"
#include "PriorityQueue.h"

namespace mp {

template <typename NodeProperties, typename EdgeWeight>
class RootedMST
{
	//#################### NESTED CLASSES ####################
private:
	struct NullType {};

	struct NodeData
	{
		int m_parent;
		NodeProperties m_properties;

		NodeData()
		:	m_parent(-1)
		{}
	};

	//#################### TYPEDEFS ####################
private:
	typedef AdjacencyGraph<NodeData, EdgeWeight> BaseGraph;
public:
	typedef typename BaseGraph::Edge Edge;
	typedef typename BaseGraph::EdgeCIter EdgeConstIterator;

	//#################### PRIVATE VARIABLES ####################
private:
	int m_root;
	BaseGraph m_base;

	//#################### CONSTRUCTORS ####################
public:
	template <typename Graph>
	explicit RootedMST(const Graph& graph)
	{
		// Build an MST for the graph using Prim's algorithm.
		typedef PriorityQueue<int, EdgeWeight, NullType> PQ;
		PQ pq;
		std::vector<int> nodeIndices = graph.node_indices();
		if(nodeIndices.empty()) throw Exception("Cannot build a *rooted* MST for a graph with no nodes (there's no potential root)");
		m_root = nodeIndices.front();
		pq.insert(m_root, 0, NullType());
		m_base.set_node_properties(m_root, NodeData());
		for(size_t i=1, size=nodeIndices.size(); i<size; ++i)
		{
			pq.insert(nodeIndices[i], INT_MAX, NullType());
			m_base.set_node_properties(nodeIndices[i], NodeData());
		}

		while(!pq.empty())
		{
			int u = pq.top().id();
			pq.pop();

			std::vector<int> adjNodes = graph.adjacent_nodes(u);
			for(size_t i=0, size=adjNodes.size(); i<size; ++i)
			{
				int v = adjNodes[i];
				EdgeWeight weight = graph.edge_weight(u, v);
				if(pq.contains(v) && weight < pq.element(v).key())
				{
					pq.update_key(v, weight);
					m_base.node_properties(v).m_parent = u;
				}
			}
		}

		// Add the adjacent edges.
		for(typename BaseGraph::NodePropertiesCIter it=m_base.node_properties_cbegin(), iend=m_base.node_properties_cend(); it!=iend; ++it)
		{
			int u = it->first;
			int v = it->second.m_parent;
			if(v != -1)
			{
				EdgeWeight weight = graph.edge_weight(u, v);
				m_base.set_edge_weight(u, v, weight);
			}
		}
	}

	//#################### PUBLIC METHODS ####################
public:
	std::vector<Edge> adjacent_edges(int n) const		{ return m_base.adjacent_edges(n); }
	std::vector<int> adjacent_nodes(int n) const		{ return m_base.adjacent_nodes(n); }
	EdgeWeight edge_weight(int u, int v) const			{ return m_base.edge_weight(u, v); }
	EdgeConstIterator edges_cbegin() const				{ return m_base.edges_cbegin(); }
	EdgeConstIterator edges_cend() const				{ return m_base.edges_cend(); }
	bool has_edge(int u, int v) const					{ return m_base.has_edge(u, v); }
	bool has_node(int n) const							{ return m_base.has_node(n); }
	std::vector<int> node_indices() const				{ return m_base.node_indices(); }
	const NodeProperties& node_properties(int n) const	{ return m_base.node_properties(n).m_properties; }

	std::set<int> tree_children(int n) const
	{
		std::vector<int> adjNodes = adjacent_nodes(n);
		std::set<int> children(adjNodes.begin(), adjNodes.end());
		children.erase(tree_parent(n));
		return children;
	}

	int tree_parent(int n) const						{ return m_base.node_properties(n).m_parent; }
	int tree_root() const								{ return m_root; }
};

}

#endif

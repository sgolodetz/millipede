/***
 * millipede: RootedMST.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_ROOTEDMST
#define H_MILLIPEDE_ROOTEDMST

#include <cassert>
#include <climits>
#include <set>

#include <boost/any.hpp>
#include <boost/shared_ptr.hpp>

#include "../util/NullType.h"
#include "AdjacencyGraph.h"
#include "PriorityQueue.h"

namespace mp {

template <typename EdgeWeight>
class RootedMST
{
	//#################### NESTED CLASSES ####################
private:
	struct NodeProperties
	{
		int m_parent;
		boost::any m_data;

		NodeProperties()
		:	m_parent(-1)
		{}

		explicit NodeProperties(int parent)
		:	m_parent(parent)
		{}
	};

	//#################### TYPEDEFS ####################
private:
	typedef AdjacencyGraph<NodeProperties, EdgeWeight> BaseGraph;	// each node stores the index of its parent and (possibly) some auxiliary data
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
		set_tree_parent(m_root, -1);
		for(size_t i=1, size=nodeIndices.size(); i<size; ++i)
		{
			pq.insert(nodeIndices[i], INT_MAX, NullType());
			set_tree_parent(nodeIndices[i], -1);
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
					set_tree_parent(v, u);
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

	int merge_nodes(int parent, int child)
	{
		assert(parent == tree_parent(child));

		// Determine the indices of the surviving node and the one to be removed.
		int survivingIndex = std::min(parent, child), otherIndex = std::max(parent, child);

		// Move all the edges descending from the node to be removed to the surviving node.
		std::set<int> otherChildren = tree_children(otherIndex);
		if(survivingIndex == child) otherChildren.erase(child);
		for(std::set<int>::const_iterator it=otherChildren.begin(), iend=otherChildren.end(); it!=iend; ++it)
		{
			set_tree_parent(*it, survivingIndex);
			m_base.set_edge_weight(*it, survivingIndex, edge_weight(*it, otherIndex));
		}

		// If the surviving node is the child, it needs to be connected to the parent of the initial parent.
		if(survivingIndex == child)
		{
			set_tree_parent(survivingIndex, tree_parent(parent));
			m_base.set_edge_weight(survivingIndex, tree_parent(parent), edge_weight(otherIndex, tree_parent(parent)));
		}

		// If the root gets removed during a merge with another node, update its index.
		if(m_root == otherIndex) m_root = survivingIndex;

		// Remove the node.
		m_base.remove_node(otherIndex);

		return survivingIndex;
	}

	int node_count() const
	{
		return m_base.node_count();
	}

	template <typename T>
	const T& node_data(int n) const
	{
		return boost::any_cast<const T&>(m_base.node_properties(n).m_data);
	}

	std::vector<int> node_indices() const
	{
		return m_base.node_indices();
	}

	template <typename T>
	void set_node_data(int n, const T& data)
	{
		m_base.node_properties(n).m_data = data;
	}

	std::set<int> tree_children(int n) const
	{
		std::vector<int> adjNodes = adjacent_nodes(n);
		std::set<int> children(adjNodes.begin(), adjNodes.end());
		children.erase(tree_parent(n));
		return children;
	}

	int tree_parent(int n) const
	{
		return m_base.node_properties(n).m_parent;
	}

	int tree_root() const
	{
		return m_root;
	}

	//#################### PRIVATE METHODS ####################
private:
	void set_tree_parent(int n, int parent)
	{
		// Note: set_tree_parent() intentionally creates node n if it doesn't already exist.
		if(m_base.has_node(n))	m_base.node_properties(n).m_parent = parent;
		else					m_base.set_node_properties(n, NodeProperties(parent));
	}
};

}

#endif

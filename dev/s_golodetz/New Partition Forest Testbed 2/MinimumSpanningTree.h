/***
 * millipede: MinimumSpanningTree.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MINIMUMSPANNINGTREE
#define H_MILLIPEDE_MINIMUMSPANNINGTREE

#include "AdjacencyGraph.h"

namespace mp {

template <typename Node, typename EdgeValue>
class MinimumSpanningTree : public IAdjacencyGraph<Node,EdgeValue>
{
	//#################### TYPEDEFS ####################
private:
	typedef IAdjacencyGraph<Node,EdgeValue> IAdjacencyGraphT;
	typedef shared_ptr<const IAdjacencyGraphT> IAdjacencyGraph_CPtr;
	typedef IAdjacencyGraphT::Edge Edge;
	typedef IAdjacencyGraphT::Node_CPtr Node_CPtr;

	typedef AdjacencyGraph<Node,EdgeValue> AdjacencyGraphT;
	typedef shared_ptr<AdjacencyGraphT> AdjacencyGraph_Ptr;
	typedef shared_ptr<const AdjacencyGraphT> AdjacencyGraph_CPtr;
	typedef typename AdjacencyGraphT::Node Node;
	typedef typename AdjacencyGraphT::Node_Ptr Node_Ptr;

	//#################### NESTED CLASSES ####################
private:
	struct EdgeValueAsc
	{
		bool operator()(const Edge& lhs, const Edge& rhs) const
		{
			return lhs.value() < rhs.value();
		}
	};

	//#################### PRIVATE VARIABLES ####################
private:
	AdjacencyGraph_Ptr m_mst;

	//#################### CONSTRUCTORS ####################
public:
	explicit MinimumSpanningTree(const IAdjacencyGraphT& graph);

	//#################### PUBLIC OPERATORS ####################
public:
	Node& operator()(int n)							{ return (*m_mst)(n); }
	const Node& operator()(int n) const				{ return (*m_mst)(n); }
	EdgeValue operator()(int u, int v) const		{ return (*m_mst)(u, v); }

	//#################### PUBLIC METHODS ####################
public:
	EdgeIterator_Ptr adjacent_edges(int n) const	{ return m_mst->adjacent_edges(n); }
	NodeCIterator_Ptr adjacent_nodes(int n) const	{ return AdjacencyGraph_CPtr(m_mst)->adjacent_nodes(n); }
	EdgeIterator_Ptr edges() const					{ return m_mst->edges(); }
	bool has_edge(int u, int v) const				{ return m_mst->has_edge(u, v); }
	bool has_node(int n) const						{ return m_mst->has_node(n); }
	int node_count() const							{ return m_mst->node_count(); }
	NodeCIterator_Ptr nodes() const					{ return AdjacencyGraph_CPtr(m_mst)->nodes(); }
};

}

#include "MinimumSpanningTree.tpp"

#endif

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
	typedef shared_ptr<const IAdjacencyGraph<Node,EdgeValue> > IAdjacencyGraph_CPtr;
	typedef shared_ptr<AdjacencyGraph<Node,EdgeValue> > AdjacencyGraph_Ptr;
	typedef shared_ptr<const AdjacencyGraph<Node,EdgeValue> > AdjacencyGraph_CPtr;

	//#################### PRIVATE VARIABLES ####################
private:
	AdjacencyGraph_Ptr m_mst;

	//#################### CONSTRUCTORS ####################
public:
	explicit MinimumSpanningTree(const IAdjacencyGraph_CPtr& graph);

	//#################### PUBLIC OPERATORS ####################
public:
	const Node& operator()(int n) const					{ return (*m_mst)(n); }
	EdgeValue operator()(int u, int v) const			{ return (*m_mst)(u, v); }

	//#################### PUBLIC METHODS ####################
public:
	EdgeCRefIterator_Ptr adjacent_edges(int n) const	{ return m_mst->adjacent_edges(n); }
	EdgeCRefIterator_Ptr edges() const					{ return m_mst->edges(); }
	bool has_edge(int u, int v) const					{ return m_mst->has_edge(u, v); }
	bool has_node(int n) const							{ return m_mst->has_node(n); }
	NodeCRefIterator_Ptr nodes() const					{ return AdjacencyGraph_CPtr(m_mst)->nodes(); }
};

}

#include "MinimumSpanningTree.tpp"

#endif

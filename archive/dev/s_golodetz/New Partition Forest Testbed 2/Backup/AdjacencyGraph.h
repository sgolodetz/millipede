/***
 * millipede: AdjacencyGraph.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_ADJACENCYGRAPH
#define H_MILLIPEDE_ADJACENCYGRAPH

#include <map>

#include <boost/multi_index_container.hpp>
#include <boost/multi_index/composite_key.hpp>
#include <boost/multi_index/identity.hpp>
#include <boost/multi_index/mem_fun.hpp>
#include <boost/multi_index/ordered_index.hpp>
#include <boost/multi_index/sequenced_index.hpp>
using namespace boost::multi_index;

#include "IAdjacencyGraph.h"

namespace mp {

template <typename Node, typename EdgeValue>
class AdjacencyGraph : public IAdjacencyGraph<Node,EdgeValue>
{
	//#################### TYPEDEFS ####################
private:
	typedef std::map<int,Node_Ptr> NodeMap;

	// Tags
	struct source;
	struct dest;

	typedef multi_index_container<
		Edge,
		indexed_by<
			ordered_unique<
				composite_key<
					Edge,
					const_mem_fun<Edge,int,&Edge::u>,
					const_mem_fun<Edge,int,&Edge::v>
				>
			>,
			ordered_non_unique<tag<source>,		const_mem_fun<Edge,int,&Edge::u> >,
			ordered_non_unique<tag<dest>,		const_mem_fun<Edge,int,&Edge::v> >
		>
	> EdgeTable;

	//#################### NESTED CLASSES ####################
private:
	class AdjEdgesCIter;
	class AllEdgesCIter;
	template <typename T, typename Map, typename Iter> class AllNodesIter;

	//#################### PRIVATE VARIABLES ####################
private:
	NodeMap m_nodes;
	EdgeTable m_edges;

	//#################### PUBLIC OPERATORS ####################
public:
	Node& operator()(int n);
	const Node& operator()(int n) const;
	//EdgeValue& operator()(int u, int v);
	EdgeValue operator()(int u, int v) const;

	//#################### PUBLIC METHODS ####################
public:
	void add_edge(int u, int v, const EdgeValue& value);
	void add_node(int n, const Node_Ptr& node);
	EdgeCRefIterator_Ptr adjacent_edges(int n) const;
	EdgeCRefIterator_Ptr edges() const;
	bool has_edge(int u, int v) const;
	bool has_node(int n) const;
	NodeRefIterator_Ptr nodes();
	NodeCRefIterator_Ptr nodes() const;
	void remove_edge(int u, int v);
	void remove_node(int n);

	//#################### PRIVATE METHODS ####################
private:
	void check_node(int n) const;
};

}

#include "AdjacencyGraph.tpp"

#endif

/***
 * millipede: AdjacencyGraph.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_ADJACENCYGRAPH
#define H_MILLIPEDE_ADJACENCYGRAPH

#include <algorithm>
#include <map>
#include <vector>

#include <boost/multi_index_container.hpp>
#include <boost/multi_index/composite_key.hpp>
#include <boost/multi_index/member.hpp>
#include <boost/multi_index/ordered_index.hpp>

#include <common/exceptions/Exception.h>
#include <common/io/util/OSSWrapper.h>
#include "WeightedEdge.h"

namespace mp {

namespace mp_AdjacencyGraph {

using namespace boost::multi_index;

template <typename NodeProperties, typename EdgeWeight>
class AdjacencyGraph
{
	//#################### TYPEDEFS ####################
public:
	typedef WeightedEdge<EdgeWeight> Edge;

private:
	// Tags
	struct tagDefault;
	struct tagSmaller;
	struct tagLarger;

	typedef multi_index_container<
		Edge,
		indexed_by<
			ordered_unique<tag<tagDefault>,
				composite_key<
					Edge,
					member<Edge,int,&Edge::u>,
					member<Edge,int,&Edge::v>
				>
			>,
			ordered_non_unique<tag<tagSmaller>,
				member<Edge,int,&Edge::u>
			>,
			ordered_non_unique<tag<tagLarger>,
				member<Edge,int,&Edge::v>
			>
		>
	> EdgeContainer;

	typedef typename EdgeContainer::template index<tagSmaller>::type EdgesBySmaller;
	typedef typename EdgeContainer::template index<tagLarger>::type EdgesByLarger;

	typedef typename EdgesBySmaller::const_iterator EdgesBySmallerCIter;
	typedef typename EdgesByLarger::const_iterator EdgesByLargerCIter;

public:
	typedef typename EdgeContainer::const_iterator EdgeCIter;
	typedef typename std::map<int,NodeProperties>::const_iterator NodePropertiesCIter;

	//#################### PRIVATE VARIABLES ####################
private:
	std::map<int,NodeProperties> m_nodeProperties;
	EdgeContainer m_edges;

	//#################### PUBLIC METHODS ####################
public:
	std::vector<Edge> adjacent_edges(int n) const
	{
		if(!has_node(n)) throw Exception(OSSWrapper() << "No such node: " << n);

		std::vector<Edge> ret;

		const EdgesByLarger& edgesByLarger = m_edges.get<tagLarger>();
		const EdgesBySmaller& edgesBySmaller = m_edges.get<tagSmaller>();
		std::pair<EdgesByLargerCIter,EdgesByLargerCIter> adjacentsByLarger = edgesByLarger.equal_range(n);
		std::pair<EdgesBySmallerCIter,EdgesBySmallerCIter> adjacentsBySmaller = edgesBySmaller.equal_range(n);
		std::copy(adjacentsByLarger.first, adjacentsByLarger.second, std::back_inserter(ret));
		std::copy(adjacentsBySmaller.first, adjacentsBySmaller.second, std::back_inserter(ret));

		return ret;
	}

	std::vector<int> adjacent_nodes(int n) const
	{
		if(!has_node(n)) throw Exception(OSSWrapper() << "No such node: " << n);

		std::vector<int> ret;

		const EdgesByLarger& edgesByLarger = m_edges.get<tagLarger>();
		const EdgesBySmaller& edgesBySmaller = m_edges.get<tagSmaller>();
		std::pair<EdgesByLargerCIter,EdgesByLargerCIter> adjacentsByLarger = edgesByLarger.equal_range(n);
		std::pair<EdgesBySmallerCIter,EdgesBySmallerCIter> adjacentsBySmaller = edgesBySmaller.equal_range(n);
		for(EdgesByLargerCIter it=adjacentsByLarger.first; it!=adjacentsByLarger.second; ++it) ret.push_back(it->u);
		for(EdgesBySmallerCIter it=adjacentsBySmaller.first; it!=adjacentsBySmaller.second; ++it) ret.push_back(it->v);

		return ret;
	}

	EdgeWeight edge_weight(int u, int v) const
	{
		typename EdgeContainer::const_iterator it = m_edges.find(make_edge_tuple(u, v));
		if(it != m_edges.end()) return it->weight;
		else throw Exception(OSSWrapper() << "No such edge: {" << u << ',' << v << '}');
	}

	std::vector<Edge> edges() const
	{
		return std::vector<Edge>(m_edges.begin(), m_edges.end());
	}

	EdgeCIter edges_cbegin() const
	{
		return m_edges.begin();
	}

	EdgeCIter edges_cend() const
	{
		return m_edges.end();
	}

	bool has_edge(int u, int v) const
	{
		return m_edges.find(make_edge_tuple(u, v)) != m_edges.end();
	}

	bool has_node(int n) const
	{
		return m_nodeProperties.find(n) != m_nodeProperties.end();
	}

	int node_count() const
	{
		return static_cast<int>(m_nodeProperties.size());
	}

	std::vector<int> node_indices() const
	{
		std::vector<int> ret;
		ret.reserve(m_nodeProperties.size());
		for(typename std::map<int,NodeProperties>::const_iterator it=m_nodeProperties.begin(), iend=m_nodeProperties.end(); it!=iend; ++it)
		{
			ret.push_back(it->first);
		}
		return ret;
	}

	NodeProperties& node_properties(int n)
	{
		typename std::map<int,NodeProperties>::iterator it = m_nodeProperties.find(n);
		if(it != m_nodeProperties.end()) return it->second;
		else throw Exception(OSSWrapper() << "No such node: " << n);
	}

	const NodeProperties& node_properties(int n) const
	{
		typename std::map<int,NodeProperties>::const_iterator it = m_nodeProperties.find(n);
		if(it != m_nodeProperties.end()) return it->second;
		else throw Exception(OSSWrapper() << "No such node: " << n);
	}

	NodePropertiesCIter node_properties_cbegin() const
	{
		return m_nodeProperties.begin();
	}

	NodePropertiesCIter node_properties_cend() const
	{
		return m_nodeProperties.end();
	}

	void remove_edge(int u, int v)
	{
		typename EdgeContainer::const_iterator it = m_edges.find(make_edge_tuple(u, v));
		if(it != m_edges.end()) m_edges.erase(it);
		else throw Exception(OSSWrapper() << "No such edge: {" << u << ',' << v << '}');
	}

	void remove_node(int n)
	{
		// Remove the node's properties.
		typename std::map<int,NodeProperties>::iterator it = m_nodeProperties.find(n);
		if(it == m_nodeProperties.end()) throw Exception(OSSWrapper() << "No such node: " << n);
		m_nodeProperties.erase(it);

		// Remove any edges connected to the node.
		m_edges.get<tagSmaller>().erase(n);
		m_edges.get<tagLarger>().erase(n);
	}

	void set_edge_weight(int u, int v, EdgeWeight weight)
	{
		if(u == v) throw Exception(OSSWrapper() << "Reflexive edges are not allowed: " << u);
		if(!has_node(u)) throw Exception(OSSWrapper() << "No such node: " << u);
		if(!has_node(v)) throw Exception(OSSWrapper() << "No such node: " << v);

		Edge e = make_edge(u, v, weight);
		typename EdgeContainer::iterator it = m_edges.find(boost::make_tuple(e.u, e.v));
		if(it != m_edges.end()) m_edges.replace(it, e);
		else m_edges.insert(e);
	}

	void set_node_properties(int n, const NodeProperties& properties)
	{
		m_nodeProperties[n] = properties;
	}

	//#################### PRIVATE METHODS ####################
private:
	static Edge make_edge(int u, int v, EdgeWeight weight)
	{
		int smaller = u < v ? u : v;
		int larger = u < v ? v : u;
		return Edge(smaller, larger, weight);
	}

	static boost::tuples::tuple<int,int> make_edge_tuple(int u, int v)
	{
		int smaller = u < v ? u : v;
		int larger = u < v ? v : u;
		return boost::make_tuple(smaller, larger);
	}
};

}

using mp_AdjacencyGraph::AdjacencyGraph;

}

#endif

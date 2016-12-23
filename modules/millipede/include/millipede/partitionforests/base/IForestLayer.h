/***
 * millipede: IForestLayer.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_IFORESTLAYER
#define H_MILLIPEDE_IFORESTLAYER

#include <iterator>
#include <set>
#include <vector>

#include <boost/shared_ptr.hpp>

#include <millipede/adts/WeightedEdge.h>

namespace mp {

template <typename BranchProperties, typename EdgeWeight>
class IForestLayer
{
	//#################### TYPEDEFS ####################
public:
	typedef WeightedEdge<EdgeWeight> Edge;

	//#################### NESTED CLASSES (EXCLUDING ITERATORS) ####################
public:
	class Node
	{
	public:
		virtual ~Node() {}
		virtual int parent() const = 0;
		virtual void set_parent(int parent) = 0;
	};

	//#################### ITERATORS ####################
protected:
	class EdgeConstIteratorImplBase
	{
	public:
		virtual ~EdgeConstIteratorImplBase() {}
		virtual const Edge& operator*() const = 0;
		virtual const Edge *operator->() const = 0;
		virtual EdgeConstIteratorImplBase& operator++() = 0;
		virtual bool operator==(const EdgeConstIteratorImplBase& rhs) const = 0;

		bool operator!=(const EdgeConstIteratorImplBase& rhs) const	{ return !(*this == rhs); }
	};

	template <typename N>
	class NodeIteratorImplBaseT
	{
	public:
		virtual ~NodeIteratorImplBaseT() {}
		virtual N& operator*() const = 0;
		virtual N *operator->() const = 0;
		virtual NodeIteratorImplBaseT& operator++() = 0;
		virtual bool operator==(const NodeIteratorImplBaseT& rhs) const = 0;
		virtual int index() const = 0;

		bool operator!=(const NodeIteratorImplBaseT& rhs) const	{ return !(*this == rhs); }
	};

	typedef NodeIteratorImplBaseT<Node> NodeIteratorImplBase;
	typedef NodeIteratorImplBaseT<const Node> NodeIteratorConstImplBase;

public:
	class EdgeConstIterator : public std::iterator<std::input_iterator_tag, Edge>
	{
	private:
		boost::shared_ptr<EdgeConstIteratorImplBase> m_impl;
	public:
		explicit EdgeConstIterator(EdgeConstIteratorImplBase *impl)
		:	m_impl(impl)
		{}

		const Edge& operator*() const	{ return m_impl->operator*(); }
		const Edge *operator->() const	{ return m_impl->operator->(); }

		EdgeConstIterator& operator++()
		{
			++(*m_impl);
			return *this;
		}

		bool operator==(const EdgeConstIterator& rhs) const	{ return *m_impl == *rhs.m_impl; }
		bool operator!=(const EdgeConstIterator& rhs) const	{ return *m_impl != *rhs.m_impl; }
	};

protected:
	template <typename N, typename Impl>
	class NodeIteratorT : public std::iterator<std::input_iterator_tag, N>
	{
	private:
		boost::shared_ptr<Impl> m_impl;
	public:
		explicit NodeIteratorT(Impl *impl)
		:	m_impl(impl)
		{}

		N& operator*() const	{ return m_impl->operator*(); }
		N *operator->() const	{ return m_impl->operator->(); }

		NodeIteratorT& operator++()
		{
			++(*m_impl);
			return *this;
		}

		bool operator==(const NodeIteratorT& rhs) const	{ return *m_impl == *rhs.m_impl; }
		bool operator!=(const NodeIteratorT& rhs) const	{ return *m_impl != *rhs.m_impl; }

		int index() const	{ return m_impl->index(); }
	};

public:
	typedef NodeIteratorT<Node, NodeIteratorImplBase> NodeIterator;
	typedef NodeIteratorT<const Node, NodeIteratorConstImplBase> NodeConstIterator;

	//#################### DESTRUCTOR ####################
public:
	virtual ~IForestLayer() {}

	//#################### PUBLIC ABSTRACT METHODS ####################
public:
	virtual std::vector<Edge> adjacent_edges(int n) const = 0;
	virtual std::vector<int> adjacent_nodes(int n) const = 0;
	virtual BranchProperties combine_properties(const std::set<int>& nodeIndices) const = 0;
	virtual EdgeWeight edge_weight(int u, int v) const = 0;
	virtual EdgeConstIterator edges_cbegin() const = 0;
	virtual EdgeConstIterator edges_cend() const = 0;
	virtual bool has_edge(int u, int v) const = 0;
	virtual bool has_node(int n) const = 0;
	virtual int node_count() const = 0;
	virtual std::vector<int> node_indices() const = 0;
	virtual int node_parent(int n) const = 0;
	virtual NodeIterator nodes_begin() = 0;
	virtual NodeConstIterator nodes_cbegin() const = 0;
	virtual NodeConstIterator nodes_cend() const = 0;
	virtual NodeIterator nodes_end() = 0;
	virtual void set_node_parent(int n, int parent) = 0;
};

}

#endif

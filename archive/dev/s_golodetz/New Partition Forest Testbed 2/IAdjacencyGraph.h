/***
 * millipede: IAdjacencyGraph.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_IADJACENCYGRAPH
#define H_MILLIPEDE_IADJACENCYGRAPH

#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

#include "Exception.h"
#include "Iterator.h"
#include "OSSWrapper.h"

namespace mp {

template <typename Node, typename EdgeValue>
class IAdjacencyGraph
{
	//#################### NESTED CLASSES ####################
public:
	class Edge
	{
		//#################### PRIVATE VARIABLES ####################
	private:
		int m_u, m_v;
		EdgeValue m_value;

		//#################### CONSTRUCTORS ####################
	public:
		Edge(int u, int v, const EdgeValue& value)
		:	m_u(u), m_v(v), m_value(value)
		{}

		//#################### PUBLIC METHODS ####################
	public:
		int u() const				{ return m_u; }
		int v() const				{ return m_v; }
		EdgeValue value() const		{ return m_value; }
	};

	//#################### TYPEDEFS ####################
public:
	typedef EdgeValue EdgeValue;
	typedef Node Node;
	typedef shared_ptr<const Node> Node_CPtr;

private:
	typedef Iterator<Edge> EdgeIterator;
	typedef Iterator<std::pair<int,Node_CPtr> > NodeCIterator;

public:
	typedef shared_ptr<EdgeIterator> EdgeIterator_Ptr;
	typedef shared_ptr<NodeCIterator> NodeCIterator_Ptr;

	//#################### DESTRUCTOR ####################
public:
	virtual ~IAdjacencyGraph() {}

	//#################### PUBLIC ABSTRACT OPERATORS ####################
public:
	virtual Node& operator()(int n) = 0;
	virtual const Node& operator()(int n) const = 0;
	virtual EdgeValue operator()(int u, int v) const = 0;

	//#################### PUBLIC ABSTRACT METHODS ####################
public:
	virtual EdgeIterator_Ptr adjacent_edges(int n) const = 0;
	virtual NodeCIterator_Ptr adjacent_nodes(int n) const = 0;
	virtual EdgeIterator_Ptr edges() const = 0;
	virtual bool has_edge(int u, int v) const = 0;
	virtual bool has_node(int n) const = 0;
	virtual int node_count() const = 0;
	virtual NodeCIterator_Ptr nodes() const = 0;

	//#################### PROTECTED METHODS ####################
protected:
	void check_node(int n) const
	{
		if(!has_node(n)) throw Exception(OSSWrapper() << "No such node: " << n);
	}
};

}

#endif

/***
 * millipede: IAdjacencyGraph.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_IADJACENCYGRAPH
#define H_MILLIPEDE_IADJACENCYGRAPH

#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

#include "Iterator.h"

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
		mutable EdgeValue m_value;

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
protected:
	typedef shared_ptr<Node> Node_Ptr;
	typedef shared_ptr<const Node> Node_CPtr;

	typedef Iterator<const Edge&> EdgeCRefIterator;
	typedef Iterator<std::pair<int,Node_Ptr> > NodeRefIterator;
	typedef Iterator<std::pair<int,Node_CPtr> > NodeCRefIterator;

public:
	typedef shared_ptr<EdgeCRefIterator> EdgeCRefIterator_Ptr;
	typedef shared_ptr<NodeRefIterator> NodeRefIterator_Ptr;
	typedef shared_ptr<NodeCRefIterator> NodeCRefIterator_Ptr;

	//#################### DESTRUCTOR ####################
public:
	virtual ~IAdjacencyGraph() {}

	//#################### PUBLIC ABSTRACT OPERATORS ####################
public:
	virtual const Node& operator()(int n) const = 0;
	virtual EdgeValue operator()(int u, int v) const = 0;

	//#################### PUBLIC ABSTRACT METHODS ####################
public:
	virtual EdgeCRefIterator_Ptr adjacent_edges(int n) const = 0;
	virtual EdgeCRefIterator_Ptr edges() const = 0;
	virtual bool has_edge(int u, int v) const = 0;
	virtual bool has_node(int n) const = 0;
	virtual NodeCRefIterator_Ptr nodes() const = 0;
};

}

#endif

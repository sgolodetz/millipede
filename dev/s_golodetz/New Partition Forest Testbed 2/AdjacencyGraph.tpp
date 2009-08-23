/***
 * millipede: AdjacencyGraph.tpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include <boost/lexical_cast.hpp>

#define AdjacencyGraph_HEADER	template <typename Node, typename EdgeValue>
#define AdjacencyGraph_THIS		AdjacencyGraph<Node,EdgeValue>

namespace mp {

//#################### CONSTRUCTORS ####################
AdjacencyGraph_HEADER
AdjacencyGraph_THIS::AdjacencyGraph()
{}

//#################### COPY CONSTRUCTOR & ASSIGNMENT OPERATOR ####################
AdjacencyGraph_HEADER
AdjacencyGraph_THIS::AdjacencyGraph(const AdjacencyGraph& rhs)
:	m_edges(rhs.m_edges)
{
	for(std::map<int,Node_Ptr>::const_iterator it=rhs.m_nodes.begin(), iend=rhs.m_nodes.end(); it!=iend; ++it)
	{
		m_nodes.insert(std::make_pair(it->first, Node_Ptr(new Node(*it->second))));
	}
}

//#################### PUBLIC OPERATORS ####################
AdjacencyGraph_HEADER
Node& AdjacencyGraph_THIS::operator()(int n)
{
	return const_cast<Node&>(const_cast<const AdjacencyGraph*>(this)->operator()(n));
}

AdjacencyGraph_HEADER
const Node& AdjacencyGraph_THIS::operator()(int n) const
{
	NodeMap::const_iterator it = m_nodes.find(n);
	if(it != m_nodes.end()) return *it->second;
	else throw Exception(OSSWrapper() << "No such node: " << n);
}

AdjacencyGraph_HEADER
EdgeValue AdjacencyGraph_THIS::operator()(int u, int v) const
{
	check_node(u);
	check_node(v);

	int smaller = u <= v ? u : v;
	int larger = u <= v ? v : u;

	EdgeTable::const_iterator it = m_edges.find(boost::make_tuple(smaller,larger));
	if(it != m_edges.end()) return it->value();
	else throw Exception(OSSWrapper() << "No edge exists between " << smaller << " and " << larger);
}

//#################### PUBLIC METHODS ####################
AdjacencyGraph_HEADER
void AdjacencyGraph_THIS::add_edge(int u, int v, const EdgeValue& value)
{
	check_node(u);
	check_node(v);

	int smaller = u <= v ? u : v;
	int larger = u <= v ? v : u;

	std::pair<EdgeTable::iterator,bool> result = m_edges.insert(Edge(smaller, larger, value));
	if(!result.second) throw Exception(OSSWrapper() << "Edge already exists between " << smaller << " and " << larger);
}

AdjacencyGraph_HEADER
void AdjacencyGraph_THIS::add_node(int n, const Node_Ptr& node)
{
	std::pair<NodeMap::iterator,bool> result = m_nodes.insert(std::make_pair(n, node));
	if(!result.second) throw Exception(OSSWrapper() << "Node already exists: " << n);
}

AdjacencyGraph_HEADER
typename AdjacencyGraph_THIS::EdgeIterator_Ptr
AdjacencyGraph_THIS::adjacent_edges(int n) const
{
	typedef EdgeTable::index<source>::type EdgeTableBySrc;
	typedef EdgeTable::index<dest>::type EdgeTableByDest;

	const EdgeTableBySrc& srcTable = m_edges.get<source>();
	const EdgeTableByDest& destTable = m_edges.get<dest>();

	return EdgeIterator_Ptr(new AdjEdgesIter(srcTable.lower_bound(n), srcTable.upper_bound(n), destTable.lower_bound(n), destTable.upper_bound(n)));
}

AdjacencyGraph_HEADER
typename AdjacencyGraph_THIS::NodeIterator_Ptr
AdjacencyGraph_THIS::adjacent_nodes(int n)
{
	return NodeIterator_Ptr(new AdjNodesIter<Node,NodeMap,NodeMap::iterator>(n, adjacent_edges(n), m_nodes));
}

AdjacencyGraph_HEADER
typename AdjacencyGraph_THIS::NodeCIterator_Ptr
AdjacencyGraph_THIS::adjacent_nodes(int n) const
{
	return NodeCIterator_Ptr(new AdjNodesIter<const Node,const NodeMap,NodeMap::const_iterator>(n, adjacent_edges(n), m_nodes));
}

AdjacencyGraph_HEADER
typename AdjacencyGraph_THIS::EdgeIterator_Ptr
AdjacencyGraph_THIS::edges() const
{
	return EdgeIterator_Ptr(new AllEdgesIter(m_edges));
}

AdjacencyGraph_HEADER
bool AdjacencyGraph_THIS::has_edge(int u, int v) const
{
	check_node(u);
	check_node(v);

	int smaller = u <= v ? u : v;
	int larger = u <= v ? v : u;

	return m_edges.find(boost::make_tuple(smaller,larger)) != m_edges.end();
}

AdjacencyGraph_HEADER
bool AdjacencyGraph_THIS::has_node(int n) const
{
	return m_nodes.find(n) != m_nodes.end();
}

AdjacencyGraph_HEADER
int AdjacencyGraph_THIS::node_count() const
{
	return static_cast<int>(m_nodes.size());
}

AdjacencyGraph_HEADER
typename AdjacencyGraph_THIS::NodeIterator_Ptr
AdjacencyGraph_THIS::nodes()
{
	return NodeIterator_Ptr(new AllNodesIter<Node,NodeMap,NodeMap::iterator>(m_nodes));
}

AdjacencyGraph_HEADER
typename AdjacencyGraph_THIS::NodeCIterator_Ptr
AdjacencyGraph_THIS::nodes() const
{
	return NodeCIterator_Ptr(new AllNodesIter<const Node,const NodeMap,NodeMap::const_iterator>(m_nodes));
}

AdjacencyGraph_HEADER
void AdjacencyGraph_THIS::remove_edge(int u, int v)
{
	check_node(u);
	check_node(v);

	int smaller = u <= v ? u : v;
	int larger = u <= v ? v : u;

	EdgeTable::iterator it = m_edges.find(boost::make_tuple(smaller, larger));
	if(it != m_edges.end()) m_edges.erase(it);
	else throw Exception(OSSWrapper() << "No edge exists between " << smaller << " and " << larger);
}

AdjacencyGraph_HEADER
void AdjacencyGraph_THIS::remove_node(int n)
{
	check_node(n);

	m_nodes.erase(n);
	m_edges.get<source>().erase(n);
	m_edges.get<dest>().erase(n);
}

AdjacencyGraph_HEADER
void AdjacencyGraph_THIS::replace_edge(int u, int v, const EdgeValue& value)
{
	check_node(u);
	check_node(v);

	int smaller = u <= v ? u : v;
	int larger = u <= v ? v : u;

	EdgeTable::iterator it = m_edges.find(boost::make_tuple(smaller, larger));
	if(it != m_edges.end()) m_edges.replace(it, Edge(smaller, larger, value));
	else throw Exception(OSSWrapper() << "No edge exists between " << smaller << " and " << larger);
}

//#################### NESTED CLASSES ####################
AdjacencyGraph_HEADER
class AdjacencyGraph_THIS::AdjEdgesIter : public Iterator<Edge>
{
	//#################### TYPEDEFS ####################
private:
	typedef typename EdgeTable::index<source>::type EdgeTableBySrc;
	typedef typename EdgeTable::index<dest>::type EdgeTableByDest;
	typedef typename EdgeTableBySrc::const_iterator SrcIter;
	typedef typename EdgeTableByDest::const_iterator DestIter;

	//#################### PRIVATE VARIABLES ####################
private:
	SrcIter m_srcBegin, m_srcEnd, m_srcIt;
	DestIter m_destBegin, m_destEnd, m_destIt;

	//#################### CONSTRUCTORS ####################
public:
	AdjEdgesIter(const SrcIter& srcBegin, const SrcIter& srcEnd, const DestIter& destBegin, const DestIter& destEnd)
	:	m_srcBegin(srcBegin), m_srcEnd(srcEnd), m_srcIt(srcBegin),
		m_destBegin(destBegin), m_destEnd(destEnd), m_destIt(destBegin)
	{}

	//#################### PUBLIC METHODS ####################
public:
	bool has_next() const
	{
		return m_srcIt != m_srcEnd || m_destIt != m_destEnd;
	}

	Edge next()
	{
		if(m_srcIt != m_srcEnd)
		{
			const Edge& ret = *m_srcIt;
			++m_srcIt;
			return ret;
		}
		else if(m_destIt != m_destEnd)
		{
			const Edge& ret = *m_destIt;
			++m_destIt;
			return ret;
		}
		else throw Exception("Trying to read more adjacent edges than actually exist");
	}
};

AdjacencyGraph_HEADER
template <typename T, typename Map, typename Iter>
class AdjacencyGraph_THIS::AdjNodesIter : public Iterator<std::pair<int,shared_ptr<T> > >
{
	//#################### PRIVATE VARIABLES ####################
private:
	int m_curNode;
	EdgeIterator_Ptr m_adjEdgesIt;
	Map& m_nodes;

	//#################### CONSTRUCTORS ####################
public:
	AdjNodesIter(int curNode, const EdgeIterator_Ptr& adjEdgesIt, Map& nodes)
	:	m_curNode(curNode), m_adjEdgesIt(adjEdgesIt), m_nodes(nodes)
	{}

	//#################### PUBLIC METHODS ####################
public:
	bool has_next() const
	{
		return m_adjEdgesIt->has_next();
	}

	std::pair<int,shared_ptr<T> > next()
	{
		Edge adjEdge = m_adjEdgesIt->next();

		int otherNode;
		if(adjEdge.u() != m_curNode)	otherNode = adjEdge.u();
		else							otherNode = adjEdge.v();

		return *m_nodes.find(otherNode);
	}
};

AdjacencyGraph_HEADER
class AdjacencyGraph_THIS::AllEdgesIter : public Iterator<Edge>
{
	//#################### PRIVATE VARIABLES ####################
private:
	const EdgeTable& m_edges;
	typename EdgeTable::const_iterator m_it;

	//#################### CONSTRUCTORS ####################
public:
	explicit AllEdgesIter(const EdgeTable& edges)
	:	m_edges(edges), m_it(m_edges.begin())
	{}

	//#################### PUBLIC METHODS ####################
public:
	bool has_next() const
	{
		return m_it != m_edges.end();
	}

	Edge next()
	{
		const Edge& ret = *m_it;
		++m_it;
		return ret;
	}
};

AdjacencyGraph_HEADER
template <typename T, typename Map, typename Iter>
class AdjacencyGraph_THIS::AllNodesIter : public Iterator<std::pair<int,shared_ptr<T> > >
{
	//#################### PRIVATE VARIABLES ####################
private:
	Map& m_nodes;
	Iter m_it;

	//#################### CONSTRUCTORS ####################
public:
	explicit AllNodesIter(Map& nodes)
	:	m_nodes(nodes), m_it(m_nodes.begin())
	{}

	//#################### PUBLIC METHODS ####################
public:
	bool has_next() const
	{
		return m_it != m_nodes.end();
	}

	std::pair<int,shared_ptr<T> > next()
	{
		std::pair<int,shared_ptr<T> > ret = std::make_pair(m_it->first, m_it->second);
		++m_it;
		return ret;
	}
};

}

#undef AdjacencyGraph_HEADER
#undef AdjacencyGraph_THIS

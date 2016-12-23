/***
 * millipede: ImageBranchLayer.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_IMAGEBRANCHLAYER
#define H_MILLIPEDE_IMAGEBRANCHLAYER

#include <set>

#include <common/adts/AdjacencyGraph.h>
#include <common/io/util/OSSWrapper.h>
#include <common/partitionforests/base/IForestLayer.h>

namespace mp {

template <typename BranchProperties>
class ImageBranchLayer : public IForestLayer<BranchProperties,int>
{
	//#################### TYPEDEFS ####################
private:
	typedef IForestLayer<BranchProperties,int> Base;
public:
	typedef int EdgeWeight;
	typedef WeightedEdge<EdgeWeight> Edge;
	typedef BranchProperties NodeProperties;

private:
	typedef AdjacencyGraph<BranchProperties,int> GraphType;

	//#################### NESTED CLASSES (EXCLUDING ITERATORS ####################
private:
	struct ForestLinks
	{
		std::set<int> m_children;
		int m_parent;

		ForestLinks()
		:	m_parent(-1)
		{}
	};

public:
	class BranchNode : public Base::Node
	{
	private:
		typename std::map<int,ForestLinks>::iterator m_forestLinksIt;
		typename GraphType::NodePropertiesCIter m_nodePropertiesIt;
	public:
		explicit BranchNode(const typename std::map<int,ForestLinks>::iterator& forestLinksIt, const typename GraphType::NodePropertiesCIter& nodePropertiesIt)
		:	m_forestLinksIt(forestLinksIt), m_nodePropertiesIt(nodePropertiesIt)
		{}

		std::set<int>& children()					{ return m_forestLinksIt->second.m_children; }
		const std::set<int>& children() const		{ return m_forestLinksIt->second.m_children; }
		int parent() const							{ return m_forestLinksIt->second.m_parent; }
		const NodeProperties& properties() const	{ return m_nodePropertiesIt->second; }
		void set_parent(int parent)					{ m_forestLinksIt->second.m_parent = parent; }
	};

	//#################### ITERATORS ####################
private:
	class EdgeConstIteratorImpl : public Base::EdgeConstIteratorImplBase
	{
	private:
		typename GraphType::EdgeCIter m_it;
	public:
		EdgeConstIteratorImpl(const typename GraphType::EdgeCIter& it)
		:	m_it(it)
		{}

		const Edge& operator*() const	{ return *m_it; }
		const Edge *operator->() const	{ return m_it.operator->(); }

		EdgeConstIteratorImpl& operator++()
		{
			++m_it;
			return *this;
		}

		bool operator==(const typename Base::EdgeConstIteratorImplBase& baseRhs) const
		{
			const EdgeConstIteratorImpl& rhs = static_cast<const EdgeConstIteratorImpl&>(baseRhs);
			return m_it == rhs.m_it;
		}
	};

	template <typename N>
	class BranchNodeIteratorImplT : public Base::template NodeIteratorImplBaseT<N>
	{
	private:
		typename std::map<int,ForestLinks>::iterator m_forestLinksIt;
		typename std::map<int,ForestLinks>::iterator m_forestLinksEnd;
		typename GraphType::NodePropertiesCIter m_nodePropertiesIt;
		boost::shared_ptr<BranchNode> m_cur;

	public:
		BranchNodeIteratorImplT(const typename std::map<int,ForestLinks>::iterator& forestLinksIt, const typename std::map<int,ForestLinks>::iterator& forestLinksEnd,
								const typename GraphType::NodePropertiesCIter& nodePropertiesIt)
		:	m_forestLinksIt(forestLinksIt), m_forestLinksEnd(forestLinksEnd), m_nodePropertiesIt(nodePropertiesIt)
		{
			set_cur();
		}

		N& operator*() const	{ return *m_cur; }
		N *operator->() const	{ return m_cur.get(); }

		BranchNodeIteratorImplT& operator++()
		{
			++m_forestLinksIt;
			++m_nodePropertiesIt;
			set_cur();
			return *this;
		}

		bool operator==(const typename Base::template NodeIteratorImplBaseT<N>& baseRhs) const
		{
			const BranchNodeIteratorImplT& rhs = static_cast<const BranchNodeIteratorImplT&>(baseRhs);
			return m_forestLinksIt == rhs.m_forestLinksIt;
		}

		int index() const	{ return m_forestLinksIt->first; }

	private:
		void set_cur()
		{
			if(m_forestLinksIt != m_forestLinksEnd) m_cur.reset(new BranchNode(m_forestLinksIt, m_nodePropertiesIt));
			else m_cur.reset();
		}
	};

	typedef BranchNodeIteratorImplT<BranchNode> BranchNodeIteratorImpl;
	typedef BranchNodeIteratorImplT<const BranchNode> BranchNodeConstIteratorImpl;
	typedef BranchNodeIteratorImplT<typename Base::Node> NodeIteratorImpl;
	typedef BranchNodeIteratorImplT<const typename Base::Node> NodeConstIteratorImpl;

public:
	typedef typename Base::template NodeIteratorT<BranchNode, BranchNodeIteratorImpl> BranchNodeIterator;
	typedef typename Base::template NodeIteratorT<const BranchNode, BranchNodeConstIteratorImpl> BranchNodeConstIterator;

	//#################### PRIVATE VARIABLES ####################
private:
	GraphType m_graph;
	mutable std::map<int,ForestLinks> m_forestLinks;	// note: this is mutable because we need to be able to get non-const iterators to pass to a BranchNodeIterator

	//#################### CONSTRUCTORS ####################
public:
	ImageBranchLayer() {}

	//#################### COPY CONSTRUCTOR & ASSIGNMENT OPERATOR ####################
private:
	ImageBranchLayer(const ImageBranchLayer&);
	ImageBranchLayer& operator=(const ImageBranchLayer&);

	//#################### PUBLIC METHODS ####################
public:
	std::vector<Edge> adjacent_edges(int n) const
	{
		return m_graph.adjacent_edges(n);
	}

	std::vector<int> adjacent_nodes(int n) const
	{
		return m_graph.adjacent_nodes(n);
	}

	BranchNodeIterator branch_nodes_begin()
	{
		return BranchNodeIterator(new BranchNodeIteratorImpl(m_forestLinks.begin(), m_forestLinks.end(), m_graph.node_properties_cbegin()));
	}

	BranchNodeConstIterator branch_nodes_cbegin() const
	{
		return BranchNodeConstIterator(new BranchNodeConstIteratorImpl(m_forestLinks.begin(), m_forestLinks.end(), m_graph.node_properties_cbegin()));
	}

	BranchNodeConstIterator branch_nodes_cend() const
	{
		return BranchNodeConstIterator(new BranchNodeConstIteratorImpl(m_forestLinks.end(), m_forestLinks.end(), m_graph.node_properties_cend()));
	}

	BranchNodeIterator branch_nodes_end()
	{
		return BranchNodeIterator(new BranchNodeIteratorImpl(m_forestLinks.end(), m_forestLinks.end(), m_graph.node_properties_cend()));
	}

	BranchProperties combine_properties(const std::set<int>& nodeIndices) const
	{
		std::vector<BranchProperties> properties;
		properties.reserve(nodeIndices.size());
		for(std::set<int>::const_iterator it=nodeIndices.begin(), iend=nodeIndices.end(); it!=iend; ++it)
		{
			properties.push_back(node_properties(*it));
		}
		return BranchProperties::combine_branch_properties(properties);
	}

	EdgeWeight edge_weight(int u, int v) const
	{
		return m_graph.edge_weight(u, v);
	}

	std::vector<Edge> edges() const
	{
		return m_graph.edges();
	}

	typename Base::EdgeConstIterator edges_cbegin() const
	{
		return typename Base::EdgeConstIterator(new EdgeConstIteratorImpl(m_graph.edges_cbegin()));
	}

	typename Base::EdgeConstIterator edges_cend() const
	{
		return typename Base::EdgeConstIterator(new EdgeConstIteratorImpl(m_graph.edges_cend()));
	}

	bool has_edge(int u, int v) const
	{
		return m_graph.has_edge(u, v);
	}

	bool has_node(int n) const
	{
		return m_graph.has_node(n);
	}

	std::set<int>& node_children(int n)
	{
		typename std::map<int,ForestLinks>::iterator it = m_forestLinks.find(n);
		if(it != m_forestLinks.end()) return it->second.m_children;
		else throw Exception(OSSWrapper() << "No such node: " << n);
	}

	const std::set<int>& node_children(int n) const
	{
		typename std::map<int,ForestLinks>::const_iterator it = m_forestLinks.find(n);
		if(it != m_forestLinks.end()) return it->second.m_children;
		else throw Exception(OSSWrapper() << "No such node: " << n);
	}

	int node_count() const
	{
		return m_graph.node_count();
	}

	std::vector<int> node_indices() const
	{
		return m_graph.node_indices();
	}

	int node_parent(int n) const
	{
		typename std::map<int,ForestLinks>::const_iterator it = m_forestLinks.find(n);
		if(it != m_forestLinks.end()) return it->second.m_parent;
		else throw Exception(OSSWrapper() << "No such node: " << n);
	}

	const NodeProperties& node_properties(int n) const
	{
		return m_graph.node_properties(n);
	}

	typename Base::NodeIterator nodes_begin()
	{
		return typename Base::NodeIterator(new NodeIteratorImpl(m_forestLinks.begin(), m_forestLinks.end(), m_graph.node_properties_cbegin()));
	}

	typename Base::NodeConstIterator nodes_cbegin() const
	{
		return typename Base::NodeConstIterator(new NodeConstIteratorImpl(m_forestLinks.begin(), m_forestLinks.end(), m_graph.node_properties_cbegin()));
	}

	typename Base::NodeConstIterator nodes_cend() const
	{
		return typename Base::NodeConstIterator(new NodeConstIteratorImpl(m_forestLinks.end(), m_forestLinks.end(), m_graph.node_properties_cend()));
	}

	typename Base::NodeIterator nodes_end()
	{
		return typename Base::NodeIterator(new NodeIteratorImpl(m_forestLinks.end(), m_forestLinks.end(), m_graph.node_properties_cend()));
	}

	void remove_edge(int u, int v)
	{
		m_graph.remove_edge(u, v);
	}

	void remove_node(int n)
	{
		m_graph.remove_node(n);
		m_forestLinks.erase(n);
	}

	void set_edge_weight(int u, int v, EdgeWeight weight)
	{
		m_graph.set_edge_weight(u, v, weight);
	}

	void set_node_children(int n, const std::set<int>& children)
	{
		m_forestLinks[n].m_children = children;
	}

	void set_node_parent(int n, int parent)
	{
		m_forestLinks[n].m_parent = parent;
	}

	void set_node_properties(int n, const NodeProperties& properties)
	{
		m_graph.set_node_properties(n, properties);
		m_forestLinks[n];	// "touch" the forest link to create it if it doesn't exist
	}

	void update_edge_weight(int u, int v, EdgeWeight weight)
	{
		if(has_edge(u, v))
		{
			EdgeWeight oldWeight = edge_weight(u, v);
			if(weight < oldWeight) set_edge_weight(u, v, weight);
		}
		else set_edge_weight(u, v, weight);
	}
};

}

#endif

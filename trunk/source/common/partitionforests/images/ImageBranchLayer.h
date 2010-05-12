/***
 * millipede: ImageBranchLayer.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_IMAGEBRANCHLAYER
#define H_MILLIPEDE_IMAGEBRANCHLAYER

#include <set>

#include <common/adts/AdjacencyGraph.h>
#include <common/partitionforests/base/IForestLayer.h>
#include "RegionProperties.h"

namespace mp {

class ImageBranchLayer : public IForestLayer<RegionProperties,int>
{
	//#################### TYPEDEFS ####################
public:
	typedef int EdgeWeight;
	typedef WeightedEdge<EdgeWeight> Edge;
	typedef RegionProperties NodeProperties;

private:
	typedef AdjacencyGraph<RegionProperties,int> GraphType;

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
	class BranchNode : public Node
	{
	private:
		std::map<int,ForestLinks>::iterator m_forestLinksIt;
		GraphType::NodePropertiesCIter m_nodePropertiesIt;
	public:
		explicit BranchNode(const std::map<int,ForestLinks>::iterator& forestLinksIt, const GraphType::NodePropertiesCIter& nodePropertiesIt)
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
	class EdgeConstIteratorImpl : public EdgeConstIteratorImplBase
	{
	private:
		GraphType::EdgeCIter m_it;
	public:
		EdgeConstIteratorImpl(const GraphType::EdgeCIter& it)
		:	m_it(it)
		{}

		const Edge& operator*() const	{ return *m_it; }
		const Edge *operator->() const	{ return m_it.operator->(); }

		EdgeConstIteratorImpl& operator++()
		{
			++m_it;
			return *this;
		}

		bool operator==(const EdgeConstIteratorImplBase& baseRhs) const
		{
			const EdgeConstIteratorImpl& rhs = static_cast<const EdgeConstIteratorImpl&>(baseRhs);
			return m_it == rhs.m_it;
		}
	};

	template <typename N>
	class BranchNodeIteratorImplT : public NodeIteratorImplBaseT<N>
	{
	private:
		std::map<int,ForestLinks>::iterator m_forestLinksIt;
		std::map<int,ForestLinks>::iterator m_forestLinksEnd;
		GraphType::NodePropertiesCIter m_nodePropertiesIt;
		boost::shared_ptr<BranchNode> m_cur;

	public:
		BranchNodeIteratorImplT(const std::map<int,ForestLinks>::iterator& forestLinksIt, const std::map<int,ForestLinks>::iterator& forestLinksEnd,
								const GraphType::NodePropertiesCIter& nodePropertiesIt)
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

		bool operator==(const NodeIteratorImplBaseT<N>& baseRhs) const
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
	typedef BranchNodeIteratorImplT<Node> NodeIteratorImpl;
	typedef BranchNodeIteratorImplT<const Node> NodeConstIteratorImpl;

public:
	typedef NodeIteratorT<BranchNode, BranchNodeIteratorImpl> BranchNodeIterator;
	typedef NodeIteratorT<const BranchNode, BranchNodeConstIteratorImpl> BranchNodeConstIterator;

	//#################### PRIVATE VARIABLES ####################
private:
	GraphType m_graph;
	mutable std::map<int,ForestLinks> m_forestLinks;	// note: this is mutable because we need to be able to get non-const iterators to pass to a BranchNodeIterator

	//#################### CONSTRUCTORS ####################
public:
	ImageBranchLayer();

	//#################### COPY CONSTRUCTOR & ASSIGNMENT OPERATOR ####################
private:
	ImageBranchLayer(const ImageBranchLayer&);
	ImageBranchLayer& operator=(const ImageBranchLayer&);

	//#################### PUBLIC METHODS ####################
public:
	std::vector<Edge> adjacent_edges(int n) const;
	std::vector<int> adjacent_nodes(int n) const;
	BranchNodeIterator branch_nodes_begin();
	BranchNodeConstIterator branch_nodes_cbegin() const;
	BranchNodeConstIterator branch_nodes_cend() const;
	BranchNodeIterator branch_nodes_end();
	RegionProperties combine_properties(const std::set<int>& nodeIndices) const;
	EdgeWeight edge_weight(int u, int v) const;
	std::vector<Edge> edges() const;
	EdgeConstIterator edges_cbegin() const;
	EdgeConstIterator edges_cend() const;
	bool has_edge(int u, int v) const;
	bool has_node(int n) const;
	std::set<int>& node_children(int n);
	const std::set<int>& node_children(int n) const;
	std::vector<int> node_indices() const;
	int node_parent(int n) const;
	const NodeProperties& node_properties(int n) const;
	NodeIterator nodes_begin();
	NodeConstIterator nodes_cbegin() const;
	NodeConstIterator nodes_cend() const;
	NodeIterator nodes_end();
	void remove_edge(int u, int v);
	void remove_node(int n);
	void set_edge_weight(int u, int v, EdgeWeight weight);
	void set_node_children(int n, const std::set<int>& children);
	void set_node_parent(int n, int parent);
	void set_node_properties(int n, const NodeProperties& properties);
	void update_edge_weight(int u, int v, EdgeWeight weight);
};

}

#endif

/***
 * millipede: ImageLeafLayer.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_IMAGELEAFLAYER
#define H_MILLIPEDE_IMAGELEAFLAYER

#include <common/exceptions/Exception.h>
#include <common/partitionforests/base/IForestLayer.h>
#include "RegionProperties.h"

namespace mp {

class ImageLeafLayer : public IForestLayer<RegionProperties,int>
{
	//#################### TYPEDEFS ####################
public:
	typedef int EdgeWeight;
	typedef WeightedEdge<EdgeWeight> Edge;
	typedef PixelProperties NodeProperties;

	//#################### CONSTANTS ####################
private:
	enum EdgeDir
	{
		FINISHED = -1,
		RIGHT = 0,
		DOWN = 1,
	};

	//#################### NESTED CLASSES (EXCLUDING ITERATORS) ####################
public:
	class LeafNode : public Node
	{
	private:
		int m_parent;
		NodeProperties m_properties;
	public:
		explicit LeafNode(const NodeProperties& properties)
		:	m_parent(-1), m_properties(properties)
		{}

		int parent() const							{ return m_parent; }
		const NodeProperties& properties() const	{ return m_properties; }
		void set_parent(int parent)					{ m_parent = parent; }
	};

	//#################### ITERATORS ####################
private:
	class EdgeConstIteratorImpl : public EdgeConstIteratorImplBase
	{
	private:
		const ImageLeafLayer *m_leafLayer;
		int m_currentNode;
		EdgeDir m_currentDir;

		mutable boost::shared_ptr<Edge> m_currentEdge;

	public:
		EdgeConstIteratorImpl(const ImageLeafLayer *leafLayer, int currentNode, EdgeDir currentDir)
		:	m_leafLayer(leafLayer), m_currentNode(currentNode), m_currentDir(currentDir)
		{}

		const Edge& operator*() const
		{
			if(!m_currentEdge)
			{
				int u = m_currentNode, v;
				if(m_currentDir == RIGHT)		v = m_currentNode + 1;
				else if(m_currentDir == DOWN)	v = m_currentNode + m_leafLayer->m_width;
				else throw Exception("Iterator does not refer to a valid edge");

				m_currentEdge.reset(new Edge(u, v, m_leafLayer->edge_weight(u,v)));
			}
			return *m_currentEdge;
		}

		const Edge *operator->() const
		{
			if(!m_currentEdge) operator*();
			return m_currentEdge.get();
		}

		EdgeConstIteratorImpl& operator++()
		{
			if(m_currentDir == RIGHT)
			{
				++m_currentNode;
				int col = m_currentNode % m_leafLayer->m_width;
				if(col == m_leafLayer->m_width-1) ++m_currentNode;
				if(m_currentNode == m_leafLayer->m_width * m_leafLayer->m_height)
				{
					m_currentNode = 0;
					m_currentDir = DOWN;
				}
			}
			else if(m_currentDir == DOWN)
			{
				++m_currentNode;
				int row = m_currentNode / m_leafLayer->m_width;
				if(row == m_leafLayer->m_height-1)
				{
					m_currentNode = -1;
					m_currentDir = FINISHED;
				}
			}
			else throw Exception("Iterator does not refer to a valid edge");

			m_currentEdge.reset();

			return *this;
		}

		bool operator==(const EdgeConstIteratorImplBase& baseRhs) const
		{
			const EdgeConstIteratorImpl& rhs = static_cast<const EdgeConstIteratorImpl&>(baseRhs);
			return m_currentNode == rhs.m_currentNode && m_currentDir == rhs.m_currentDir;
		}
	};

	// Note: Either Vec = std::vector<LeafNode> or Vec = const std::vector<LeafNode>.
	template <typename N, typename LeafNodeVec>
	class LeafNodeIteratorImplT : public NodeIteratorImplBaseT<N>
	{
	private:
		int m_index;
		LeafNodeVec& m_nodes;
	public:
		LeafNodeIteratorImplT(int index, LeafNodeVec& nodes)
		:	m_index(index), m_nodes(nodes)
		{}

		N& operator*() const	{ return m_nodes[m_index]; }
		N *operator->() const	{ return &m_nodes[m_index]; }

		LeafNodeIteratorImplT& operator++()
		{
			++m_index;
			return *this;
		}

		bool operator==(const NodeIteratorImplBaseT<N>& baseRhs) const
		{
			const LeafNodeIteratorImplT& rhs = static_cast<const LeafNodeIteratorImplT&>(baseRhs);
			return m_index == rhs.m_index;
		}

		int index() const
		{
			return m_index;
		}
	};

public:
	typedef LeafNodeIteratorImplT<LeafNode, std::vector<LeafNode> > LeafNodeIteratorImpl;
	typedef LeafNodeIteratorImplT<const LeafNode, const std::vector<LeafNode> > LeafNodeConstIteratorImpl;
	typedef LeafNodeIteratorImplT<Node, std::vector<LeafNode> > NodeIteratorImpl;
	typedef LeafNodeIteratorImplT<const Node, const std::vector<LeafNode> > NodeConstIteratorImpl;

	typedef NodeIteratorT<LeafNode, LeafNodeIteratorImpl> LeafNodeIterator;
	typedef NodeIteratorT<const LeafNode, LeafNodeConstIteratorImpl> LeafNodeConstIterator;

	//#################### PRIVATE VARIABLES ####################
private:
	int m_width;
	int m_height;
	std::vector<LeafNode> m_nodes;

	//#################### CONSTRUCTORS ####################
public:
	ImageLeafLayer(int width, int height, const std::vector<NodeProperties>& nodeProperties);

	//#################### PUBLIC METHODS ####################
public:
	std::vector<Edge> adjacent_edges(int n) const;
	std::vector<int> adjacent_nodes(int n) const;
	RegionProperties combine_properties(const std::set<int>& nodeIndices) const;
	EdgeWeight edge_weight(int u, int v) const;
	EdgeConstIterator edges_cbegin() const;
	EdgeConstIterator edges_cend() const;
	bool has_edge(int u, int v) const;
	bool has_node(int n) const;
	LeafNodeIterator leaf_nodes_begin();
	LeafNodeConstIterator leaf_nodes_cbegin() const;
	LeafNodeConstIterator leaf_nodes_cend() const;
	LeafNodeIterator leaf_nodes_end();
	std::vector<int> node_indices() const;
	int node_parent(int n) const;
	const PixelProperties& node_properties(int n) const;
	NodeIterator nodes_begin();
	NodeConstIterator nodes_cbegin() const;
	NodeConstIterator nodes_cend() const;
	NodeIterator nodes_end();
	void set_node_parent(int n, int parent);
};

}

#endif

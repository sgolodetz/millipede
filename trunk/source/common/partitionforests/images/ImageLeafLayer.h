/***
 * millipede: ImageLeafLayer.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_IMAGELEAFLAYER
#define H_MILLIPEDE_IMAGELEAFLAYER

#include <vector>

#include <itkImage.h>

#include <common/adts/WeightedEdge.h>
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

private:
	typedef itk::Image<signed int,3> HounsfieldImage;
	typedef HounsfieldImage::Pointer HounsfieldImagePointer;
	typedef itk::Image<unsigned char,3> WindowedImage;
	typedef WindowedImage::Pointer WindowedImagePointer;

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

	//#################### CONSTANTS ####################
private:
	enum EdgeDir
	{
		NONE,
		XPOS,	// +x direction
		YPOS,	// +y direction
		ZPOS,	// +z direction
	};

	//#################### ITERATORS ####################
private:
	class EdgeConstIteratorImpl : public EdgeConstIteratorImplBase
	{
	private:
		const ImageLeafLayer *m_base;
		int m_currentNode;
		EdgeDir m_currentDir;
		mutable boost::shared_ptr<Edge> m_currentEdge;

	public:
		EdgeConstIteratorImpl(const ImageLeafLayer *base, int currentNode)
		:	m_base(base), m_currentNode(currentNode), m_currentDir(NONE)
		{
			if(m_currentNode != m_base->m_sizeXYZ) advance();
		}

		const Edge& operator*() const	{ return *m_currentEdge; }
		const Edge *operator->() const	{ return m_currentEdge.get(); }

		EdgeConstIteratorImpl& operator++()
		{
			advance();
			return *this;
		}

		bool operator==(const EdgeConstIteratorImplBase& baseRhs) const
		{
			const EdgeConstIteratorImpl& rhs = static_cast<const EdgeConstIteratorImpl&>(baseRhs);
			return m_currentNode == rhs.m_currentNode && m_currentDir == rhs.m_currentDir;
		}

	private:
		void advance()
		{
			do
			{
				advance_single();
			}
			while(!is_valid());

			make_current_edge();
		}

		void advance_single()
		{
			if(m_currentDir == ZPOS)
			{
				++m_currentNode;
				m_currentDir = m_currentNode != m_base->m_sizeXYZ ? XPOS : NONE;
			}
			else m_currentDir = EdgeDir(m_currentDir + 1);
		}

		bool is_valid() const
		{
			if(m_currentNode == m_base->m_sizeXYZ) return true;
			switch(m_currentDir)
			{
				case XPOS:	return m_base->x_of(m_currentNode) != m_base->m_sizeX - 1;
				case YPOS:	return m_base->y_of(m_currentNode) != m_base->m_sizeY - 1;
				case ZPOS:	return m_base->z_of(m_currentNode) != m_base->m_sizeZ - 1;
				default:	throw Exception("Unexpectedly bad edge direction");		// this should never happen
			}
		}

		void make_current_edge()
		{
			if(m_currentNode != m_base->m_sizeXYZ)
			{
				int v;
				switch(m_currentDir)
				{
					case XPOS:	v = m_currentNode + 1;					break;
					case YPOS:	v = m_currentNode + m_base->m_sizeX;	break;
					case ZPOS:	v = m_currentNode + m_base->m_sizeXY;	break;
					default:	throw Exception("Cannot make current edge");	// this should never happen
				}
				m_currentEdge.reset(new Edge(m_currentNode, v, m_base->edge_weight(m_currentNode, v)));
			}
			else m_currentEdge.reset();
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
	int m_sizeX, m_sizeY, m_sizeZ, m_sizeXY, m_sizeXYZ;
	std::vector<LeafNode> m_nodes;

	//#################### CONSTRUCTORS ####################
public:
	ImageLeafLayer(int sizeX, int sizeY, int sizeZ, const std::vector<NodeProperties>& nodeProperties);
	ImageLeafLayer(const HounsfieldImagePointer& hounsfieldImage, const WindowedImagePointer& windowedImage);

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

	//#################### PRIVATE METHODS ####################
private:
	int x_of(int n) const;
	int y_of(int n) const;
	int z_of(int n) const;
};

}

#endif

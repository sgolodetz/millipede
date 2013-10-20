/***
 * millipede: ImageLeafLayer.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 * Modified by Varduhi Yeghiazaryan, 2013.
 ***/

#ifndef H_MILLIPEDE_IMAGELEAFLAYER
#define H_MILLIPEDE_IMAGELEAFLAYER

#include <vector>

#include <common/adts/WeightedEdge.h>
#include <common/exceptions/Exception.h>
#include <common/math/Vector3.h>
#include <common/partitionforests/base/IForestLayer.h>
#include <common/util/GridUtil.h>

namespace mp {

template <typename LeafProperties, typename BranchProperties>
class ImageLeafLayer : public IForestLayer<BranchProperties,int>
{
	//#################### TYPEDEFS ####################
private:
	typedef IForestLayer<BranchProperties,int> Base;
public:
	typedef int EdgeWeight;
	typedef WeightedEdge<EdgeWeight> Edge;
	typedef LeafProperties NodeProperties;

	//#################### NESTED CLASSES (EXCLUDING ITERATORS) ####################
public:
	class LeafNode : public Base::Node
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
	class EdgeConstIteratorImpl : public Base::EdgeConstIteratorImplBase
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

		bool operator==(const typename Base::EdgeConstIteratorImplBase& baseRhs) const
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
	class LeafNodeIteratorImplT : public Base::template NodeIteratorImplBaseT<N>
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

		bool operator==(const typename Base::template NodeIteratorImplBaseT<N>& baseRhs) const
		{
			const LeafNodeIteratorImplT& rhs = static_cast<const LeafNodeIteratorImplT&>(baseRhs);
			return m_index == rhs.m_index;
		}

		int index() const
		{
			return m_index;
		}
	};

	typedef LeafNodeIteratorImplT<LeafNode, std::vector<LeafNode> > LeafNodeIteratorImpl;
	typedef LeafNodeIteratorImplT<const LeafNode, const std::vector<LeafNode> > LeafNodeConstIteratorImpl;
	typedef LeafNodeIteratorImplT<typename Base::Node, std::vector<LeafNode> > NodeIteratorImpl;
	typedef LeafNodeIteratorImplT<const typename Base::Node, const std::vector<LeafNode> > NodeConstIteratorImpl;

public:
	typedef typename Base::template NodeIteratorT<LeafNode, LeafNodeIteratorImpl> LeafNodeIterator;
	typedef typename Base::template NodeIteratorT<const LeafNode, LeafNodeConstIteratorImpl> LeafNodeConstIterator;

	//#################### PROTECTED VARIABLES ####################
protected:
	int m_sizeX, m_sizeY, m_sizeZ, m_sizeXY, m_sizeXYZ;
	std::vector<LeafNode> m_nodes;

	//#################### CONSTRUCTORS ####################
public:
	ImageLeafLayer(const std::vector<NodeProperties>& nodeProperties, int sizeX, int sizeY, int sizeZ = 1)
	{
		initialise(nodeProperties, sizeX, sizeY, sizeZ);
	}

protected:
	ImageLeafLayer() {}

	//#################### DESTRUCTOR ####################
protected:
	~ImageLeafLayer() {}

	//#################### PUBLIC ABSTRACT METHODS ####################
public:
	virtual EdgeWeight edge_weight(int u, int v) const = 0;

	//#################### PUBLIC METHODS ####################
public:
	std::vector<Edge> adjacent_edges(int n) const
	{
		// Note: This is a 6-connected implementation.
		std::vector<Edge> ret;

		int x = x_of(n), y = y_of(n), z = z_of(n);
		if(z != 0)				ret.push_back(Edge(n - m_sizeXY, n, edge_weight(n - m_sizeXY, n)));
		if(y != 0)				ret.push_back(Edge(n - m_sizeX, n, edge_weight(n - m_sizeX, n)));
		if(x != 0)				ret.push_back(Edge(n - 1, n, edge_weight(n - 1, n)));
		if(x != m_sizeX - 1)	ret.push_back(Edge(n, n + 1, edge_weight(n, n + 1)));
		if(y != m_sizeY - 1)	ret.push_back(Edge(n, n + m_sizeX, edge_weight(n, n + m_sizeX)));
		if(z != m_sizeZ - 1)	ret.push_back(Edge(n, n + m_sizeXY, edge_weight(n, n + m_sizeXY)));

		return ret;
	}

	std::vector<int> adjacent_nodes(int n) const
	{
		// Note: This is a 6-connected implementation.
		std::vector<int> ret;

		int x = x_of(n), y = y_of(n), z = z_of(n);
		if(z != 0)				ret.push_back(n - m_sizeXY);
		if(y != 0)				ret.push_back(n - m_sizeX);
		if(x != 0)				ret.push_back(n - 1);
		if(x != m_sizeX - 1)	ret.push_back(n + 1);
		if(y != m_sizeY - 1)	ret.push_back(n + m_sizeX);
		if(z != m_sizeZ - 1)	ret.push_back(n + m_sizeXY);

		return ret;
	}

	BranchProperties combine_properties(const std::set<int>& nodeIndices) const
	{
		std::vector<std::pair<Vector3i,LeafProperties> > properties;
		properties.reserve(nodeIndices.size());
		for(std::set<int>::const_iterator it=nodeIndices.begin(), iend=nodeIndices.end(); it!=iend; ++it)
		{
			properties.push_back(std::make_pair(position_of(*it), node_properties(*it)));
		}
		return BranchProperties::combine_leaf_properties(properties);
	}

	typename Base::EdgeConstIterator edges_cbegin() const
	{
		return typename Base::EdgeConstIterator(new EdgeConstIteratorImpl(this, 0));
	}

	typename Base::EdgeConstIterator edges_cend() const
	{
		return typename Base::EdgeConstIterator(new EdgeConstIteratorImpl(this, m_sizeXYZ));
	}

	bool has_edge(int u, int v) const
	{
		// Note: This is a 6-connected implementation.
		if(!has_node(u) || !has_node(v)) return false;
		int ux = x_of(u), uy = y_of(u), uz = z_of(u);
		int vx = x_of(v), vy = y_of(v), vz = z_of(v);
		int xdiff = abs(ux - vx), ydiff = abs(uy - vy), zdiff = abs(uz - vz);
		return xdiff + ydiff + zdiff == 1;	// note that each is either 0 or 1
	}

	bool has_node(int n) const
	{
		return 0 <= n && n < static_cast<int>(m_nodes.size());
	}

	LeafNodeIterator leaf_nodes_begin()
	{
		return LeafNodeIterator(new LeafNodeIteratorImpl(0, m_nodes));
	}

	LeafNodeConstIterator leaf_nodes_cbegin() const
	{
		return LeafNodeConstIterator(new LeafNodeConstIteratorImpl(0, m_nodes));
	}

	LeafNodeConstIterator leaf_nodes_cend() const
	{
		return LeafNodeConstIterator(new LeafNodeConstIteratorImpl(static_cast<int>(m_nodes.size()), m_nodes));
	}

	LeafNodeIterator leaf_nodes_end()
	{
		return LeafNodeIterator(new LeafNodeIteratorImpl(static_cast<int>(m_nodes.size()), m_nodes));
	}

	int node_count() const
	{
		return static_cast<int>(m_nodes.size());
	}

	std::vector<int> node_indices() const
	{
		int size = static_cast<int>(m_nodes.size());
		std::vector<int> ret(size);
		for(int i=0; i<size; ++i)
		{
			ret[i] = i;
		}
		return ret;
	}

	// Precondition: n is in the right range
	int node_parent(int n) const
	{
		return m_nodes[n].parent();
	}

	// Precondition: n is in the right range
	const LeafProperties& node_properties(int n) const
	{
		return m_nodes[n].properties();
	}

	typename Base::NodeIterator nodes_begin()
	{
		return typename Base::NodeIterator(new NodeIteratorImpl(0, m_nodes));
	}

	typename Base::NodeConstIterator nodes_cbegin() const
	{
		return typename Base::NodeConstIterator(new NodeConstIteratorImpl(0, m_nodes));
	}

	typename Base::NodeConstIterator nodes_cend() const
	{
		return typename Base::NodeConstIterator(new NodeConstIteratorImpl(static_cast<int>(m_nodes.size()), m_nodes));
	}

	typename Base::NodeIterator nodes_end()
	{
		return typename Base::NodeIterator(new NodeIteratorImpl(static_cast<int>(m_nodes.size()), m_nodes));
	}

	// Precondition: n is in the right range
	void set_node_parent(int n, int parent)
	{
		m_nodes[n].set_parent(parent);
	}

	int size_x() const
	{
		return m_sizeX;
	}

	int size_y() const
	{
		return m_sizeY;
	}

	int size_z() const
	{
		return m_sizeZ;
	}
		
	//#################### PROTECTED METHODS ####################
protected:
	void initialise(const std::vector<NodeProperties>& nodeProperties, int sizeX, int sizeY, int sizeZ = 1)
	{
		m_sizeX = sizeX;
		m_sizeY = sizeY;
		m_sizeZ = sizeZ;
		m_sizeXY = m_sizeX * m_sizeY;
		m_sizeXYZ = m_sizeXY * m_sizeZ;

		size_t size = nodeProperties.size();
		m_nodes.reserve(size);
		for(size_t i=0; i<size; ++i) m_nodes.push_back(LeafNode(nodeProperties[i]));
	}

	//#################### PRIVATE METHODS ####################
private:
	Vector3i position_of(int n) const	{ return Vector3i(x_of(n), y_of(n), z_of(n)); }
	int x_of(int n) const				{ return GridUtil::x_of(n, m_sizeX); }
	int y_of(int n) const				{ return GridUtil::y_of(n, m_sizeX, m_sizeY); }
	int z_of(int n) const				{ return GridUtil::z_of(n, m_sizeXY); }
};

}

#endif

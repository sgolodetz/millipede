/***
 * millipede: ImageLeafLayer.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "ImageLeafLayer.h"

namespace mp {

//#################### CONSTRUCTORS ####################
ImageLeafLayer::ImageLeafLayer(int width, int height, const std::vector<NodeProperties>& nodeProperties)
:	m_width(width), m_height(height)
{
	size_t size = nodeProperties.size();
	m_nodes.reserve(size);
	for(size_t i=0; i<size; ++i) m_nodes.push_back(LeafNode(nodeProperties[i]));
}

//#################### PUBLIC METHODS ####################
std::vector<ImageLeafLayer::Edge> ImageLeafLayer::adjacent_edges(int n) const
{
	std::vector<Edge> ret;

	int x = n % m_width, y = n / m_width;
	if(y != 0)				ret.push_back(Edge(n - m_width, n, edge_weight(n - m_width, n)));
	if(x != 0)				ret.push_back(Edge(n - 1, n, edge_weight(n - 1, n)));
	if(x != m_width - 1)	ret.push_back(Edge(n, n + 1, edge_weight(n,  n + 1)));
	if(y != m_height - 1)	ret.push_back(Edge(n, n + m_width, edge_weight(n, n + m_width)));

	return ret;
}

std::vector<int> ImageLeafLayer::adjacent_nodes(int n) const
{
	std::vector<int> ret;

	int x = n % m_width, y = n / m_width;
	if(y != 0)				ret.push_back(n - m_width);
	if(x != 0)				ret.push_back(n - 1);
	if(x != m_width - 1)	ret.push_back(n + 1);
	if(y != m_height - 1)	ret.push_back(n + m_width);

	return ret;
}

RegionProperties ImageLeafLayer::combine_properties(const std::set<int>& nodeIndices) const
{
	std::vector<PixelProperties> properties;
	properties.reserve(nodeIndices.size());
	for(std::set<int>::const_iterator it=nodeIndices.begin(), iend=nodeIndices.end(); it!=iend; ++it)
	{
		properties.push_back(node_properties(*it));
	}
	return RegionProperties::combine_leaf_properties(properties);
}

// Precondition: has_edge(u, v)
ImageLeafLayer::EdgeWeight ImageLeafLayer::edge_weight(int u, int v) const
{
	return abs(m_nodes[u].properties().grey_value() - m_nodes[v].properties().grey_value());
}

ImageLeafLayer::EdgeConstIterator ImageLeafLayer::edges_cbegin() const
{
	return EdgeConstIterator(new EdgeConstIteratorImpl(this, 0, RIGHT));
}

ImageLeafLayer::EdgeConstIterator ImageLeafLayer::edges_cend() const
{
	return EdgeConstIterator(new EdgeConstIteratorImpl(this, -1, FINISHED));
}

bool ImageLeafLayer::has_edge(int u, int v) const
{
	if(!has_node(u) || !has_node(v)) return false;
	int ux = u % m_width, uy = u / m_width, vx = v % m_width, vy = v / m_width;
	int xdiff = abs(ux - vx), ydiff = abs(uy - vy);
	return (xdiff == 1 && ydiff == 0) || (xdiff == 0 && ydiff == 1);
}

bool ImageLeafLayer::has_node(int n) const
{
	return 0 <= n && n < static_cast<int>(m_nodes.size());
}

ImageLeafLayer::LeafNodeIterator ImageLeafLayer::leaf_nodes_begin()
{
	return LeafNodeIterator(new LeafNodeIteratorImpl(0, m_nodes));
}

ImageLeafLayer::LeafNodeConstIterator ImageLeafLayer::leaf_nodes_cbegin() const
{
	return LeafNodeConstIterator(new LeafNodeConstIteratorImpl(0, m_nodes));
}

ImageLeafLayer::LeafNodeConstIterator ImageLeafLayer::leaf_nodes_cend() const
{
	return LeafNodeConstIterator(new LeafNodeConstIteratorImpl(static_cast<int>(m_nodes.size()), m_nodes));
}

ImageLeafLayer::LeafNodeIterator ImageLeafLayer::leaf_nodes_end()
{
	return LeafNodeIterator(new LeafNodeIteratorImpl(static_cast<int>(m_nodes.size()), m_nodes));
}

std::vector<int> ImageLeafLayer::node_indices() const
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
int ImageLeafLayer::node_parent(int n) const
{
	return m_nodes[n].parent();
}

// Precondition: n is in the right range
const PixelProperties& ImageLeafLayer::node_properties(int n) const
{
	return m_nodes[n].properties();
}

ImageLeafLayer::NodeIterator ImageLeafLayer::nodes_begin()
{
	return NodeIterator(new NodeIteratorImpl(0, m_nodes));
}

ImageLeafLayer::NodeConstIterator ImageLeafLayer::nodes_cbegin() const
{
	return NodeConstIterator(new NodeConstIteratorImpl(0, m_nodes));
}

ImageLeafLayer::NodeConstIterator ImageLeafLayer::nodes_cend() const
{
	return NodeConstIterator(new NodeConstIteratorImpl(static_cast<int>(m_nodes.size()), m_nodes));
}

ImageLeafLayer::NodeIterator ImageLeafLayer::nodes_end()
{
	return NodeIterator(new NodeIteratorImpl(static_cast<int>(m_nodes.size()), m_nodes));
}

// Precondition: n is in the right range
void ImageLeafLayer::set_node_parent(int n, int parent)
{
	m_nodes[n].set_parent(parent);
}

}

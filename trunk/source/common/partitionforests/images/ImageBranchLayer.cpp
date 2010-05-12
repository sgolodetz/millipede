/***
 * millipede: ImageBranchLayer.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "ImageBranchLayer.h"

#include <boost/lexical_cast.hpp>
using boost::lexical_cast;

namespace mp {

//#################### CONSTRUCTORS ####################
ImageBranchLayer::ImageBranchLayer()
{}

//#################### PUBLIC METHODS ####################
std::vector<ImageBranchLayer::Edge> ImageBranchLayer::adjacent_edges(int n) const
{
	return m_graph.adjacent_edges(n);
}

std::vector<int> ImageBranchLayer::adjacent_nodes(int n) const
{
	return m_graph.adjacent_nodes(n);
}

ImageBranchLayer::BranchNodeIterator ImageBranchLayer::branch_nodes_begin()
{
	return BranchNodeIterator(new BranchNodeIteratorImpl(m_forestLinks.begin(), m_forestLinks.end(), m_graph.node_properties_cbegin()));
}

ImageBranchLayer::BranchNodeConstIterator ImageBranchLayer::branch_nodes_cbegin() const
{
	return BranchNodeConstIterator(new BranchNodeConstIteratorImpl(m_forestLinks.begin(), m_forestLinks.end(), m_graph.node_properties_cbegin()));
}

ImageBranchLayer::BranchNodeConstIterator ImageBranchLayer::branch_nodes_cend() const
{
	return BranchNodeConstIterator(new BranchNodeConstIteratorImpl(m_forestLinks.end(), m_forestLinks.end(), m_graph.node_properties_cend()));
}

ImageBranchLayer::BranchNodeIterator ImageBranchLayer::branch_nodes_end()
{
	return BranchNodeIterator(new BranchNodeIteratorImpl(m_forestLinks.end(), m_forestLinks.end(), m_graph.node_properties_cend()));
}

RegionProperties ImageBranchLayer::combine_properties(const std::set<int>& nodeIndices) const
{
	std::vector<RegionProperties> properties;
	properties.reserve(nodeIndices.size());
	for(std::set<int>::const_iterator it=nodeIndices.begin(), iend=nodeIndices.end(); it!=iend; ++it)
	{
		properties.push_back(node_properties(*it));
	}
	return RegionProperties::combine_branch_properties(properties);
}

ImageBranchLayer::EdgeWeight ImageBranchLayer::edge_weight(int u, int v) const
{
	return m_graph.edge_weight(u, v);
}

std::vector<ImageBranchLayer::Edge> ImageBranchLayer::edges() const
{
	return m_graph.edges();
}

ImageBranchLayer::EdgeConstIterator ImageBranchLayer::edges_cbegin() const
{
	return EdgeConstIterator(new EdgeConstIteratorImpl(m_graph.edges_cbegin()));
}

ImageBranchLayer::EdgeConstIterator ImageBranchLayer::edges_cend() const
{
	return EdgeConstIterator(new EdgeConstIteratorImpl(m_graph.edges_cend()));
}

bool ImageBranchLayer::has_edge(int u, int v) const
{
	return m_graph.has_edge(u, v);
}

bool ImageBranchLayer::has_node(int n) const
{
	return m_graph.has_node(n);
}

std::set<int>& ImageBranchLayer::node_children(int n)
{
	std::map<int,ForestLinks>::iterator it = m_forestLinks.find(n);
	if(it != m_forestLinks.end()) return it->second.m_children;
	else throw Exception("No such node: " + lexical_cast<std::string>(n));
}

const std::set<int>& ImageBranchLayer::node_children(int n) const
{
	std::map<int,ForestLinks>::const_iterator it = m_forestLinks.find(n);
	if(it != m_forestLinks.end()) return it->second.m_children;
	else throw Exception("No such node: " + lexical_cast<std::string>(n));
}

std::vector<int> ImageBranchLayer::node_indices() const
{
	return m_graph.node_indices();
}

int ImageBranchLayer::node_parent(int n) const
{
	std::map<int,ForestLinks>::const_iterator it = m_forestLinks.find(n);
	if(it != m_forestLinks.end()) return it->second.m_parent;
	else throw Exception("No such node: " + lexical_cast<std::string>(n));
}

const ImageBranchLayer::NodeProperties& ImageBranchLayer::node_properties(int n) const
{
	return m_graph.node_properties(n);
}

ImageBranchLayer::NodeIterator ImageBranchLayer::nodes_begin()
{
	return NodeIterator(new NodeIteratorImpl(m_forestLinks.begin(), m_forestLinks.end(), m_graph.node_properties_cbegin()));
}

ImageBranchLayer::NodeConstIterator ImageBranchLayer::nodes_cbegin() const
{
	return NodeConstIterator(new NodeConstIteratorImpl(m_forestLinks.begin(), m_forestLinks.end(), m_graph.node_properties_cbegin()));
}

ImageBranchLayer::NodeConstIterator ImageBranchLayer::nodes_cend() const
{
	return NodeConstIterator(new NodeConstIteratorImpl(m_forestLinks.end(), m_forestLinks.end(), m_graph.node_properties_cend()));
}

ImageBranchLayer::NodeIterator ImageBranchLayer::nodes_end()
{
	return NodeIterator(new NodeIteratorImpl(m_forestLinks.end(), m_forestLinks.end(), m_graph.node_properties_cend()));
}

void ImageBranchLayer::remove_edge(int u, int v)
{
	m_graph.remove_edge(u, v);
}

void ImageBranchLayer::remove_node(int n)
{
	m_graph.remove_node(n);
	m_forestLinks.erase(n);
}

void ImageBranchLayer::set_edge_weight(int u, int v, EdgeWeight weight)
{
	m_graph.set_edge_weight(u, v, weight);
}

void ImageBranchLayer::set_node_children(int n, const std::set<int>& children)
{
	m_forestLinks[n].m_children = children;
}

void ImageBranchLayer::set_node_parent(int n, int parent)
{
	m_forestLinks[n].m_parent = parent;
}

void ImageBranchLayer::set_node_properties(int n, const NodeProperties& properties)
{
	m_graph.set_node_properties(n, properties);
	m_forestLinks[n];	// "touch" the forest link to create it if it doesn't exist
}

void ImageBranchLayer::update_edge_weight(int u, int v, EdgeWeight weight)
{
	if(has_edge(u, v))
	{
		EdgeWeight oldWeight = edge_weight(u, v);
		if(weight < oldWeight) set_edge_weight(u, v, weight);
	}
	else set_edge_weight(u, v, weight);
}

}

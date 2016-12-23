/***
 * millipede: DisjointSetForest.tpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#define DisjointSetForest_HEADER	template <typename T>
#define DisjointSetForest_THIS		DisjointSetForest<T>

#include "Exception.h"
#include "OSSWrapper.h"

namespace mp {

//#################### CONSTRUCTORS ####################
DisjointSetForest_HEADER
DisjointSetForest_THIS::DisjointSetForest()
:	m_treeCount(0)
{}

DisjointSetForest_HEADER
DisjointSetForest_THIS::DisjointSetForest(const std::map<int,T>& initialSets)
:	m_treeCount(0)
{
	add_nodes(initialSets);
}

//#################### PUBLIC METHODS ####################
DisjointSetForest_HEADER
void DisjointSetForest_THIS::add_node(int x, const T& value)
{
	m_nodes.insert(std::make_pair(x, Node(value, x)));
	++m_treeCount;
}

DisjointSetForest_HEADER
void DisjointSetForest_THIS::add_nodes(const std::map<int,T>& nodes)
{
	for(std::map<int,T>::const_iterator it=nodes.begin(), iend=nodes.end(); it!=iend; ++it)
	{
		m_nodes.insert(std::make_pair(it->first, Node(it->second, it->first)));
	}
	m_treeCount += nodes.size();
}

DisjointSetForest_HEADER
int DisjointSetForest_THIS::find_set(int x) const
{
	Node& node = get_node(x);
	int& parent = node.m_parent;
	if(parent != x)
	{
		parent = find_set(parent);
	}
	return parent;
}

DisjointSetForest_HEADER
int DisjointSetForest_THIS::node_count() const
{
	return static_cast<int>(m_nodes.size());
}

DisjointSetForest_HEADER
int DisjointSetForest_THIS::tree_count() const
{
	return m_treeCount;
}

DisjointSetForest_HEADER
void DisjointSetForest_THIS::union_nodes(int x, int y)
{
	int setX = find_set(x);
	int setY = find_set(y);
	if(setX != setY) link(setX, setY);
}

DisjointSetForest_HEADER
T& DisjointSetForest_THIS::value_of(int x)
{
	return get_node(x).m_value;
}

DisjointSetForest_HEADER
const T& DisjointSetForest_THIS::value_of(int x) const
{
	return get_node(x).m_value;
}

//#################### PRIVATE METHODS ####################
DisjointSetForest_HEADER
typename DisjointSetForest_THIS::Node&
DisjointSetForest_THIS::get_node(int x) const
{
	std::map<int,Node>::iterator it = m_nodes.find(x);
	if(it != m_nodes.end()) return it->second;
	else throw Exception(OSSWrapper() << "No such node: " << x);
}

DisjointSetForest_HEADER
void DisjointSetForest_THIS::link(int x, int y)
{
	Node& nodeX = get_node(x);
	Node& nodeY = get_node(y);
	int& rankX = nodeX.m_rank;
	int& rankY = nodeY.m_rank;
	if(rankX > rankY)
	{
		nodeY.m_parent = x;
	}
	else
	{
		nodeX.m_parent = y;
		if(rankX == rankY) ++rankY;
	}
	--m_treeCount;
}

}

#undef DisjointSetForest_HEADER
#undef DisjointSetForest_THIS

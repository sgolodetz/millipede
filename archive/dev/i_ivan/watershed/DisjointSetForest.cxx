/***
 * centipede_basics: DisjointSetForest.tpp
 * Copyright Stuart Golodetz, 2009.
 ***/
#include "DisjointSetForest.h"

namespace cp {

//################## CONSTRUCTORS ##################//
template <typename T>
DisjointSetForest<T>::DisjointSetForest()
:	m_treeCount(0)
{}

template <typename T>
DisjointSetForest<T>::DisjointSetForest(const std::vector<T>& initialSets)
:	m_treeCount(0)
{
	add_nodes(initialSets);
}

//################## PUBLIC METHODS ##################//
template <typename T>
void DisjointSetForest<T>::add_node(const T& value)
{
	m_nodes.push_back(Node(value, node_count()));
	++m_treeCount;
}

template <typename T>
void DisjointSetForest<T>::add_nodes(const std::vector<T>& values)
{
	m_nodes.reserve(values.size());
	int i = 0;
	for(typename std::vector<T>::const_iterator it=values.begin(), iend=values.end(); it!=iend; ++it)
	{
		m_nodes.push_back(Node(*it, i++));
	}
	m_treeCount += static_cast<int>(values.size());
}

template <typename T>
int DisjointSetForest<T>::find_set(int x) const
{
	check_range(x);

	int& parent = m_nodes[x].m_parent;
	if(parent != x)
	{
		parent = find_set(parent);
	}
	return parent;
}

template <typename T>
int DisjointSetForest<T>::node_count() const
{
	return static_cast<int>(m_nodes.size());
}

template <typename T>
int DisjointSetForest<T>::tree_count() const
{
	return m_treeCount;
}

template <typename T>
void DisjointSetForest<T>::union_nodes(int x, int y)
{
	int setX = find_set(x);
	int setY = find_set(y);
	if(setX != setY) link(setX, setY);
}

template <typename T>
T& DisjointSetForest<T>::value_of(int x)
{
	check_range(x);
	return m_nodes[x].m_value;
}

template <typename T>
const T& DisjointSetForest<T>::value_of(int x) const
{
	check_range(x);
	return m_nodes[x].m_value;
}

//################## PRIVATE METHODS ##################//
template <typename T>
void DisjointSetForest<T>::check_range(int x) const
{
	if(x < 0 || x >= node_count()) throw std::exception();
}

template <typename T>
void DisjointSetForest<T>::link(int x, int y)
{
	int& rankX = m_nodes[x].m_rank;
	int& rankY = m_nodes[y].m_rank;
	if(rankX > rankY)
	{
		m_nodes[y].m_parent = x;
	}
	else
	{
		m_nodes[x].m_parent = y;
		if(rankX == rankY) ++rankY;
	}
	--m_treeCount;
}

}


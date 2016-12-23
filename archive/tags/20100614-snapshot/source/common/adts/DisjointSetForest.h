/***
 * millipede: DisjointSetForest.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_DISJOINTSETFOREST
#define H_MILLIPEDE_DISJOINTSETFOREST

#include <map>

namespace mp {

struct DSFDummy {};

template <typename T = DSFDummy>
class DisjointSetForest
{
	//#################### NESTED CLASSES ####################
private:
	struct Node
	{
		T m_value;
		int m_parent;
		int m_rank;

		Node(const T& value, int parent)
		:	m_value(value), m_parent(parent), m_rank(0)
		{}
	};

	//#################### PRIVATE VARIABLES ####################
private:
	mutable std::map<int,Node> m_nodes;
	int m_treeCount;

	//#################### CONSTRUCTORS ####################
public:
	DisjointSetForest();
	explicit DisjointSetForest(const std::map<int,T>& initialSets);

	//#################### PUBLIC METHODS ####################
public:
	void add_node(int x, const T& value = T());
	void add_nodes(const std::map<int,T>& nodes);
	int find_set(int x) const;
	int node_count() const;
	int tree_count() const;
	void union_nodes(int x, int y);
	T& value_of(int x);
	const T& value_of(int x) const;

	//#################### PRIVATE METHODS ####################
private:
	Node& get_node(int x) const;
	void link(int x, int y);
};

}

#include "DisjointSetForest.tpp"

#endif

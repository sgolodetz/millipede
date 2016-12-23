/***
 * centipede_basics: DisjointSetForest.h
 * Copyright Stuart Golodetz, 2009.
 ***/

#ifndef H_CENTIPEDE_BASICS_DATASTRUCTURES_DISJOINTSETFOREST
#define H_CENTIPEDE_BASICS_DATASTRUCTURES_DISJOINTSETFOREST

#include <vector>

#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

namespace cp {

template <typename T>
class DisjointSetForest
{
	//################## NESTED CLASSES ##################//
private:
	struct Node
	{
		T m_value;
		int m_parent;
		int m_rank;

		explicit Node(const T& value, int parent)
		:	m_value(value), m_parent(parent), m_rank(0)
		{}
	};

	//################## PRIVATE VARIABLES ##################//
private:
	mutable std::vector<Node> m_nodes;
	int m_treeCount;

	//################## CONSTRUCTORS ##################//
public:
	DisjointSetForest();
	explicit DisjointSetForest(const std::vector<T>& initialSets);

	//################## PUBLIC METHODS ##################//
public:
	/**
	Adds a new node with the specified value to the forest.
	*/
	void add_node(const T& value);

	/**
	Adds a number of new nodes with the specified values to the forest.
	*/
	void add_nodes(const std::vector<T>& values);

	/**
	Finds the label of the set to which the node with index x belongs.
	*/
	int find_set(int x) const;

	/**
	Returns the number of nodes in the forest (note: this is not the number of trees, which changes over time).
	*/
	int node_count() const;

	/**
	Returns the number of trees currently in the forest.
	*/
	int tree_count() const;

	/**
	Unions the sets to which the nodes with indices x and y belong.
	*/
	void union_nodes(int x, int y);

	/**
	Returns the value stored at the node with index x.
	*/
	T& value_of(int x);

	/**
	Returns the value stored at the node with index x.
	*/
	const T& value_of(int x) const;

	//################## PRIVATE METHODS ##################//
private:
	/**
	Checks that x is a valid node index.
	*/
	void check_range(int x) const;

	/**
	Links the two (set-representing) trees rooted at nodes x and y.
	*/
	void link(int x, int y);
};

}

#include "DisjointSetForest.cxx"


#endif


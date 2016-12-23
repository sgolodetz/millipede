/***
 * test-disjointsetforest: main.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include <iostream>
#include <string>

#include <common/adts/DisjointSetForest.h>
using namespace mp;

int main()
{
	DisjointSetForest<std::string> dsf;
	dsf.add_node(0, "a");
	dsf.add_node(2, "c");
	dsf.add_node(1, "b");
	std::cout << dsf.find_set(2) << '\n';
	std::cout << dsf.tree_count() << '\n';
	std::cout << dsf.node_count() << '\n';
	dsf.union_nodes(2, 1);
	std::cout << dsf.tree_count() << '\n';
	std::cout << dsf.node_count() << '\n';
	std::cout << dsf.value_of(1) << '\n';
	std::cout << dsf.value_of(2) << '\n';
	std::cout << dsf.find_set(1) << '\n';
	std::cout << dsf.find_set(2) << '\n';
	return 0;
}

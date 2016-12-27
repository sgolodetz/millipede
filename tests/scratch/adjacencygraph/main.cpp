/***
 * scratchtest_adjacencygraph: main.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include <algorithm>
#include <iostream>

#include <millipede/adts/AdjacencyGraph.h>
using namespace mp;

void output_edges(const std::vector<WeightedEdge<int> >& edges)
{
	std::copy(edges.begin(), edges.end(), std::ostream_iterator<WeightedEdge<int> >(std::cout, " "));
	std::cout << '\n';
}

void output_nodes(const std::vector<int>& nodes)
{
	std::copy(nodes.begin(), nodes.end(), std::ostream_iterator<int>(std::cout, " "));
	std::cout << '\n';
}

int main()
{
	AdjacencyGraph<std::string,int> g;
	g.set_node_properties(0, "Wibble");
	g.set_node_properties(1, "Blah");
	std::cout << g.node_properties(1) << '\n';
	g.set_edge_weight(1, 0, 23);
	output_edges(g.edges());
	g.set_edge_weight(0, 1, 9);
	output_edges(g.edges());
	g.remove_edge(1, 0);
	output_edges(g.edges());
	g.set_node_properties(2, "Boing");
	g.set_edge_weight(2, 0, 84);
	g.set_edge_weight(1, 2, 7);
	g.set_edge_weight(1, 0, 8);
	output_edges(g.edges());
	output_edges(g.adjacent_edges(1));
	output_nodes(g.adjacent_nodes(1));
	g.remove_node(1);
	output_edges(g.edges());
	return 0;
}

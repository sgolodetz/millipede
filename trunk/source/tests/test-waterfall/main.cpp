/***
 * test-waterfall: main.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include <iostream>

#include <common/segmentation/waterfall/NichollsWaterfallPass.h>
using namespace mp;

struct Listener : WaterfallPass<int>::Listener
{
	void merge_nodes(int u, int v)
	{
		std::cout << "Merging nodes " << u << " and " << v << '\n';
	}
};

void basic_test()
{
	// Create the graph in the Marcotegui waterfall paper.
	AdjacencyGraph<int, int> graph;
	for(int i=0; i<14; ++i) graph.set_node_properties(i, i);
	graph.set_edge_weight(0, 1, 3);
		graph.set_edge_weight(1, 2, 2);
		graph.set_edge_weight(1, 3, 20);
			graph.set_edge_weight(3, 4, 4);
				graph.set_edge_weight(4, 5, 2);
			graph.set_edge_weight(3, 6, 10);
				graph.set_edge_weight(6, 7, 5);
					graph.set_edge_weight(7, 8, 5);
						graph.set_edge_weight(8, 9, 4);
						graph.set_edge_weight(8, 10, 2);
						graph.set_edge_weight(8, 11, 20);
							graph.set_edge_weight(11, 12, 4);
							graph.set_edge_weight(11, 13, 4);

	// Create a rooted MST from the graph.
	RootedMST<int> mst(graph);

	// Run a Nicholls waterfall pass on the MST.
	NichollsWaterfallPass<int> pass;
	boost::shared_ptr<Listener> listener(new Listener);
	pass.add_listener(listener);
	pass.run(mst);

	// Output the remaining MST edges.
	std::cout << "\nRemaining edges: ";
	std::copy(mst.edges_cbegin(), mst.edges_cend(), std::ostream_iterator<WeightedEdge<int> >(std::cout, " "));
	std::cout << '\n';
}

int main()
{
	basic_test();
	return 0;
}

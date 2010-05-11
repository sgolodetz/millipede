/***
 * test-rootedmst: main.cpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include <iostream>

#include <common/adts/RootedMST.h>
#include <common/partitionforests/images/ImageLeafLayer.h>
using namespace mp;

void adjacency_graph_mst()
{
	AdjacencyGraph<std::string, int> graph;
	graph.set_node_properties(0, "Wibble");
	RootedMST<int, int> mst(graph);
}

void leaf_layer_mst()
{
	PixelProperties arr[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8 };
	std::vector<PixelProperties> leafProperties(&arr[0], &arr[sizeof(arr)/sizeof(PixelProperties)]);
	std::auto_ptr<ImageLeafLayer> leafLayer(new ImageLeafLayer(3, 3, leafProperties));

	typedef int Label;
	RootedMST<Label,ImageLeafLayer::EdgeWeight> mst(*leafLayer);
}

int main()
{
	adjacency_graph_mst();
	leaf_layer_mst();
	return 0;
}
